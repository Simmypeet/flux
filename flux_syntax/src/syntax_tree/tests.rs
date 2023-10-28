use std::fmt::{Debug, Display};

use flux_base::{diagnostic::Storage, source_file::SourceFile};
use flux_lexical::{token, token_stream::TokenStream};
use flux_test::input::Input;
use proptest::{
    prelude::Arbitrary,
    prop_assert_eq,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::{error::Error, parser::Parser};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier(String);

impl Arbitrary for Identifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        "[a-zA-Z_][a-zA-Z0-9_]*".prop_map(Self).boxed()
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self.0) }
}

impl Input<&token::Identifier> for &Identifier {
    fn assert(self, output: &token::Identifier) -> TestCaseResult {
        prop_assert_eq!(&self.0, output.span.str());
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Numeric(String);

impl Arbitrary for Numeric {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy { "[0-9]+".prop_map(Self).boxed() }
}

impl Display for Numeric {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self.0) }
}

impl Input<&token::Numeric> for &Numeric {
    fn assert(self, output: &token::Numeric) -> TestCaseResult {
        prop_assert_eq!(&self.0, output.span.str());
        Ok(())
    }
}

pub fn parse<T, F>(source: impl Display, f: F) -> Result<T, TestCaseError>
where
    F: FnOnce(&mut Parser, &Storage<Error>) -> Option<T>,
{
    let source_file = SourceFile::temp(&source)?;

    let storage: Storage<flux_lexical::error::Error> = Storage::new();

    let token_stream = TokenStream::tokenize(&source_file, &storage);

    if !storage.as_vec().is_empty() {
        return Err(TestCaseError::reject(format!(
            "found lexical error(s): {:#?};\nsource: {source}",
            storage.as_vec(),
        )));
    }

    let mut parser = Parser::new(&token_stream);

    let storage: Storage<Error> = Storage::new();
    let output = f(&mut parser, &storage);

    if !storage.as_vec().is_empty() {
        return Err(TestCaseError::fail(format!(
            "found syntax error(s): {:#?};\nsource: {source}",
            storage.as_vec(),
        )));
    }

    output.map_or_else(
        || {
            Err(TestCaseError::fail(format!(
                "failed to parse the source code: {source}",
            )))
        },
        |output| Ok(output),
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ConstantPunctuation<const CHAR: char>;

impl<const CHAR: char> Arbitrary for ConstantPunctuation<CHAR> {
    type Parameters = ();
    type Strategy = Just<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy { Just(Self) }
}

impl<const CHAR: char> Input<&token::Punctuation> for &ConstantPunctuation<CHAR> {
    fn assert(self, output: &token::Punctuation) -> TestCaseResult {
        prop_assert_eq!(CHAR, output.punctuation);
        Ok(())
    }
}

impl<const CHAR: char> Display for ConstantPunctuation<CHAR> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{CHAR}") }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConnectedList<T, U> {
    pub first: T,

    pub rest: Vec<(U, T)>,

    pub trailing_separator: Option<U>,
}

impl<T, U, V, W> Input<&super::ConnectedList<V, W>> for &ConnectedList<T, U>
where
    for<'a, 'b> &'a T: Input<&'b V>,
    for<'a, 'b> &'a U: Input<&'b W>,
{
    fn assert(self, output: &super::ConnectedList<V, W>) -> TestCaseResult {
        self.first.assert(output.first())?;

        prop_assert_eq!(self.rest.len(), output.rest().len());

        for (input, output) in self.rest.iter().zip(output.rest().iter()) {
            input.0.assert(&output.0)?;
            input.1.assert(&output.1)?;
        }

        self.trailing_separator
            .as_ref()
            .assert(output.trailing_separator().as_ref())?;

        Ok(())
    }
}

impl<T: Display, U: Display> Display for ConnectedList<T, U> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.first, f)?;

        for (separator, element) in &self.rest {
            write!(f, "{separator} ")?;
            Display::fmt(element, f)?;
        }

        if let Some(separator) = self.trailing_separator.as_ref() {
            Display::fmt(separator, f)?;
        }

        Ok(())
    }
}

impl<T: Debug, U: Debug> ConnectedList<T, U> {
    pub fn arbitrary_with(
        element_strategy: impl Strategy<Value = T> + Clone,
        punctuation: impl Strategy<Value = U> + Clone,
    ) -> impl Strategy<Value = Self> {
        (
            element_strategy.clone(),
            proptest::collection::vec((punctuation.clone(), element_strategy), 0..=4),
            proptest::option::of(punctuation),
        )
            .prop_map(|(first, rest, trailing_separator)| Self {
                first,
                rest,
                trailing_separator,
            })
    }
}
