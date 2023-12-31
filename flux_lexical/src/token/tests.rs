use std::{
    fmt::{Display, Write},
    str::FromStr,
};

use derive_more::{Deref, DerefMut};
use flux_base::{diagnostic::Storage, source_file::SourceFile};
use flux_test::input::Input;
use lazy_static::lazy_static;
use proptest::{
    prelude::Arbitrary,
    prop_assert, prop_assert_eq, prop_oneof, proptest,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};
use strum::IntoEnumIterator;

use super::KeywordKind;

/// Represents an input for the [`super::Identifier`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier {
    /// The valid identifier string.
    pub string: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.write_str(&self.string) }
}

impl Arbitrary for Identifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        "[A-Za-z_@][A-Za-z0-9_]*"
            .prop_filter_map(
                "filter out identifiers that can be used as a keyword",
                |x| {
                    if KeywordKind::from_str(x.as_ref()).is_ok() {
                        None
                    } else {
                        Some(Self { string: x })
                    }
                },
            )
            .boxed()
    }
}

impl Input<&super::Identifier> for &Identifier {
    fn assert(self, output: &super::Identifier) -> TestCaseResult {
        prop_assert_eq!(self.string.as_str(), output.span.str());
        Ok(())
    }
}

/// Represents a valid keyword input for the [`super::Keyword`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Keyword {
    /// The kind of keyword.
    pub keyword: KeywordKind,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.keyword.as_str())
    }
}

impl Arbitrary for Keyword {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        lazy_static! {
            static ref KEYWORDS: Vec<KeywordKind> = KeywordKind::iter().collect();
        }

        proptest::sample::select(KEYWORDS.as_slice())
            .prop_map(|kind| Self { keyword: kind })
            .boxed()
    }
}

impl Input<&super::Keyword> for &Keyword {
    fn assert(self, output: &super::Keyword) -> TestCaseResult {
        prop_assert_eq!(self.keyword, output.keyword);
        Ok(())
    }
}

/// Represents an input for the [`super::Numeric`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Numeric {
    /// The valid numeric literal value string.
    pub value: String,
}

impl Display for Numeric {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.value)?;
        Ok(())
    }
}

impl Arbitrary for Numeric {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (proptest::num::u64::ANY.prop_map(|x| x.to_string()))
            .prop_map(|value| Self { value })
            .boxed()
    }
}

impl Input<&super::Numeric> for &Numeric {
    fn assert(self, output: &super::Numeric) -> TestCaseResult {
        prop_assert_eq!(self.value.as_str(), output.span.str());
        Ok(())
    }
}

/// Represents an input for the delimited [`super::Comment`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deref, DerefMut)]
pub struct DelimitedComment {
    /// The content of the delimited comment (without the `/*` and `*/`).
    pub comment_body: String,
}

impl Arbitrary for DelimitedComment {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        "[^\r]*"
            .prop_filter_map("must not contain */", |x| {
                if x.contains("*/") {
                    None
                } else {
                    Some(Self { comment_body: x })
                }
            })
            .boxed()
    }
}

impl Display for DelimitedComment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("/*")?;
        f.write_str(&self.comment_body)?;
        f.write_str("*/")
    }
}

/// Represents an input for the line [`super::Comment`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LineComment {
    /// The content of the line comment (without the `//` and new line terminator).
    pub comment_body: String,
}

impl Arbitrary for LineComment {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        "[^\\n\\r]*"
            .prop_map(|body| Self { comment_body: body })
            .boxed()
    }
}

impl std::fmt::Display for LineComment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("//")?;
        f.write_str(&self.comment_body)?;
        f.write_char('\n')
    }
}

/// Represents an input for the [`super::Comment`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Comment {
    Line(LineComment),
    Delimited(DelimitedComment),
}

impl Arbitrary for Comment {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            LineComment::arbitrary().prop_map(Comment::Line),
            DelimitedComment::arbitrary().prop_map(Comment::Delimited)
        ]
        .boxed()
    }
}

impl Display for Comment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Line(x) => x.fmt(f),
            Self::Delimited(x) => x.fmt(f),
        }
    }
}

impl Comment {
    /// Verifies that the given [`super::Comment`] complies with this input.
    #[allow(clippy::missing_errors_doc)]
    pub fn assert(&self, output: &super::Comment) -> TestCaseResult {
        match self {
            Self::Line(i) => {
                prop_assert_eq!(output.kind, super::CommentKind::Line);
                prop_assert_eq!(
                    &output.span.str()[2..output.span.str().len() - 1],
                    i.comment_body.as_str()
                );
            }
            Self::Delimited(i) => {
                prop_assert_eq!(output.kind, super::CommentKind::Delimited);
                prop_assert_eq!(
                    &output.span.str()[2..output.span.str().len() - 2],
                    i.comment_body.as_str()
                );
            }
        };

        Ok(())
    }
}

/// Represents an input for the [`super::Punctuation`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Punctuation {
    /// The valid punctuation character.
    pub punctuation: char,
}

impl Arbitrary for Punctuation {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        proptest::char::any()
            .prop_filter_map("allows only ascii puncutation", |x| {
                if x.is_ascii_punctuation() && x != '_' && x != '@' {
                    Some(Self { punctuation: x })
                } else {
                    None
                }
            })
            .boxed()
    }
}

impl Display for Punctuation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(self.punctuation)
    }
}

impl Input<&super::Punctuation> for &Punctuation {
    fn assert(self, output: &super::Punctuation) -> TestCaseResult {
        prop_assert_eq!(output.punctuation, self.punctuation);
        Ok(())
    }
}

/// Represents an input for the [`super::WhiteSpaces`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum WhiteSpaces {
    Spaces(u8),
    Tabs(u8),
    NewLines(u8),
}

impl Arbitrary for WhiteSpaces {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (1u8..4)
            .prop_flat_map(|x| {
                prop_oneof![
                    Just(Self::Spaces(x)),
                    Just(Self::Tabs(x)),
                    Just(Self::NewLines(x))
                ]
            })
            .boxed()
    }
}

impl Display for WhiteSpaces {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Spaces(x) => {
                for _ in 0..*x {
                    f.write_char(' ')?;
                }
            }
            Self::Tabs(x) => {
                for _ in 0..*x {
                    f.write_char('\t')?;
                }
            }
            Self::NewLines(x) => {
                for _ in 0..*x {
                    f.write_char('\n')?;
                }
            }
        }
        Ok(())
    }
}

impl WhiteSpaces {
    /// Verifies that the given [`super::WhiteSpaces`] complies with this input.
    #[allow(clippy::missing_errors_doc)]
    pub fn assert(self, output: &super::WhiteSpaces) -> TestCaseResult {
        match self {
            Self::Spaces(i) => {
                prop_assert_eq!(output.span.str().len(), i as usize);
                prop_assert!(output.span.str().chars().all(|x| x == ' '));
            }
            Self::Tabs(i) => {
                prop_assert_eq!(output.span.str().len(), i as usize);
                prop_assert!(output.span.str().chars().all(|x| x == '\t'));
            }
            Self::NewLines(i) => {
                prop_assert_eq!(output.span.str().len(), i as usize);
                prop_assert!(output.span.str().chars().all(|x| x == '\n'));
            }
        }

        Ok(())
    }
}

/// Represents an input for the [`super::Identifier`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Token {
    Identifier(Identifier),
    Comment(Comment),
    Keyword(Keyword),
    NumericLiteral(Numeric),
    WhiteSpaces(WhiteSpaces),
    Punctuation(Punctuation),
}

impl Arbitrary for Token {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Identifier::arbitrary().prop_map(Self::Identifier),
            Comment::arbitrary().prop_map(Self::Comment),
            Keyword::arbitrary().prop_map(Self::Keyword),
            Numeric::arbitrary().prop_map(Self::NumericLiteral),
            WhiteSpaces::arbitrary().prop_map(Self::WhiteSpaces),
            Punctuation::arbitrary().prop_map(Self::Punctuation)
        ]
        .boxed()
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(x) => Display::fmt(x, f),
            Self::Comment(x) => Display::fmt(x, f),
            Self::Keyword(x) => Display::fmt(x, f),
            Self::NumericLiteral(x) => Display::fmt(x, f),
            Self::WhiteSpaces(x) => Display::fmt(x, f),
            Self::Punctuation(x) => Display::fmt(x, f),
        }
    }
}

impl Input<&super::Token> for &Token {
    /// Verifies that the given [`super::Token`] complies with this input.
    #[allow(clippy::missing_errors_doc)]
    fn assert(self, output: &super::Token) -> TestCaseResult {
        match (self, output) {
            (Token::Identifier(i), super::Token::Identifier(o)) => {
                i.assert(o)?;
            }
            (Token::Keyword(i), super::Token::Keyword(o)) => {
                i.assert(o)?;
            }
            (Token::NumericLiteral(i), super::Token::Numeric(o)) => {
                i.assert(o)?;
            }
            (Token::Comment(i), super::Token::Comment(o)) => {
                i.assert(o)?;
            }
            (Token::WhiteSpaces(i), super::Token::WhiteSpaces(o)) => {
                i.assert(o)?;
            }
            (Token::Punctuation(i), super::Token::Punctuation(o)) => {
                i.assert(o)?;
            }
            _ => {
                return Err(TestCaseError::fail(format!(
                    "expected {self:?} got {output:?}",
                )))
            }
        }

        Ok(())
    }
}

fn tokenize(source: String) -> Result<super::Token, proptest::test_runner::TestCaseError> {
    let source_file = SourceFile::temp(source)?;
    let mut iterator = source_file.iter();

    let error_storage: Storage<super::error::Error> = Storage::new();
    let token = super::Token::lex(&mut iterator, &error_storage)?;

    // no errors
    prop_assert!(error_storage.as_vec().is_empty());

    Ok(token)
}

proptest! {
    #[test]
    fn token_test(
        input in Token::arbitrary()
    ) {
        let source = input.to_string();
        let token = tokenize(source)?;

        input.assert(&token)?;
    }
}
