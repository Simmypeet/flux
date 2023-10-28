//! Contains all the definition of syntax trees for the Flux language and their parsing logic.

use flux_base::{
    diagnostic::Handler,
    source_file::{SourceElement, Span},
};
use flux_lexical::{
    token::{Punctuation, Token},
    token_stream::Delimiter,
};
use getset::Getters;

use crate::{
    error::Error,
    parser::{Parser, Reading},
};

pub mod declaration;
pub mod expression;
pub mod program;
pub mod statement;

/// Represents a syntax tree node with a pattern of syntax tree nodes separated by a separator.
///
/// This struct is useful for representing syntax tree nodes that are separated by a separator.
/// For example, a comma separated list of expressions such as `1, 2, 3` can be represented by a
/// [`ConnectedList`] with the separator being a comma token and the elements being the expressions.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ConnectedList<Element, Separator> {
    /// The first element of the list.
    #[get = "pub"]
    first: Element,

    /// The rest of the elements of the list.
    ///
    /// Each element of the list is a tuple containing the separator and the element. The separator
    /// is the token/syntax tree node that separates the current element from the prior one.
    #[get = "pub"]
    rest: Vec<(Separator, Element)>,

    /// The trailing separator of the list.
    #[get = "pub"]
    trailing_separator: Option<Separator>,
}

/// Represents a syntax tree node with a pattern of having [`ConnectedList`] delimited by a pair of
/// punctuation like such `(a, b, c)`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DelimitedList<T> {
    /// The open punctuation of the list.
    pub open: Punctuation,

    /// The list of elements of the list.   
    ///
    /// If `None` then the list is empty (or immediately closed after the open punctuation).
    pub list: Option<ConnectedList<T, Punctuation>>,

    /// The close punctuation of the list.
    pub close: Punctuation,
}

impl<'a> Parser<'a> {
    /// Parses a list of elements enclosed by a pair of delimiters, separated by a separator.
    ///
    /// The parser position must be at the delimited list of the given delimiter. It will
    /// consume the whole delimited list and move the next token after the list.
    ///
    /// # Errors
    /// - if the parser position is not at the delimited list of the given delimiter.
    /// - any error returned by the given parser function.
    pub fn parse_enclosed_list<T>(
        &mut self,
        delimiter: Delimiter,
        separator: char,
        mut f: impl FnMut(&mut Self) -> Option<T>,
        handler: &dyn Handler<Error>,
    ) -> Option<DelimitedList<T>> {
        fn skip_to_next_separator(this: &mut Parser, separator: char) -> Option<Punctuation> {
            if let Reading::Atomic(Token::Punctuation(punc)) = this.stop_at(|token| {
                matches!(
                    token, Reading::Atomic(Token::Punctuation(punc))
                    if punc.punctuation == separator
                )
            }) {
                this.forward();
                Some(punc)
            } else {
                None
            }
        }

        let delimited_tree = self.step_into(
            delimiter,
            |parser| {
                let mut first = None;
                let mut rest = Vec::new();
                let mut trailing_separator: Option<Punctuation> = None;

                while !parser.is_exhausted() {
                    let Some(element) = f(parser) else {
                        skip_to_next_separator(parser, separator);
                        continue;
                    };

                    // adds new element
                    match (&first, &trailing_separator) {
                        (None, None) => {
                            first = Some(element);
                        }
                        (Some(_), Some(separator)) => {
                            rest.push((separator.clone(), element));
                            trailing_separator = None;
                        }
                        _ => {
                            unreachable!()
                        }
                    }

                    // expect separator if not exhausted
                    if !parser.is_exhausted() {
                        let Some(separator) = parser.parse_punctuation(separator, true, handler)
                        else {
                            if let Some(punctuation) = skip_to_next_separator(parser, separator) {
                                trailing_separator = Some(punctuation);
                            }

                            continue;
                        };

                        trailing_separator = Some(separator);
                    }
                }

                Some(first.map(|first| ConnectedList {
                    first,
                    rest,
                    trailing_separator,
                }))
            },
            handler,
        )?;

        Some(DelimitedList {
            open: delimited_tree.open,
            list: delimited_tree.tree.unwrap(),
            close: delimited_tree.close,
        })
    }
}

impl<Element: SourceElement, Separator: SourceElement> SourceElement
    for ConnectedList<Element, Separator>
{
    fn span(&self) -> Span {
        let end = self.trailing_separator.as_ref().map_or_else(
            || {
                self.rest
                    .last()
                    .map_or_else(|| self.first.span(), |(_, element)| element.span())
            },
            SourceElement::span,
        );

        self.first.span().join(&end).unwrap()
    }
}

impl<Element, Separator> ConnectedList<Element, Separator> {
    /// Returns an iterator over the elements of the list.
    pub fn elements(&self) -> impl Iterator<Item = &Element> {
        std::iter::once(&self.first).chain(self.rest.iter().map(|(_, element)| element))
    }

    /// Returns an iterator over the elements of the list.
    pub fn into_elements(self) -> impl Iterator<Item = Element> {
        std::iter::once(self.first).chain(self.rest.into_iter().map(|(_, element)| element))
    }

    /// Gets the number of elements in the list.
    pub fn len(&self) -> usize { self.rest.len() + 1 }

    /// Returns `true` if the list is empty.
    ///
    /// The function will never return `false`.
    pub fn is_empty(&self) -> bool { false }
}

#[cfg(test)]
pub(super) mod tests;
