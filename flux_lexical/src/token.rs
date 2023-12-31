//! Is a module containing the [`Token`] type and all of its related types.

use std::{collections::HashMap, hash::Hash, iter::Iterator, str::FromStr};

use derive_more::From;
use enum_as_inner::EnumAsInner;
use flux_base::{
    diagnostic::Handler,
    source_file::{self, ByteIndex, SourceElement, Span},
};
use lazy_static::lazy_static;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use thiserror::Error;

use crate::error::{self, UnterminatedDelimitedComment};

/// Is an enumeration representing keywords in the Flux programming language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumIter)]
#[allow(missing_docs)]
pub enum KeywordKind {
    Function,
    And,
    Or,
    If,
    Else,
    Let,
    While,
    True,
    False,
    Return,
    Null,
    Continue,
    Break,
    Print,
    Int64,
    Float64,
    Bool,
}

impl ToString for KeywordKind {
    fn to_string(&self) -> String { self.as_str().to_string() }
}

/// Is an error that is returned when a string cannot be parsed into a [`Keyword`] in [`FromStr`]
/// trait implementation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Error)]
#[error("invalid string representation of keyword.")]
pub struct KeywordParseError;

impl FromStr for KeywordKind {
    type Err = KeywordParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref STRING_KEYWORD_MAP: HashMap<&'static str, KeywordKind> = {
                let mut map = HashMap::new();

                for keyword in KeywordKind::iter() {
                    map.insert(keyword.as_str(), keyword);
                }

                map
            };
        }
        STRING_KEYWORD_MAP.get(s).copied().ok_or(KeywordParseError)
    }
}

impl KeywordKind {
    /// Gets the string representation of the keyword as a `&str`.
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Function => "function",
            Self::Let => "let",
            Self::If => "if",
            Self::Else => "else",
            Self::While => "while",
            Self::True => "true",
            Self::False => "false",
            Self::And => "and",
            Self::Or => "or",
            Self::Return => "return",
            Self::Null => "null",
            Self::Continue => "continue",
            Self::Break => "break",
            Self::Print => "print",
            Self::Bool => "bool",
            Self::Int64 => "int64",
            Self::Float64 => "float64",
        }
    }
}

/// Is an enumeration containing all kinds of tokens in the Flux programming language.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Token {
    WhiteSpaces(WhiteSpaces),
    Identifier(Identifier),
    Keyword(Keyword),
    Punctuation(Punctuation),
    Numeric(Numeric),
    Comment(Comment),
}

impl Token {
    /// Returns the span of the token.
    #[must_use]
    pub fn span(&self) -> &Span {
        match self {
            Self::WhiteSpaces(token) => &token.span,
            Self::Identifier(token) => &token.span,
            Self::Keyword(token) => &token.span,
            Self::Punctuation(token) => &token.span,
            Self::Numeric(token) => &token.span,
            Self::Comment(token) => &token.span,
        }
    }
}

impl SourceElement for Token {
    fn span(&self) -> Span {
        match self {
            Self::WhiteSpaces(token) => token.span(),
            Self::Identifier(token) => token.span(),
            Self::Keyword(token) => token.span(),
            Self::Punctuation(token) => token.span(),
            Self::Numeric(token) => token.span(),
            Self::Comment(token) => token.span(),
        }
    }
}

/// Represents a contiguous sequence of whitespace characters.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WhiteSpaces {
    /// Is the span that makes up the token.
    pub span: Span,
}

impl SourceElement for WhiteSpaces {
    fn span(&self) -> Span { self.span.clone() }
}

/// Represents a contiguous sequence of characters that are valid in an identifier.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier {
    /// Is the span that makes up the token.
    pub span: Span,
}

impl SourceElement for Identifier {
    fn span(&self) -> Span { self.span.clone() }
}

/// Represents a contiguous sequence of characters that are reserved for a keyword.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Keyword {
    /// Is the span that makes up the token.
    pub span: Span,

    /// Is the [`KeywordKind`] that the token represents.
    pub keyword: KeywordKind,
}

impl SourceElement for Keyword {
    fn span(&self) -> Span { self.span.clone() }
}

/// Represents a single ASCII punctuation character.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Punctuation {
    /// Is the span that makes up the token.
    pub span: Span,

    /// Is the ASCII punctuation character that the token represents.
    pub punctuation: char,
}

impl SourceElement for Punctuation {
    fn span(&self) -> Span { self.span.clone() }
}

/// Represents a hardcoded numeric literal value in the source code.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Numeric {
    /// Is the span that makes up the token.
    pub span: Span,
}

impl SourceElement for Numeric {
    fn span(&self) -> Span { self.span.clone() }
}

/// Is an enumeration representing the two kinds of comments in the Flux programming language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CommentKind {
    /// A comment that starts with `//` and ends at the end of the line.
    Line,

    /// A comment that starts with `/*` and ends with `*/`.
    Delimited,
}

/// Represents a portion of the source code that is ignored by the interpreter.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Comment {
    /// Is the span that makes up the token.
    pub span: Span,

    /// Is the kind of comment that the token represents.
    pub kind: CommentKind,
}

impl SourceElement for Comment {
    fn span(&self) -> Span { self.span.clone() }
}

/// Is an error that can occur when invoking the [`Token::lex`] method.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, thiserror::Error, From,
)]
#[allow(missing_docs)]
pub enum Error {
    #[error("encountered a fatal lexical error that causes the process to stop.")]
    FatalLexicalError,

    #[error("the iterator argument is at the end of the source code.")]
    EndOfSourceCodeIteratorArgument,
}

impl Token {
    /// Increments the iterator until the predicate returns false.
    fn walk_iter(iter: &mut source_file::Iterator, predicate: impl Fn(char) -> bool) {
        while let Some((_, character)) = iter.peek() {
            if !predicate(character) {
                break;
            }

            iter.next();
        }
    }

    /// Creates a span from the given start location to the current location of the iterator.
    fn create_span(start: ByteIndex, iter: &mut source_file::Iterator) -> Span {
        iter.peek().map_or_else(
            || Span::to_end(iter.source_file().clone(), start).unwrap(),
            |(index, _)| Span::new(iter.source_file().clone(), start, index).unwrap(),
        )
    }

    /// Checks if the given character is a valid first character of an identifier.
    fn is_first_identifier_character(character: char) -> bool {
        character == '_'
            || character == '@'
            || (!character.is_control()
                && !character.is_whitespace()
                && !character.is_ascii_punctuation()
                && !character.is_ascii_digit())
    }

    /// Checks if the given character is a valid character of an identifier.
    fn is_identifier_character(character: char) -> bool {
        character == '_'
            || (!character.is_control()
                && !character.is_whitespace()
                && !character.is_ascii_punctuation())
    }

    fn handle_whitespace(iter: &mut source_file::Iterator, start: ByteIndex) -> Self {
        Self::walk_iter(iter, char::is_whitespace);

        WhiteSpaces {
            span: Self::create_span(start, iter),
        }
        .into()
    }

    fn handle_identifier_and_keyword(iter: &mut source_file::Iterator, start: ByteIndex) -> Self {
        Self::walk_iter(iter, Self::is_identifier_character);

        let span = Self::create_span(start, iter);
        let word = span.str();

        // Checks if the word is a keyword
        KeywordKind::from_str(word).ok().map_or_else(
            || Identifier { span: span.clone() }.into(),
            |kw| {
                Keyword {
                    span: span.clone(),
                    keyword: kw,
                }
                .into()
            },
        )
    }

    fn handle_comment(
        iter: &mut source_file::Iterator,
        start: ByteIndex,
        character: char,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self, Error> {
        // Single line comment
        if let Some((_, '/')) = iter.peek() {
            iter.next();

            Self::walk_iter(iter, |character| !(character == '\n' || character == '\r'));

            let is_cr = iter
                .peek()
                .map_or(false, |(_, character)| character == '\r');

            if let (true, Some((_, '\n'))) = (is_cr, iter.next()) {
                // skips the crlf
                iter.next();
            }

            Ok(Comment {
                span: Self::create_span(start, iter),
                kind: CommentKind::Line,
            }
            .into())
        }
        // Delimited comment
        else if let Some((_, '*')) = iter.peek() {
            iter.next();

            let mut is_terminated = false;

            while let Some((_, character)) = iter.next() {
                if character == '*' {
                    if let Some((_, '/')) = iter.peek() {
                        iter.next();

                        is_terminated = true;

                        break;
                    }
                }
            }

            // Checks if the comment is terminated
            if is_terminated {
                Ok(Comment {
                    span: Self::create_span(start, iter),
                    kind: CommentKind::Delimited,
                }
                .into())
            } else {
                handler.receive(
                    UnterminatedDelimitedComment {
                        span: Span::new(iter.source_file().clone(), start, start + 2).unwrap(),
                    }
                    .into(),
                );
                return Err(Error::FatalLexicalError);
            }
        }
        // Just a single slash punctuation
        else {
            Ok(Punctuation {
                span: Self::create_span(start, iter),
                punctuation: character,
            }
            .into())
        }
    }

    fn handle_numeric_literal(iter: &mut source_file::Iterator, start: ByteIndex) -> Self {
        // Tokenizes the whole number part
        Self::walk_iter(iter, |character| character.is_ascii_digit());

        Numeric {
            span: Self::create_span(start, iter),
        }
        .into()
    }

    /// Lexes the source code from the given iterator.
    ///
    /// The tokenization starts at the current location of the iterator. The function moves the
    /// iterator at least once and forwards it until it makes a token. After the token is made, the
    /// iterator is left at the next character that is not part of the token.
    ///
    /// # Errors
    /// - [`Error::EndOfSourceCodeIteratorArgument`] - The iterator argument is at the end of the
    ///   source code.
    /// - [`Error::FatalLexicalError`] - A fatal lexical error occurred.
    pub fn lex(
        iter: &mut source_file::Iterator,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self, Error> {
        // Gets the first character
        let (start, character) = iter.next().ok_or(Error::EndOfSourceCodeIteratorArgument)?;

        // Found white spaces
        if character.is_whitespace() {
            Ok(Self::handle_whitespace(iter, start))
        }
        // Found identifier/keyword
        else if Self::is_first_identifier_character(character) {
            Ok(Self::handle_identifier_and_keyword(iter, start))
        }
        // Found comment/single slash punctuation
        else if character == '/' {
            Self::handle_comment(iter, start, character, handler)
        }
        // Found numeric literal
        else if character.is_ascii_digit() {
            Ok(Self::handle_numeric_literal(iter, start))
        }
        // Found a punctuation
        else if character.is_ascii_punctuation() {
            Ok(Punctuation {
                span: Self::create_span(start, iter),
                punctuation: character,
            }
            .into())
        } else {
            unreachable!("should've been handled by earlier cases")
        }
    }
}

#[cfg(test)]
pub(crate) mod tests;
