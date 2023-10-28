//! Contains the syntax trees related to expressions and their parsing logic.

use flux_base::{
    diagnostic::Handler,
    source_file::{SourceElement, Span},
};
use flux_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation, Token},
    token_stream::Delimiter,
};
use getset::Getters;

use super::{statement::Block, ConnectedList};
use crate::{
    error::{Error, SyntaxKind, UnexpectedSyntax},
    parser::{Parser, Reading},
};

/// Syntax Synopsis:
/// ``` ebnf
/// Declaration:
///     Function
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Declaration {
    Function(Function),
}

impl SourceElement for Declaration {
    fn span(&self) -> Span {
        match self {
            Self::Function(function) => function.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Function:
///     'function' Identifier '(' ParameterList? ')' Block
///     ;
///
/// ParameterList:
///     Identifier (',' Identifier)* ','?  
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Function {
    #[get = "pub"]
    function_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    open_paren: Punctuation,
    #[get = "pub"]
    parameters: Option<ConnectedList<Identifier, Punctuation>>,
    #[get = "pub"]
    close_paren: Punctuation,
    #[get = "pub"]
    block: Block,
}

impl Function {
    /// Dissolves the [`Function`] into its components.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (
        Keyword,
        Identifier,
        Punctuation,
        Option<ConnectedList<Identifier, Punctuation>>,
        Punctuation,
        Block,
    ) {
        (
            self.function_keyword,
            self.identifier,
            self.open_paren,
            self.parameters,
            self.close_paren,
            self.block,
        )
    }
}

impl SourceElement for Function {
    fn span(&self) -> Span { self.function_keyword.span.join(&self.block.span()).unwrap() }
}

impl<'a> Parser<'a> {
    pub fn parse_declaration(&mut self, handler: &dyn Handler<Error>) -> Option<Declaration> {
        match self.stop_at_significant() {
            Reading::Atomic(Token::Keyword(function_keyword))
                if function_keyword.keyword == KeywordKind::Function =>
            {
                // eat the function keyword
                self.forward();

                // parse the identifier
                let identifier = self.parse_identifier(handler)?;
                let delimited_tree = self.parse_enclosed_list(
                    Delimiter::Parenthesis,
                    ',',
                    |parser| parser.parse_identifier(handler),
                    handler,
                )?;

                // parse the block
                let block = self.parse_block(handler)?;

                Some(Declaration::Function(Function {
                    function_keyword,
                    identifier,
                    open_paren: delimited_tree.open,
                    parameters: delimited_tree.list,
                    close_paren: delimited_tree.close,
                    block,
                }))
            }

            unexpected => {
                // make progress
                self.forward();

                handler.receive(Error::UnexpectedSyntax(UnexpectedSyntax {
                    expected: SyntaxKind::Declaration,
                    found: unexpected.into_token(),
                }));

                None
            }
        }
    }
}

#[cfg(test)]
pub(super) mod tests;
