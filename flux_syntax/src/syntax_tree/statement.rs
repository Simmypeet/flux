//! Contains the syntax trees related to statements and their parsing logic.

use enum_as_inner::EnumAsInner;
use flux_base::{
    diagnostic::Handler,
    source_file::{SourceElement, Span},
};
use flux_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation, Token},
    token_stream::Delimiter,
};
use getset::Getters;

use super::expression::{Expression, Parenthesized};
use crate::{
    error::Error,
    parser::{Parser, Reading},
};

/// Syntax Synopsis:
///
/// ``` ebnf
/// Statement:
///     VariableDeclaration
///     | Semi
///     | While
///     | Block
///     | Conditional
///     | Return
///     | Continue
///     | Break
///     | Print
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    Semi(Semi),
    While(While),
    Block(Block),
    Conditional(Conditional),
    Return(Return),
    Continue(Continue),
    Break(Break),
    Print(Print),
}

impl SourceElement for Statement {
    fn span(&self) -> Span {
        match self {
            Self::VariableDeclaration(variable_declaration) => variable_declaration.span(),
            Self::Semi(semi) => semi.span(),
            Self::While(while_) => while_.span(),
            Self::Block(block) => block.span(),
            Self::Conditional(conditional) => conditional.span(),
            Self::Return(r#return) => r#return.span(),
            Self::Continue(r#continue) => r#continue.span(),
            Self::Break(r#break) => r#break.span(),
            Self::Print(print) => print.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Assignment:
///     '=' Expression
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Assignment {
    #[get = "pub"]
    equals: Punctuation,
    #[get = "pub"]
    expression: Expression,
}

impl Assignment {
    /// Dissolves the [`Assignment`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, Expression) { (self.equals, self.expression) }
}

impl SourceElement for Assignment {
    fn span(&self) -> Span { self.equals.span().join(&self.expression.span()).unwrap() }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// VariableDeclaration:
///     'let' Identifier Assignment? ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct VariableDeclaration {
    #[get = "pub"]
    let_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    assignment: Option<Assignment>,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl VariableDeclaration {
    /// Dissolves the [`VariableDeclaration`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Identifier, Option<Assignment>, Punctuation) {
        (
            self.let_keyword,
            self.identifier,
            self.assignment,
            self.semicolon,
        )
    }
}

impl SourceElement for VariableDeclaration {
    fn span(&self) -> Span {
        self.let_keyword
            .span()
            .join(&self.semicolon.span())
            .unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Semi:
///     Expression ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Semi {
    #[get = "pub"]
    pub expression: Expression,
    #[get = "pub"]
    pub semicolon: Punctuation,
}

impl Semi {
    /// Dissolves the [`Semi`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Expression, Punctuation) { (self.expression, self.semicolon) }
}

impl SourceElement for Semi {
    fn span(&self) -> Span { self.expression.span().join(&self.semicolon.span()).unwrap() }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Block:
///     '{' Statement* '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Block {
    #[get = "pub"]
    pub open_brace: Punctuation,
    #[get = "pub"]
    pub statements: Vec<Statement>,
    #[get = "pub"]
    pub close_brace: Punctuation,
}

impl Block {
    /// Dissolves the [`Block`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, Vec<Statement>, Punctuation) {
        (self.open_brace, self.statements, self.close_brace)
    }
}

impl SourceElement for Block {
    fn span(&self) -> Span {
        self.open_brace
            .span()
            .join(&self.close_brace.span())
            .unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// While:
///     'while' Parentized Block
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct While {
    #[get = "pub"]
    while_keyword: Keyword,
    #[get = "pub"]
    parenthesized: Parenthesized,
    #[get = "pub"]
    statement: Box<Statement>,
}

impl While {
    /// Dissolves the [`While`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Parenthesized, Box<Statement>) {
        (self.while_keyword, self.parenthesized, self.statement)
    }
}

impl SourceElement for While {
    fn span(&self) -> Span {
        self.while_keyword
            .span()
            .join(&self.statement.span())
            .unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Else:
///     'else' Statement
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Else {
    #[get = "pub"]
    else_keyword: Keyword,
    #[get = "pub"]
    else_statement: Box<Statement>,
}

impl Else {
    /// Dissolves the [`Else`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Box<Statement>) { (self.else_keyword, self.else_statement) }
}

impl SourceElement for Else {
    fn span(&self) -> Span {
        self.else_keyword
            .span()
            .join(&self.else_statement.span())
            .unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Conditional:
///     'if' Parentized Statement Else?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Conditional {
    #[get = "pub"]
    if_keyword: Keyword,
    #[get = "pub"]
    parenthesized: Parenthesized,
    #[get = "pub"]
    if_statement: Box<Statement>,
    #[get = "pub"]
    r#else: Option<Else>,
}

impl Conditional {
    /// Dissolves the [`Conditional`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Parenthesized, Box<Statement>, Option<Else>) {
        (
            self.if_keyword,
            self.parenthesized,
            self.if_statement,
            self.r#else,
        )
    }
}

impl SourceElement for Conditional {
    fn span(&self) -> Span {
        self.r#else.as_ref().map_or_else(
            || {
                self.if_keyword
                    .span
                    .join(&self.if_statement.span())
                    .unwrap()
            },
            |else_| {
                self.if_keyword
                    .span()
                    .join(&else_.span())
                    .unwrap()
                    .join(&self.if_statement.span())
                    .unwrap()
            },
        )
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Return:
///     'return' Expression? ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Return {
    #[get = "pub"]
    return_keyword: Keyword,
    #[get = "pub"]
    expression: Option<Expression>,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl Return {
    /// Dissolves the [`Return`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Option<Expression>, Punctuation) {
        (self.return_keyword, self.expression, self.semicolon)
    }
}

impl SourceElement for Return {
    fn span(&self) -> Span {
        self.return_keyword
            .span()
            .join(&self.semicolon.span())
            .unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Print:
///     'print' Expression ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Print {
    #[get = "pub"]
    print_keyword: Keyword,
    #[get = "pub"]
    expression: Expression,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl Print {
    /// Dissolves the [`Print`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Expression, Punctuation) {
        (self.print_keyword, self.expression, self.semicolon)
    }
}

impl SourceElement for Print {
    fn span(&self) -> Span {
        self.print_keyword
            .span()
            .join(&self.semicolon.span())
            .unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Continue:
///     'continue' ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Continue {
    #[get = "pub"]
    continue_keyword: Keyword,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for Continue {
    fn span(&self) -> Span {
        self.continue_keyword
            .span()
            .join(&self.semicolon.span())
            .unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Break:
///     'break' ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Break {
    #[get = "pub"]
    break_keyword: Keyword,
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for Break {
    fn span(&self) -> Span {
        self.break_keyword
            .span()
            .join(&self.semicolon.span())
            .unwrap()
    }
}

impl<'a> Parser<'a> {
    /// Parses a [`Block`].
    pub fn parse_block(&mut self, handler: &dyn Handler<Error>) -> Option<Block> {
        let token_tree = self.step_into(
            Delimiter::Brace,
            |parser| {
                let mut statements = Vec::new();

                while !parser.is_exhausted() {
                    parser.parse_statement(handler).map_or_else(
                        || {
                            // error recovery
                            parser.stop_at(|reading| matches!(
                                reading,
                                Reading::Atomic(Token::Punctuation(punc)) if punc.punctuation == ';'
                            ) || matches!(
                                reading,
                                Reading::IntoDelimited(punc) if punc.punctuation == '{'
                            ));

                            // goes after the semicolon or the open brace
                            parser.forward();
                        },
                        |statement| statements.push(statement),
                    );
                }

                Some(statements)
            },
            handler,
        )?;

        Some(Block {
            open_brace: token_tree.open,
            statements: token_tree.tree?,
            close_brace: token_tree.close,
        })
    }

    /// Parses a [`Statement`].
    #[allow(clippy::too_many_lines)]
    pub fn parse_statement(&mut self, handler: &dyn Handler<Error>) -> Option<Statement> {
        match self.stop_at_significant() {
            // variable declaration
            Reading::Atomic(Token::Keyword(let_keyword))
                if let_keyword.keyword == KeywordKind::Let =>
            {
                // eat the `let` keyword
                self.forward();

                let identifier = self.parse_identifier(handler)?;

                let assignment = match self.stop_at_significant() {
                    Reading::Atomic(Token::Punctuation(equals)) if equals.punctuation == '=' => {
                        // eat '='
                        self.forward();

                        let expression = self.parse_expression(handler)?;

                        Some(Assignment { equals, expression })
                    }
                    _ => None,
                };

                let semicolon = self.parse_punctuation(';', true, handler)?;

                Some(Statement::VariableDeclaration(VariableDeclaration {
                    let_keyword,
                    identifier,
                    assignment,
                    semicolon,
                }))
            }

            // while statement
            Reading::Atomic(Token::Keyword(while_keyword))
                if while_keyword.keyword == KeywordKind::While =>
            {
                // eat the `while` keyword
                self.forward();

                let parenthesized = self.parse_parenthesized(handler)?;
                let statement = self.parse_statement(handler)?;

                Some(Statement::While(While {
                    while_keyword,
                    parenthesized,
                    statement: Box::new(statement),
                }))
            }

            // block statement
            Reading::IntoDelimited(open_brace) if open_brace.punctuation == '{' => {
                let block = self.parse_block(handler)?;

                Some(Statement::Block(block))
            }

            // return statement
            Reading::Atomic(Token::Keyword(return_keyword))
                if return_keyword.keyword == KeywordKind::Return =>
            {
                // eat the `return` keyword
                self.forward();

                match self.stop_at_significant() {
                    Reading::Atomic(Token::Punctuation(semicolon))
                        if semicolon.punctuation == ';' =>
                    {
                        // eat the `;` punctuation
                        self.forward();

                        Some(Statement::Return(Return {
                            return_keyword,
                            expression: None,
                            semicolon,
                        }))
                    }

                    _ => {
                        let expression = self.parse_expression(handler)?;
                        let semicolon = self.parse_punctuation(';', true, handler)?;

                        Some(Statement::Return(Return {
                            return_keyword,
                            expression: Some(expression),
                            semicolon,
                        }))
                    }
                }
            }

            // continue statement
            Reading::Atomic(Token::Keyword(continue_keyword))
                if continue_keyword.keyword == KeywordKind::Continue =>
            {
                // eat the `continue` keyword
                self.forward();

                let semicolon = self.parse_punctuation(';', true, handler)?;

                Some(Statement::Continue(Continue {
                    continue_keyword,
                    semicolon,
                }))
            }

            // break statement
            Reading::Atomic(Token::Keyword(break_keyword))
                if break_keyword.keyword == KeywordKind::Break =>
            {
                // eat the `break` keyword
                self.forward();

                let semicolon = self.parse_punctuation(';', true, handler)?;

                Some(Statement::Break(Break {
                    break_keyword,
                    semicolon,
                }))
            }

            // print statement
            Reading::Atomic(Token::Keyword(print_keyword))
                if print_keyword.keyword == KeywordKind::Print =>
            {
                // eat the `print` keyword
                self.forward();

                let expression = self.parse_expression(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                Some(Statement::Print(Print {
                    print_keyword,
                    expression,
                    semicolon,
                }))
            }

            // conditional statement
            Reading::Atomic(Token::Keyword(if_keyword))
                if if_keyword.keyword == KeywordKind::If =>
            {
                // eat the `if` keyword
                self.forward();

                let parenthesized = self.parse_parenthesized(handler)?;
                let if_statement = Box::new(self.parse_statement(handler)?);

                match self.stop_at_significant() {
                    Reading::Atomic(Token::Keyword(else_keyword))
                        if else_keyword.keyword == KeywordKind::Else =>
                    {
                        // eat the `else` keyword
                        self.forward();

                        let else_statement = Box::new(self.parse_statement(handler)?);

                        Some(Statement::Conditional(Conditional {
                            if_keyword,
                            parenthesized,
                            if_statement,
                            r#else: Some(Else {
                                else_keyword,
                                else_statement,
                            }),
                        }))
                    }

                    _ => Some(Statement::Conditional(Conditional {
                        if_keyword,
                        parenthesized,
                        if_statement,
                        r#else: None,
                    })),
                }
            }

            // semi
            _ => {
                let expression = self.parse_expression(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                Some(Statement::Semi(Semi {
                    expression,
                    semicolon,
                }))
            }
        }
    }
}

#[cfg(test)]
pub(super) mod tests;
