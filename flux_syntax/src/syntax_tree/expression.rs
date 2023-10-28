//! Contains the syntax trees related to expressions and their parsing logic.

use std::cmp::Ordering;

use enum_as_inner::EnumAsInner;
use flux_base::{
    diagnostic::{Dummy, Handler},
    source_file::{SourceElement, Span},
};
use flux_lexical::{
    token::{self, Identifier, Keyword, KeywordKind, Punctuation, Token},
    token_stream::Delimiter,
};
use getset::Getters;

use super::ConnectedList;
use crate::{
    error::{Error, UnexpectedSyntax},
    parser::{Parser, Reading},
};

/// Syntax Synopsis:
///
/// ``` ebnf
/// BinaryOperator:
///     '+'
///     | '-'
///     | '*'
///     | '/'
///     | '%'
///     | '='
///     | '+='
///     | '-='
///     | '*='
///     | '/='
///     | '%='
///     | '=='
///     | '!='
///     | '<'
///     | '<='
///     | '>'
///     | '>='
///     | ':='
///     | 'and'
///     | 'or'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum BinaryOperator {
    Add(Punctuation),
    Subtract(Punctuation),
    Multiply(Punctuation),
    Divide(Punctuation),
    Modulo(Punctuation),
    Assign(Punctuation),
    CompoundAdd(Punctuation, Punctuation),
    CompoundSubtract(Punctuation, Punctuation),
    CompoundMultiply(Punctuation, Punctuation),
    CompoundDivide(Punctuation, Punctuation),
    CompoundModulo(Punctuation, Punctuation),
    Equal(Punctuation, Punctuation),
    NotEqual(Punctuation, Punctuation),
    LessThan(Punctuation),
    LessThanOrEqual(Punctuation, Punctuation),
    GreaterThan(Punctuation),
    GreaterThanOrEqual(Punctuation, Punctuation),
    LogicalAnd(Keyword),
    LogicalOr(Keyword),
}

impl BinaryOperator {
    /// Returns `true` if the operator is assignment (including compound assignment)
    #[must_use]
    pub fn is_assignment(&self) -> bool {
        matches!(
            self,
            Self::Assign(..)
                | Self::CompoundAdd(..)
                | Self::CompoundSubtract(..)
                | Self::CompoundMultiply(..)
                | Self::CompoundDivide(..)
                | Self::CompoundModulo(..)
        )
    }

    /// Gets the precedence of the operator (the higher the number, the first it will be evaluated)
    ///
    /// The least operator has precedence 1.
    #[must_use]
    pub fn get_precedence(&self) -> u32 {
        match self {
            Self::Assign(..)
            | Self::CompoundAdd(..)
            | Self::CompoundSubtract(..)
            | Self::CompoundMultiply(..)
            | Self::CompoundDivide(..)
            | Self::CompoundModulo(..) => 1,
            Self::LogicalOr(..) => 2,
            Self::LogicalAnd(..) => 3,
            Self::Equal(..) | Self::NotEqual(..) => 4,
            Self::LessThan(..)
            | Self::LessThanOrEqual(..)
            | Self::GreaterThan(..)
            | Self::GreaterThanOrEqual(..) => 5,
            Self::Add(..) | Self::Subtract(..) => 6,
            Self::Multiply(..) | Self::Divide(..) | Self::Modulo(..) => 7,
        }
    }
}

impl SourceElement for BinaryOperator {
    fn span(&self) -> Span {
        match self {
            Self::Add(token)
            | Self::Subtract(token)
            | Self::Multiply(token)
            | Self::Divide(token)
            | Self::Modulo(token)
            | Self::Assign(token)
            | Self::LessThan(token)
            | Self::GreaterThan(token) => token.span.clone(),
            Self::CompoundAdd(token, token1)
            | Self::CompoundSubtract(token, token1)
            | Self::CompoundMultiply(token, token1)
            | Self::CompoundDivide(token, token1)
            | Self::CompoundModulo(token, token1)
            | Self::Equal(token, token1)
            | Self::NotEqual(token, token1)
            | Self::LessThanOrEqual(token, token1)
            | Self::GreaterThanOrEqual(token, token1) => token.span().join(&token1.span).unwrap(),
            Self::LogicalAnd(token) | Self::LogicalOr(token) => token.span.clone(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Binary:
///     Expression BinaryOperator Expression
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Binary {
    #[get = "pub"]
    left_operand: Box<Expression>,
    #[get = "pub"]
    operator: BinaryOperator,
    #[get = "pub"]
    right_operand: Box<Expression>,
}

impl SourceElement for Binary {
    fn span(&self) -> Span {
        self.left_operand
            .span()
            .join(&self.right_operand.span())
            .unwrap()
    }
}

/// Syntax Synopsis:
///
/// ```ebnf
/// Expression:
///     Primary
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Expression {
    Primary(Primary),
    Binary(Binary),
}

impl SourceElement for Expression {
    fn span(&self) -> Span {
        match self {
            Self::Primary(primary) => primary.span(),
            Self::Binary(binary) => binary.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Expression:
///     Identifier
///     | 'null'
///     | Numeric
///     | Prefix
///     | Parenthesized
///     | FunctionCall
///     | 'true'
///     | 'false'
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Primary {
    Identifier(Identifier),
    Null(Keyword),
    Numeric(Numeric),
    Prefix(Prefix),
    Parenthesized(Parenthesized),
    FunctionCall(FunctionCall),
    True(Keyword),
    False(Keyword),
}

impl SourceElement for Primary {
    fn span(&self) -> Span {
        match self {
            Self::Identifier(identifier) => identifier.span(),
            Self::Null(keyword) | Self::True(keyword) | Self::False(keyword) => keyword.span(),
            Self::Prefix(prefix) => prefix.span(),
            Self::Parenthesized(parenthesized) => parenthesized.span(),
            Self::FunctionCall(function_call) => function_call.span(),
            Self::Numeric(numeric) => numeric.span(),
        }
    }
}

/// Syntax Synopsis:    
///
/// ``` ebnf
/// Parenthesized:
///     '(' Expression ')'
///     ;
/// ````
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Parenthesized {
    #[get = "pub"]
    open: Punctuation,
    #[get = "pub"]
    expression: Box<Expression>,
    #[get = "pub"]
    close: Punctuation,
}

impl Parenthesized {
    /// Dissolves the parenthesized expression into its components
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, Expression, Punctuation) {
        (self.open, *self.expression, self.close)
    }
}

impl SourceElement for Parenthesized {
    fn span(&self) -> Span { self.open.span().join(&self.close.span).unwrap() }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// PrefixOperator:
///     '!' | '-' | TypeCast
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum PrefixOperator {
    LogicalNot(Punctuation),
    Negate(Punctuation),
    TypeCast(TypeCast),
}

impl SourceElement for PrefixOperator {
    fn span(&self) -> Span {
        match self {
            Self::LogicalNot(token) | Self::Negate(token) => token.span.clone(),
            Self::TypeCast(type_cast) => type_cast.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Type:
///     'float64'
///     | 'int64'
///     | 'bool'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Float64(Keyword),
    Int64(Keyword),
    Bool(Keyword),
}

impl SourceElement for Type {
    fn span(&self) -> Span {
        match self {
            Self::Float64(keyword) | Self::Int64(keyword) | Self::Bool(keyword) => {
                keyword.span.clone()
            }
        }
    }
}

/// Syntax Synopsis:
///
/// ```ebnf
/// TypeCast:
///     '[' Type ']'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TypeCast {
    #[get = "pub"]
    left_bracket: Punctuation,
    #[get = "pub"]
    r#type: Type,
    #[get = "pub"]
    right_bracket: Punctuation,
}

impl SourceElement for TypeCast {
    fn span(&self) -> Span {
        self.left_bracket
            .span()
            .join(&self.right_bracket.span)
            .unwrap()
    }
}

/// Syntax Synopsis:
///
/// ```ebnf
/// Decimal:
///     '.' NumericToken
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Decimal {
    #[get = "pub"]
    dot: Punctuation,
    #[get = "pub"]
    numeric: token::Numeric,
}

impl SourceElement for Decimal {
    fn span(&self) -> Span { self.dot.span().join(&self.numeric.span).unwrap() }
}

/// Syntax Synopsis:
///
/// ```ebnf
/// Numeric:
///     NumericToken Decimal?
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Numeric {
    #[get = "pub"]
    integer: token::Numeric,
    #[get = "pub"]
    decimal: Option<Decimal>,
}

impl SourceElement for Numeric {
    fn span(&self) -> Span {
        self.decimal.as_ref().map_or_else(
            || self.integer.span.clone(),
            |decimal| self.integer.span().join(&decimal.span()).unwrap(),
        )
    }
}

/// Syntax Synopsis:
///
/// ```ebnf
/// Prefix:
///     PrefixOperator Primary
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Prefix {
    #[get = "pub"]
    operator: PrefixOperator,
    #[get = "pub"]
    operand: Box<Primary>,
}

impl SourceElement for Prefix {
    fn span(&self) -> Span { self.operator.span().join(&self.operand.span()).unwrap() }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// FunctionCall:
///     Identifier '(' (Expression (',' Expression)*)? ')'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct FunctionCall {
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    left_parenthesis: Punctuation,
    #[get = "pub"]
    arguments: Option<ConnectedList<Box<Expression>, Punctuation>>,
    #[get = "pub"]
    right_parenthesis: Punctuation,
}

impl SourceElement for FunctionCall {
    fn span(&self) -> Span {
        self.identifier
            .span()
            .join(&self.right_parenthesis.span)
            .unwrap()
    }
}

impl<'a> Parser<'a> {
    /// Parses a [`Parenthesized`]
    pub fn parse_parenthesized(&mut self, handler: &dyn Handler<Error>) -> Option<Parenthesized> {
        let token_tree = self.step_into(
            Delimiter::Parenthesis,
            |parser| parser.parse_expression(handler),
            handler,
        )?;

        Some(Parenthesized {
            open: token_tree.open,
            expression: Box::new(token_tree.tree?),
            close: token_tree.close,
        })
    }

    /// Parses an [`Expression`]
    pub fn parse_expression(&mut self, handler: &dyn Handler<Error>) -> Option<Expression> {
        let mut first_primary = Expression::Primary(self.parse_primary(handler)?);
        let mut expressions = Vec::new();

        // Parses a list of binary operators and expressions
        while let Some(binary_operator) = self.try_parse_binary_operator() {
            expressions.push((
                binary_operator,
                Some(Expression::Primary(self.parse_primary(handler)?)),
            ));
        }

        // We have to fold the expressions based on the precedence of the binary operators and the
        // associativity of the binary operators.

        // This a vector of indices of the expressions that are candidates for folding.
        let mut candidate_index = 0;
        let mut current_precedence;

        while !expressions.is_empty() {
            // Reset the current precedence and the candidate indices
            current_precedence = 0;

            for (index, (binary_operator, _)) in expressions.iter().enumerate() {
                let new_precedence = binary_operator.get_precedence();
                match new_precedence.cmp(&current_precedence) {
                    // Push the index of the binary operator to the candidate indices
                    Ordering::Equal => {
                        if binary_operator.is_assignment() {
                            candidate_index = index;
                        }
                    }

                    // Clear the candidate indices and set the current precedence to the
                    // precedence of the current binary operator.
                    Ordering::Greater => {
                        current_precedence = new_precedence;
                        candidate_index = index;
                    }

                    Ordering::Less => (),
                }
            }

            // ASSUMPTION: The assignments have 1 precedence and are right associative.
            assert!(current_precedence > 0);

            if candidate_index == 0 {
                let (binary_operator, right_expression) = expressions.remove(0);

                // Replace the first expression with the folded expression.
                first_primary = Expression::Binary(Binary {
                    left_operand: Box::new(first_primary),
                    operator: binary_operator,
                    right_operand: Box::new(right_expression.unwrap()),
                });
            } else {
                let (binary_operator, right_expression) = expressions.remove(candidate_index);

                // Replace the expression at the index with the folded expression.
                expressions[candidate_index - 1].1 = Some(Expression::Binary(Binary {
                    left_operand: Box::new(expressions[candidate_index - 1].1.take().unwrap()),
                    operator: binary_operator,
                    right_operand: Box::new(right_expression.unwrap()),
                }));
            }
        }

        Some(first_primary)
    }

    /// Parses an [`Primary`]
    #[allow(clippy::too_many_lines)]
    pub fn parse_primary(&mut self, handler: &dyn Handler<Error>) -> Option<Primary> {
        // try to parse a prefixed expression
        match self.stop_at_significant() {
            // prefixed expression
            Reading::Atomic(Token::Punctuation(pun))
                if pun.punctuation == '!' || pun.punctuation == '-' =>
            {
                // eat the prefix oeprator
                self.forward();

                let operator = match pun.punctuation {
                    '!' => PrefixOperator::LogicalNot(pun),
                    '-' => PrefixOperator::Negate(pun),

                    _ => unreachable!(),
                };

                let operand = Box::new(self.parse_primary(handler)?);

                Some(Primary::Prefix(Prefix { operator, operand }))
            }

            // prefixed type cast
            Reading::IntoDelimited(left_bracket) if left_bracket.punctuation == '[' => {
                let tree = self.step_into(
                    Delimiter::Bracket,
                    |parser| match parser.next_significant_token() {
                        Reading::Atomic(Token::Keyword(k)) if k.keyword == KeywordKind::Int64 => {
                            Some(Type::Int64(k))
                        }
                        Reading::Atomic(Token::Keyword(k)) if k.keyword == KeywordKind::Float64 => {
                            Some(Type::Float64(k))
                        }
                        Reading::Atomic(Token::Keyword(k)) if k.keyword == KeywordKind::Bool => {
                            Some(Type::Bool(k))
                        }
                        unexpected => {
                            handler.receive(Error::UnexpectedSyntax(UnexpectedSyntax {
                                expected: crate::error::SyntaxKind::Type,
                                found: unexpected.into_token(),
                            }));
                            None
                        }
                    },
                    handler,
                )?;

                let type_cast = TypeCast {
                    left_bracket,
                    r#type: tree.tree?,
                    right_bracket: tree.close,
                };

                Some(Primary::Prefix(Prefix {
                    operator: PrefixOperator::TypeCast(type_cast),
                    operand: Box::new(self.parse_primary(handler)?),
                }))
            }

            // null expression
            Reading::Atomic(Token::Keyword(kw)) if kw.keyword == KeywordKind::Null => {
                self.forward();
                Some(Primary::Null(kw))
            }

            // identifier expression
            Reading::Atomic(Token::Identifier(identifier)) => {
                self.forward();

                // function call
                if matches!(
                    self.stop_at_significant(),
                    Reading::IntoDelimited(punc) if punc.punctuation == '('
                ) {
                    let token_tree = self.parse_enclosed_list(
                        Delimiter::Parenthesis,
                        ',',
                        |parser| parser.parse_expression(handler).map(Box::new),
                        handler,
                    )?;

                    Some(Primary::FunctionCall(FunctionCall {
                        identifier,
                        left_parenthesis: token_tree.open,
                        arguments: token_tree.list,
                        right_parenthesis: token_tree.close,
                    }))
                }
                // regular identifier expression
                else {
                    Some(Primary::Identifier(identifier))
                }
            }

            Reading::Atomic(Token::Keyword(true_kw)) if true_kw.keyword == KeywordKind::True => {
                self.forward();
                Some(Primary::True(true_kw))
            }

            Reading::Atomic(Token::Keyword(false_kw)) if false_kw.keyword == KeywordKind::False => {
                self.forward();
                Some(Primary::False(false_kw))
            }

            // numeric expression
            Reading::Atomic(Token::Numeric(numeric)) => {
                self.forward();

                let decimal = match self.peek() {
                    Reading::Atomic(Token::Punctuation(pun)) if pun.punctuation == '.' => {
                        self.forward();
                        Some(Decimal {
                            dot: pun,
                            numeric: self.parse_numeric(handler)?,
                        })
                    }
                    _ => None,
                };

                Some(Primary::Numeric(Numeric {
                    integer: numeric,
                    decimal,
                }))
            }

            // parenthesized expression
            Reading::IntoDelimited(punctuation) if punctuation.punctuation == '(' => self
                .parse_parenthesized(handler)
                .map(Primary::Parenthesized),

            unexpected => {
                // make progress
                self.forward();

                handler.receive(Error::UnexpectedSyntax(UnexpectedSyntax {
                    expected: crate::error::SyntaxKind::Expression,
                    found: unexpected.into_token(),
                }));

                None
            }
        }
    }

    fn try_parse_binary_operator(&mut self) -> Option<BinaryOperator> {
        let first_level = self.try_parse(|parser| match parser.next_significant_token() {
            Reading::Atomic(Token::Punctuation(p)) => match p.punctuation {
                '+' => Some(BinaryOperator::Add(p)),
                '-' => Some(BinaryOperator::Subtract(p)),
                '*' => Some(BinaryOperator::Multiply(p)),
                '/' => Some(BinaryOperator::Divide(p)),
                '%' => Some(BinaryOperator::Modulo(p)),
                '=' => Some(BinaryOperator::Assign(p)),
                '!' => {
                    let equal = parser.parse_punctuation('=', false, &Dummy)?;
                    Some(BinaryOperator::NotEqual(p, equal))
                }
                '>' => Some(BinaryOperator::GreaterThan(p)),
                '<' => Some(BinaryOperator::LessThan(p)),
                _ => None,
            },
            Reading::Atomic(Token::Keyword(k)) => match k.keyword {
                KeywordKind::And => Some(BinaryOperator::LogicalAnd(k)),
                KeywordKind::Or => Some(BinaryOperator::LogicalOr(k)),
                _ => None,
            },
            _ => None,
        })?;

        Some(
            self.try_parse(|parser| match (first_level.clone(), parser.next_token()) {
                (first_level, Reading::Atomic(Token::Punctuation(n))) => {
                    match (first_level, n.punctuation) {
                        (BinaryOperator::Add(p), '=') => Some(BinaryOperator::CompoundAdd(p, n)),
                        (BinaryOperator::Subtract(p), '=') => {
                            Some(BinaryOperator::CompoundSubtract(p, n))
                        }
                        (BinaryOperator::Multiply(p), '=') => {
                            Some(BinaryOperator::CompoundMultiply(p, n))
                        }
                        (BinaryOperator::Divide(p), '=') => {
                            Some(BinaryOperator::CompoundDivide(p, n))
                        }
                        (BinaryOperator::Modulo(p), '=') => {
                            Some(BinaryOperator::CompoundModulo(p, n))
                        }
                        (BinaryOperator::Assign(p), '=') => Some(BinaryOperator::Equal(p, n)),
                        (BinaryOperator::GreaterThan(p), '=') => {
                            Some(BinaryOperator::GreaterThanOrEqual(p, n))
                        }
                        (BinaryOperator::LessThan(p), '=') => {
                            Some(BinaryOperator::LessThanOrEqual(p, n))
                        }
                        _ => None,
                    }
                }
                _ => None,
            })
            .unwrap_or(first_level),
        )
    }
}

#[cfg(test)]
pub(super) mod tests;
