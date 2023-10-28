use std::fmt::Display;

use enum_as_inner::EnumAsInner;
use flux_lexical::token::KeywordKind;
use flux_test::input::Input;
use proptest::{
    prelude::Arbitrary,
    prop_assert_eq, prop_oneof, proptest,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::syntax_tree::tests::{self, ConnectedList, ConstantPunctuation, Identifier};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Expression {
    Primary(Primary),
    Binary(Binary),
}

impl Arbitrary for Expression {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        fn into_primary(
            expression: impl Strategy<Value = Expression> + 'static,
        ) -> BoxedStrategy<Primary> {
            expression
                .prop_filter_map("filter for primary variant", |x| x.into_primary().ok())
                .boxed()
        }

        let leaf = prop_oneof![
            Identifier::arbitrary().prop_map(|x| Self::Primary(Primary::Identifier(x))),
            Just(Self::Primary(Primary::Null)),
            Just(Self::Primary(Primary::True)),
            Just(Self::Primary(Primary::False)),
            Numeric::arbitrary().prop_map(|x| Self::Primary(Primary::Numeric(x))),
        ];

        leaf.prop_recursive(16, 6, 4, |inner| {
            prop_oneof![
                Binary::arbitrary_with(Some(inner.clone())).prop_map(Self::Binary),
                FunctionCall::arbitrary_with(Some(inner.clone()))
                    .prop_map(|x| Self::Primary(Primary::FunctionCall(x))),
                Prefix::arbitrary_with(Some(into_primary(inner)))
                    .prop_map(|x| Self::Primary(Primary::Prefix(x))),
            ]
        })
        .boxed()
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Primary(primary) => primary.fmt(f),
            Self::Binary(binary) => binary.fmt(f),
        }
    }
}

impl Input<&super::Expression> for &Expression {
    fn assert(self, output: &super::Expression) -> TestCaseResult {
        match (self, output) {
            (Expression::Primary(primary1), super::Expression::Primary(primary2)) => {
                primary1.assert(primary2)
            }
            (Expression::Binary(binary1), super::Expression::Binary(binary2)) => {
                binary1.assert(binary2)
            }
            _ => Err(TestCaseError::fail(format!(
                "expected {self:?} but found {output:?}",
            ))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Primary {
    Identifier(Identifier),
    Null,
    Numeric(Numeric),
    Prefix(Prefix),
    Parenthesized(Parenthesized),
    FunctionCall(FunctionCall),
    True,
    False,
}

impl Arbitrary for Primary {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        Expression::arbitrary()
            .prop_filter_map("filter for primary variant", |x| x.into_primary().ok())
            .boxed()
    }
}

impl Input<&super::Primary> for &Primary {
    fn assert(self, output: &super::Primary) -> TestCaseResult {
        match (self, output) {
            (Primary::Identifier(id1), super::Primary::Identifier(id2)) => id1.assert(id2),
            (Primary::Null, super::Primary::Null(..))
            | (Primary::True, super::Primary::True(..))
            | (Primary::False, super::Primary::False(..)) => Ok(()),
            (Primary::Numeric(num1), super::Primary::Numeric(num2)) => num1.assert(num2),
            (Primary::Prefix(prefix1), super::Primary::Prefix(prefix2)) => prefix1.assert(prefix2),
            (
                Primary::Parenthesized(parenthesized1),
                super::Primary::Parenthesized(parenthesized2),
            ) => parenthesized1.assert(parenthesized2),
            (
                Primary::FunctionCall(function_call1),
                super::Primary::FunctionCall(function_call2),
            ) => function_call1.assert(function_call2),
            _ => Err(TestCaseError::fail(format!(
                "expected {self:?} but found {output:?}",
            ))),
        }
    }
}

impl Display for Primary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(id) => write!(f, "{id}"),
            Self::Null => write!(f, "null"),
            Self::Numeric(numeric) => write!(f, "{numeric}"),
            Self::Prefix(prefix) => write!(f, "{prefix}"),
            Self::Parenthesized(parenthesized) => write!(f, "({parenthesized})"),
            Self::FunctionCall(function_call) => write!(f, "{function_call}"),
            Self::True => write!(f, "true"),
            Self::False => write!(f, "false"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Int64,
    Float64,
    Bool,
}

impl Arbitrary for Type {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![Just(Self::Int64), Just(Self::Float64), Just(Self::Bool)].boxed()
    }
}

impl Input<&super::Type> for &Type {
    fn assert(self, output: &super::Type) -> TestCaseResult {
        match (self, output) {
            (Type::Int64, super::Type::Int64(..))
            | (Type::Float64, super::Type::Float64(..))
            | (Type::Bool, super::Type::Bool(..)) => Ok(()),
            _ => Err(TestCaseError::fail(format!(
                "expected {self:?} but found {output:?}",
            ))),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int64 => write!(f, "int64"),
            Self::Float64 => write!(f, "float64"),
            Self::Bool => write!(f, "bool"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PrefixOperator {
    LogicalNot,
    Negate,
    TypeCast(Type),
}

impl Arbitrary for PrefixOperator {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::LogicalNot),
            Just(Self::Negate),
            Type::arbitrary().prop_map(PrefixOperator::TypeCast)
        ]
        .boxed()
    }
}

impl Display for PrefixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LogicalNot => write!(f, "!"),
            Self::Negate => write!(f, "-"),
            Self::TypeCast(ty) => write!(f, "[{ty}]"),
        }
    }
}

impl Input<&super::PrefixOperator> for &PrefixOperator {
    fn assert(self, output: &super::PrefixOperator) -> TestCaseResult {
        match (self, output) {
            (PrefixOperator::LogicalNot, super::PrefixOperator::LogicalNot(..))
            | (PrefixOperator::Negate, super::PrefixOperator::Negate(..)) => Ok(()),
            (PrefixOperator::TypeCast(ty1), super::PrefixOperator::TypeCast(ty2)) => {
                ty1.assert(&ty2.r#type)
            }
            _ => Err(TestCaseError::fail(format!(
                "expected {self:?} but found {output:?}",
            ))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Numeric {
    pub integer: tests::Numeric,
    pub decimal: Option<tests::Numeric>,
}

impl Arbitrary for Numeric {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        let integer = tests::Numeric::arbitrary();
        let decimal = proptest::option::of(tests::Numeric::arbitrary());

        (integer, decimal)
            .prop_map(|(integer, decimal)| Self { integer, decimal })
            .boxed()
    }
}

impl Display for Numeric {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.integer.fmt(f)?;
        if let Some(decimal) = &self.decimal {
            write!(f, ".")?;
            decimal.fmt(f)?;
        }
        Ok(())
    }
}

impl Input<&super::Decimal> for &tests::Numeric {
    fn assert(self, output: &super::Decimal) -> TestCaseResult { self.assert(&output.numeric) }
}

impl Input<&super::Numeric> for &Numeric {
    fn assert(self, output: &super::Numeric) -> TestCaseResult {
        self.integer.assert(&output.integer)?;
        self.decimal.as_ref().assert(output.decimal.as_ref())?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Prefix {
    pub operator: PrefixOperator,
    pub operand: Box<Primary>,
}

impl Arbitrary for Prefix {
    type Parameters = Option<BoxedStrategy<Primary>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let strategy = args.unwrap_or_else(Primary::arbitrary);

        (PrefixOperator::arbitrary(), strategy)
            .prop_map(|(operator, operand)| Self {
                operator,
                operand: Box::new(operand),
            })
            .boxed()
    }
}

impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{operator}{operand}",
            operator = self.operator,
            operand = self.operand
        )
    }
}

impl Input<&super::Prefix> for &Prefix {
    fn assert(self, output: &super::Prefix) -> TestCaseResult {
        self.operator.assert(&output.operator)?;
        self.operand.assert(&output.operand)?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Parenthesized {
    pub expression: Box<Expression>,
}

impl Arbitrary for Parenthesized {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        let strategy = args.unwrap_or_else(Expression::arbitrary);

        strategy
            .prop_map(|expression| Self {
                expression: Box::new(expression),
            })
            .boxed()
    }
}

impl Display for Parenthesized {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({expression})", expression = self.expression)
    }
}

impl Input<&super::Parenthesized> for &Parenthesized {
    fn assert(self, output: &super::Parenthesized) -> TestCaseResult {
        self.expression.assert(&output.expression)?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionCall {
    identifier: Identifier,
    arguments: Option<ConnectedList<Box<Expression>, ConstantPunctuation<','>>>,
}

impl Arbitrary for FunctionCall {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        let strategy = arg.unwrap_or_else(Expression::arbitrary).prop_map(Box::new);

        (
            Identifier::arbitrary(),
            proptest::option::of(ConnectedList::arbitrary_with(
                strategy,
                ConstantPunctuation::<','>::arbitrary(),
            )),
        )
            .prop_map(|(identifier, arguments)| Self {
                identifier,
                arguments,
            })
            .boxed()
    }
}

impl Display for FunctionCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.identifier)?;

        if let Some(value) = &self.arguments {
            value.fmt(f)?;
        }

        write!(f, ")")?;

        Ok(())
    }
}

impl Input<&super::FunctionCall> for &FunctionCall {
    fn assert(self, output: &super::FunctionCall) -> TestCaseResult {
        self.identifier.assert(&output.identifier)?;
        self.arguments.as_ref().assert(output.arguments.as_ref())?;

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Assign,
    CompoundAdd,
    CompoundSubtract,
    CompoundMultiply,
    CompoundDivide,
    CompoundModulo,
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    LogicalAnd,
    LogicalOr,
}

impl Input<&super::BinaryOperator> for &BinaryOperator {
    fn assert(self, output: &super::BinaryOperator) -> TestCaseResult {
        match (self, output) {
            (BinaryOperator::Add, super::BinaryOperator::Add(k)) => {
                prop_assert_eq!(k.punctuation, '+');
            }
            (BinaryOperator::Subtract, super::BinaryOperator::Subtract(k)) => {
                prop_assert_eq!(k.punctuation, '-');
            }
            (BinaryOperator::Multiply, super::BinaryOperator::Multiply(k)) => {
                prop_assert_eq!(k.punctuation, '*');
            }
            (BinaryOperator::Divide, super::BinaryOperator::Divide(k)) => {
                prop_assert_eq!(k.punctuation, '/');
            }
            (BinaryOperator::Modulo, super::BinaryOperator::Modulo(k)) => {
                prop_assert_eq!(k.punctuation, '%');
            }
            (BinaryOperator::Assign, super::BinaryOperator::Assign(k)) => {
                prop_assert_eq!(k.punctuation, '=');
            }
            (BinaryOperator::CompoundAdd, super::BinaryOperator::CompoundAdd(f, s)) => {
                prop_assert_eq!(f.punctuation, '+');
                prop_assert_eq!(s.punctuation, '=');
            }
            (BinaryOperator::CompoundSubtract, super::BinaryOperator::CompoundSubtract(f, s)) => {
                prop_assert_eq!(f.punctuation, '-');
                prop_assert_eq!(s.punctuation, '=');
            }
            (BinaryOperator::CompoundMultiply, super::BinaryOperator::CompoundMultiply(f, s)) => {
                prop_assert_eq!(f.punctuation, '*');
                prop_assert_eq!(s.punctuation, '=');
            }
            (BinaryOperator::CompoundDivide, super::BinaryOperator::CompoundDivide(f, s)) => {
                prop_assert_eq!(f.punctuation, '/');
                prop_assert_eq!(s.punctuation, '=');
            }
            (BinaryOperator::CompoundModulo, super::BinaryOperator::CompoundModulo(f, s)) => {
                prop_assert_eq!(f.punctuation, '%');
                prop_assert_eq!(s.punctuation, '=');
            }
            (BinaryOperator::Equal, super::BinaryOperator::Equal(f, s)) => {
                prop_assert_eq!(f.punctuation, '=');
                prop_assert_eq!(s.punctuation, '=');
            }
            (BinaryOperator::NotEqual, super::BinaryOperator::NotEqual(f, s)) => {
                prop_assert_eq!(f.punctuation, '!');
                prop_assert_eq!(s.punctuation, '=');
            }
            (BinaryOperator::GreaterThan, super::BinaryOperator::GreaterThan(f)) => {
                prop_assert_eq!(f.punctuation, '>');
            }
            (
                BinaryOperator::GreaterThanOrEqual,
                super::BinaryOperator::GreaterThanOrEqual(f, s),
            ) => {
                prop_assert_eq!(f.punctuation, '>');
                prop_assert_eq!(s.punctuation, '=');
            }
            (BinaryOperator::LessThan, super::BinaryOperator::LessThan(f)) => {
                prop_assert_eq!(f.punctuation, '<');
            }
            (BinaryOperator::LessThanOrEqual, super::BinaryOperator::LessThanOrEqual(f, s)) => {
                prop_assert_eq!(f.punctuation, '<');
                prop_assert_eq!(s.punctuation, '=');
            }
            (BinaryOperator::LogicalAnd, super::BinaryOperator::LogicalAnd(k)) => {
                prop_assert_eq!(k.keyword, KeywordKind::And);
            }
            (BinaryOperator::LogicalOr, super::BinaryOperator::LogicalOr(k)) => {
                prop_assert_eq!(k.keyword, KeywordKind::Or);
            }
            _ => {
                return Err(TestCaseError::fail(format!(
                    "expected `{self:?}`, found `{output:?}`",
                )))
            }
        }

        Ok(())
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Subtract => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::Modulo => write!(f, "%"),
            Self::Assign => write!(f, "="),
            Self::CompoundAdd => write!(f, "+="),
            Self::CompoundSubtract => write!(f, "-="),
            Self::CompoundMultiply => write!(f, "*="),
            Self::CompoundDivide => write!(f, "/="),
            Self::CompoundModulo => write!(f, "%="),
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::GreaterThan => write!(f, ">"),
            Self::GreaterThanOrEqual => write!(f, ">="),
            Self::LessThan => write!(f, "<"),
            Self::LessThanOrEqual => write!(f, "<="),
            Self::LogicalAnd => write!(f, "and"),
            Self::LogicalOr => write!(f, "or"),
        }
    }
}

impl Arbitrary for BinaryOperator {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Add),
            Just(Self::Subtract),
            Just(Self::Multiply),
            Just(Self::Divide),
            Just(Self::Modulo),
            Just(Self::Assign),
            Just(Self::CompoundAdd),
            Just(Self::CompoundSubtract),
            Just(Self::CompoundMultiply),
            Just(Self::CompoundDivide),
            Just(Self::CompoundModulo),
            Just(Self::Equal),
            Just(Self::NotEqual),
            Just(Self::GreaterThan),
            Just(Self::GreaterThanOrEqual),
            Just(Self::LessThan),
            Just(Self::LessThanOrEqual),
            Just(Self::LogicalAnd),
            Just(Self::LogicalOr),
        ]
        .boxed()
    }
}

impl BinaryOperator {
    #[must_use]
    pub fn is_assignment(self) -> bool {
        matches!(
            self,
            Self::Assign
                | Self::CompoundAdd
                | Self::CompoundSubtract
                | Self::CompoundMultiply
                | Self::CompoundDivide
                | Self::CompoundModulo
        )
    }

    #[must_use]
    pub fn get_precedence(self) -> u32 {
        match self {
            Self::Assign
            | Self::CompoundAdd
            | Self::CompoundSubtract
            | Self::CompoundMultiply
            | Self::CompoundDivide
            | Self::CompoundModulo => 1,
            Self::LogicalOr => 2,
            Self::LogicalAnd => 3,
            Self::Equal | Self::NotEqual => 4,
            Self::LessThan
            | Self::LessThanOrEqual
            | Self::GreaterThan
            | Self::GreaterThanOrEqual => 5,
            Self::Add | Self::Subtract => 6,
            Self::Multiply | Self::Divide | Self::Modulo => 7,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Binary {
    pub left_operand: Box<Expression>,
    pub operator: BinaryOperator,
    pub right_operand: Box<Expression>,
}

impl Input<&super::Binary> for &Binary {
    fn assert(self, output: &super::Binary) -> TestCaseResult {
        self.operator.assert(output.operator())?;

        self.left_operand.assert(output.left_operand())?;
        self.right_operand.assert(output.right_operand())?;

        Ok(())
    }
}

impl Display for Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.left_operand, self.operator, self.right_operand
        )
    }
}

impl Arbitrary for Binary {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        fn create_binary(
            operator: BinaryOperator,
            binary_input: Binary,
            expression_input: Expression,
            swap: bool,
        ) -> Option<Binary> {
            match operator
                .get_precedence()
                .cmp(&binary_input.operator.get_precedence())
            {
                std::cmp::Ordering::Less => {
                    let mut left_operand = Box::new(Expression::Binary(binary_input));
                    let mut right_operand = Box::new(expression_input);

                    if swap {
                        std::mem::swap(&mut left_operand, &mut right_operand);
                    }

                    Some(Binary {
                        left_operand,
                        operator,
                        right_operand,
                    })
                }
                std::cmp::Ordering::Equal => {
                    let mut left_operand = Box::new(Expression::Binary(binary_input));
                    let mut right_operand = Box::new(expression_input);

                    if operator.is_assignment() {
                        std::mem::swap(&mut left_operand, &mut right_operand);
                    }

                    Some(Binary {
                        left_operand,
                        operator,
                        right_operand,
                    })
                }
                std::cmp::Ordering::Greater => None,
            }
        }
        let expression_strategy = args.unwrap_or_else(Expression::arbitrary);

        (
            expression_strategy.clone(),
            BinaryOperator::arbitrary(),
            expression_strategy,
        )
            .prop_filter_map("disambiguate the syntax", |(left, operator, right)| match (
                left, right,
            ) {
                (Expression::Binary(left_operand), Expression::Binary(right_operand)) => {
                    if operator.get_precedence() >= left_operand.operator.get_precedence()
                        || operator.get_precedence() >= right_operand.operator.get_precedence()
                    {
                        None
                    } else {
                        Some(Self {
                            left_operand: Box::new(Expression::Binary(left_operand)),
                            operator,
                            right_operand: Box::new(Expression::Binary(right_operand)),
                        })
                    }
                }
                (Expression::Binary(left_operand), right_operand) => {
                    create_binary(operator, left_operand, right_operand, false)
                }
                (left_operand, Expression::Binary(right_operand)) => {
                    create_binary(operator, right_operand, left_operand, true)
                }
                (left_operand, right_operand) => Some(Self {
                    left_operand: Box::new(left_operand),
                    operator,
                    right_operand: Box::new(right_operand),
                }),
            })
            .boxed()
    }
}

proptest! {
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn epression_test(
        expression_input in Expression::arbitrary()
    ) {
        let expression = tests::parse(
            &expression_input,
            |parser, handler| parser.parse_expression(handler)
        )?;

        expression_input.assert(&expression)?;
    }
}
