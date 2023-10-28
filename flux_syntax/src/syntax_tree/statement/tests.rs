use std::fmt::Display;

use enum_as_inner::EnumAsInner;
use flux_test::input::Input;
use proptest::{
    prelude::Arbitrary,
    prop_oneof, proptest,
    strategy::{BoxedStrategy, Just, Strategy},
    test_runner::{TestCaseError, TestCaseResult},
};

use crate::syntax_tree::{
    expression::tests::Expression,
    tests::{self, Identifier},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    Semi(Semi),
    Block(Block),
    Continue,
    Break,
    Return(Return),
    Conditional(Conditional),
    Print(Print),
    While(While),
}

impl Input<&super::Statement> for &Statement {
    fn assert(self, output: &super::Statement) -> TestCaseResult {
        match (self, output) {
            (
                Statement::VariableDeclaration(input),
                super::Statement::VariableDeclaration(output),
            ) => input.assert(output),
            (Statement::Semi(input), super::Statement::Semi(output)) => input.assert(output),
            (Statement::Block(input), super::Statement::Block(output)) => input.assert(output),
            (Statement::Continue, super::Statement::Continue(..))
            | (Statement::Break, super::Statement::Break(..)) => Ok(()),
            (Statement::Return(input), super::Statement::Return(output)) => input.assert(output),
            (Statement::Conditional(input), super::Statement::Conditional(output)) => {
                input.assert(output)
            }
            (Statement::Print(input), super::Statement::Print(output)) => input.assert(output),
            (Statement::While(input), super::Statement::While(output)) => input.assert(output),

            _ => Err(TestCaseError::fail(format!(
                "expected {self:?}, found {output:?}",
            ))),
        }
    }
}

impl Arbitrary for Statement {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        let leaf = prop_oneof![
            Expression::arbitrary().prop_map(|x| Self::Semi(Semi(x))),
            VariableDeclaration::arbitrary().prop_map(Statement::VariableDeclaration),
            Return::arbitrary().prop_map(Statement::Return),
            Print::arbitrary().prop_map(Statement::Print),
            Just(Self::Continue),
            Just(Self::Break),
        ];

        leaf.prop_recursive(
            16, // levels deep
            64, // max size
            4,
            |inner| {
                prop_oneof![
                    While::arbitrary_with(Some(inner.clone())).prop_map(Self::While),
                    Conditional::arbitrary_with(Some(inner.clone())).prop_map(Self::Conditional),
                    Block::arbitrary_with(Some(inner)).prop_map(Self::Block)
                ]
            },
        )
        .boxed()
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VariableDeclaration(declaration) => declaration.fmt(f),
            Self::Semi(semi) => semi.fmt(f),
            Self::Block(block) => block.fmt(f),
            Self::Continue => write!(f, "continue;"),
            Self::Break => write!(f, "break;"),
            Self::Return(r#return) => r#return.fmt(f),
            Self::Conditional(conditional) => conditional.fmt(f),
            Self::Print(print) => print.fmt(f),
            Self::While(while_) => while_.fmt(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableDeclaration {
    pub identifier: Identifier,
    pub value: Option<Expression>,
}

impl Input<&super::VariableDeclaration> for &VariableDeclaration {
    fn assert(self, output: &super::VariableDeclaration) -> TestCaseResult {
        self.identifier.assert(&output.identifier)?;

        self.value
            .as_ref()
            .assert(output.assignment.as_ref().map(|x| &x.expression))?;

        Ok(())
    }
}

impl Arbitrary for VariableDeclaration {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary_with(()),
            proptest::option::of(Expression::arbitrary_with(())),
        )
            .prop_map(|(identifier, value)| Self { identifier, value })
            .boxed()
    }
}

impl Display for VariableDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {}", self.identifier)?;

        if let Some(value) = &self.value {
            write!(f, " = {value}")?;
        }

        write!(f, ";")?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Semi(Expression);

impl Input<&super::Semi> for &Semi {
    fn assert(self, output: &super::Semi) -> TestCaseResult { self.0.assert(&output.expression) }
}

impl Arbitrary for Semi {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        Expression::arbitrary_with(()).prop_map(Self).boxed()
    }
}

impl Display for Semi {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{};", self.0) }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Block {
    pub statements: Vec<Statement>,
}

impl Input<&super::Block> for &Block {
    fn assert(self, output: &super::Block) -> TestCaseResult {
        self.statements.assert(&output.statements)
    }
}

impl Arbitrary for Block {
    type Parameters = Option<BoxedStrategy<Statement>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        let strategy = arg.unwrap_or_else(Statement::arbitrary);

        proptest::collection::vec(strategy, 0..4)
            .prop_map(|statements| Self { statements })
            .boxed()
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for statement in &self.statements {
            write!(f, "{statement}")?;
        }
        write!(f, "}}")?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Return(Option<Expression>);

impl Input<&super::Return> for &Return {
    fn assert(self, output: &super::Return) -> TestCaseResult {
        self.0.as_ref().assert(output.expression.as_ref())
    }
}

impl Arbitrary for Return {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        proptest::option::of(Expression::arbitrary())
            .prop_map(Self)
            .boxed()
    }
}

impl Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return")?;

        if let Some(expression) = &self.0 {
            write!(f, " {expression}")?;
        }

        write!(f, ";")?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Conditional {
    pub condition: Expression,
    pub if_statement: Box<Statement>,
    pub else_statement: Option<Box<Statement>>,
}

impl Input<&super::Conditional> for &Conditional {
    fn assert(self, output: &super::Conditional) -> TestCaseResult {
        self.condition.assert(output.parenthesized.expression())?;
        self.if_statement.assert(&output.if_statement)?;
        self.else_statement
            .as_ref()
            .assert(output.r#else.as_ref().map(|x| &x.else_statement))?;

        Ok(())
    }
}

impl Arbitrary for Conditional {
    type Parameters = Option<BoxedStrategy<Statement>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        let strategy = arg.unwrap_or_else(Statement::arbitrary);

        (
            Expression::arbitrary_with(()),
            strategy.clone(),
            proptest::option::of(strategy),
        )
            .prop_map(|(condition, if_statement, else_statement)| {
                let if_statement_is_conditional = if_statement.as_conditional().is_some();
                Self {
                    condition,
                    if_statement: Box::new(if_statement),
                    else_statement: if if_statement_is_conditional {
                        None
                    } else {
                        else_statement.map(Box::new)
                    },
                }
            })
            .boxed()
    }
}

impl Display for Conditional {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "if ({condition}) {if_statement}",
            condition = self.condition,
            if_statement = self.if_statement
        )?;

        if let Some(else_statement) = &self.else_statement {
            write!(f, " else {else_statement}",)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct While {
    pub condition: Expression,
    pub statement: Box<Statement>,
}

impl Input<&super::While> for &While {
    fn assert(self, output: &super::While) -> TestCaseResult {
        self.condition.assert(output.parenthesized.expression())?;
        self.statement.assert(&output.statement)?;

        Ok(())
    }
}

impl Arbitrary for While {
    type Parameters = Option<BoxedStrategy<Statement>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        let strategy = arg.unwrap_or_else(Statement::arbitrary);

        (Expression::arbitrary_with(()), strategy)
            .prop_map(|(condition, statement)| Self {
                condition,
                statement: Box::new(statement),
            })
            .boxed()
    }
}

impl Display for While {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "while ({condition}) {statement}",
            condition = self.condition,
            statement = self.statement
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Print(Expression);

impl Input<&super::Print> for &Print {
    fn assert(self, output: &super::Print) -> TestCaseResult { self.0.assert(&output.expression) }
}

impl Arbitrary for Print {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
        Expression::arbitrary_with(()).prop_map(Self).boxed()
    }
}

impl Display for Print {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "print {};", self.0)
    }
}

proptest! {
    #![
        proptest_config(
            proptest::test_runner::Config {
                max_shrink_iters: 4096,
                ..proptest::test_runner::Config::default()
            }
        )
    ]
    #[test]
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn statement_test(
        statement_input in Statement::arbitrary()
    ) {
        println!("{statement_input}");
        let statement = tests::parse(
            &statement_input,
            |parser, handler| parser.parse_statement(handler)
        )?;

        statement_input.assert(&statement)?;
    }
}
