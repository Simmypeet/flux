use std::fmt::Display;

use flux_test::input::Input;
use proptest::{prelude::Arbitrary, strategy::Strategy, test_runner::TestCaseResult};

use crate::syntax_tree::{
    statement::tests::Block,
    tests::{ConnectedList, ConstantPunctuation, Identifier},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Function {
    identifier: Identifier,
    parameter_list: Option<ConnectedList<Identifier, ConstantPunctuation<','>>>,
    block: Block,
}

impl Arbitrary for Function {
    type Parameters = ();
    type Strategy = proptest::prelude::BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        (
            Identifier::arbitrary(),
            proptest::option::of(ConnectedList::arbitrary_with(
                Identifier::arbitrary(),
                ConstantPunctuation::<','>::arbitrary(),
            )),
            Block::arbitrary(),
        )
            .prop_map(|(identifier, parameter_list, block)| Self {
                identifier,
                parameter_list,
                block,
            })
            .boxed()
    }
}

impl Input<&super::Function> for &Function {
    fn assert(self, output: &super::Function) -> TestCaseResult {
        self.parameter_list
            .as_ref()
            .assert(output.parameters.as_ref())?;
        self.block.assert(&output.block)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "function {}(", self.identifier)?;

        if let Some(parameters) = &self.parameter_list {
            write!(f, "{parameters}",)?;
        }

        write!(f, ") {}", self.block)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Declaration {
    Function(Function),
}

impl Arbitrary for Declaration {
    type Parameters = ();
    type Strategy = proptest::prelude::BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        proptest::prop_oneof![Function::arbitrary().prop_map(Self::Function),].boxed()
    }
}

impl Input<&super::Declaration> for &Declaration {
    fn assert(self, output: &super::Declaration) -> TestCaseResult {
        match (self, output) {
            (Declaration::Function(function), super::Declaration::Function(output)) => {
                function.assert(output)
            }
        }
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function(function) => write!(f, "{function}"),
        }
    }
}
