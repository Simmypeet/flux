//! Contains all definition of instructions in the control flow graph.

use enum_as_inner::EnumAsInner;
use flux_syntax::syntax_tree::expression::Expression;

use super::BasicBlock;
use crate::arena::ID;

/// Represents a variable declaration.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableDeclaration {
    /// The name of the variable to declare.
    pub name: String,

    /// The initial value of the variable.
    ///
    /// if `None` the variable is initialized to `null`.
    pub expression: Option<Expression>,
}

/// A jump to a particular basic block based on a condition.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConditionalJump {
    /// The condition expression.
    pub condition: Expression,

    /// The basic block id to jump to if the condition is true.
    pub true_block: ID<BasicBlock>,

    /// The basic block id to jump to if the condition is false.
    pub false_block: ID<BasicBlock>,
}

/// Represents a single instruction in the control flow graph.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Instruction {
    /// Pushes a new scope on the stack.
    ScopePush,

    /// Pops the top scope from the stack.
    ScopePop,

    /// Evaluates an expression.
    Evalueate(Expression),

    /// Declares a new variable.
    ///
    /// The first element of the tuple is the name of the variable.
    /// The second element of the tuple is the expression that is assigned to the variable.
    VariableDeclaration(VariableDeclaration),

    /// Unconditionally jumps to the given basic block index.
    Jump(ID<BasicBlock>),

    /// Returns from the function.
    Return(Option<Expression>),

    /// Jumps to a particular basic block based on a condition
    ConditionalJump(ConditionalJump),

    /// Prints the given expression.
    Print(Expression),
}
