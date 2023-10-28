//! Contains the definition of [`Error`]

use std::fmt::Display;

use flux_base::{
    log::{Message, Severity, SourceCodeDisplay},
    source_file::{SourceElement, Span},
};
use flux_syntax::syntax_tree::statement::{Break, Continue};

use crate::interpreter::value::Type;

/// `break` statement was found outside of a loop.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BreakOutsideOfLoop {
    /// The `break` statement that was found outside of a loop.
    pub break_statement: Break,
}

impl Display for BreakOutsideOfLoop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
            Message::new(
                Severity::Error,
                "found a `break` statement outside of a loop"
            ),
            SourceCodeDisplay::new(&self.break_statement.span(), Option::<i32>::None)
        )
    }
}

/// `continue` statement was found outside of a loop.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ContinueOutsideOfLoop {
    /// The `continue` statement that was found outside of a loop.
    pub continue_statement: Continue,
}

impl Display for ContinueOutsideOfLoop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
            Message::new(
                Severity::Error,
                "found a `continue` statement outside of a loop"
            ),
            SourceCodeDisplay::new(&self.continue_statement.span(), Option::<i32>::None)
        )
    }
}

/// The type of an expression does not match the expected type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeMismatch {
    /// The expected type.
    pub expected: Type,

    /// The type of the expression.
    pub found: Type,

    /// The span of the expression.
    pub expression_span: Span,
}

impl Display for TypeMismatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
            Message::new(
                Severity::Error,
                format!(
                    "expected type `{}` but found type `{}`",
                    self.expected, self.found
                )
            ),
            SourceCodeDisplay::new(
                &self.expression_span,
                Some(format_args!("has type `{}`", self.found))
            )
        )
    }
}

/// A function with the given name was not found.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionNotFound {
    /// The name of the function that was not found.
    pub span: Span,
}

impl Display for FunctionNotFound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
            Message::new(Severity::Error, "function not found"),
            SourceCodeDisplay::new(&self.span, Option::<i32>::None)
        )
    }
}

/// The function was called with the wrong number of arguments.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ArgumentCountMismatch {
    /// The expected number of arguments.
    pub expected: usize,

    /// The number of arguments that were found.
    pub found: usize,

    /// The span of the function call.
    pub span: Span,
}

impl Display for ArgumentCountMismatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
            Message::new(
                Severity::Error,
                format!(
                    "expected {} argument(s) but found {}",
                    self.expected, self.found
                )
            ),
            SourceCodeDisplay::new(&self.span, Option::<i32>::None)
        )
    }
}

/// A variable with the given name was not found.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableNotFound {
    /// The name of the variable that was not found.
    pub span: Span,
}

impl Display for VariableNotFound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
            Message::new(Severity::Error, "variable not found"),
            SourceCodeDisplay::new(&self.span, Option::<i32>::None)
        )
    }
}

/// The given numeric value was too large.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TooLargeNumeric {
    /// The given numeric value was too large.
    pub span: Span,
}

impl Display for TooLargeNumeric {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
            Message::new(Severity::Error, "numeric value too large"),
            SourceCodeDisplay::new(&self.span, Option::<i32>::None)
        )
    }
}

/// Function main should take no argument.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionMainTakeNoArgument {
    /// The span of the function main.
    pub span: Span,
}

impl Display for FunctionMainTakeNoArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
            Message::new(Severity::Error, "function main should take no argument"),
            SourceCodeDisplay::new(&self.span, Option::<i32>::None)
        )
    }
}

/// Function redeclaration.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionRedeclaration {
    /// The span of the existing function.
    pub existing_function_span: Span,

    /// The span of the new function (redeclaration)
    pub new_function_span: Span,
}

impl Display for FunctionRedeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}\n{}",
            Message::new(
                Severity::Error,
                format_args!(
                    "function `{}` is already declared",
                    self.existing_function_span.str()
                )
            ),
            SourceCodeDisplay::new(
                &self.existing_function_span,
                Some("previously declared here")
            ),
            SourceCodeDisplay::new(&self.new_function_span, Some("redeclaration found here"))
        )
    }
}

/// Value of `null` cannot be casted to any type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NullTypeCasting {
    /// The span of the type casting.
    pub span: Span,
}

impl Display for NullTypeCasting {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
            Message::new(
                Severity::Error,
                "value of `null` cannot be casted to any type"
            ),
            SourceCodeDisplay::new(&self.span, Option::<i32>::None)
        )
    }
}

/// Arithmetic operation can only be performed on numeric types.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ArithmeticOnNonNumberType {
    /// The span of the arithmetic operation.
    pub span: Span,

    /// The type of the operand.
    pub found: Type,
}

impl Display for ArithmeticOnNonNumberType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = format!(
            "arithmetic operation can only be performed on numeric types, but found type `{}`",
            self.found
        );

        write!(
            f,
            "{}\n{}",
            Message::new(Severity::Error, message),
            SourceCodeDisplay::new(&self.span, Option::<i32>::None)
        )
    }
}

/// Assignment to an rvalue.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AssignToRValue {
    /// The span of the assignment.
    pub span: Span,
}

impl Display for AssignToRValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
            Message::new(Severity::Error, "assignment to an rvalue"),
            SourceCodeDisplay::new(&self.span, Option::<i32>::None)
        )
    }
}

/// Predicate can only be performed on numeric types.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PredicateOnNonNumberType {
    /// The span of the predicate.  pub span: Span,
    pub span: Span,

    /// The type of the operand.
    pub found: Type,
}

impl Display for PredicateOnNonNumberType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = format!(
            "predicate can only be performed on numeric types, but found type `{}`",
            self.found
        );

        write!(
            f,
            "{}\n{}",
            Message::new(Severity::Error, message),
            SourceCodeDisplay::new(&self.span, Option::<i32>::None)
        )
    }
}

/// Logical operation can only be performed on boolean types.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LogicalOperationOnNonBooleanType {
    /// The span of the logical operation.
    pub span: Span,

    /// The type of the operand.
    pub found: Type,
}

impl Display for LogicalOperationOnNonBooleanType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = format!(
            "logical operation can only be performed on boolean types, but found type `{}`",
            self.found
        );

        write!(
            f,
            "{}\n{}",
            Message::new(Severity::Error, message),
            SourceCodeDisplay::new(&self.span, Option::<i32>::None)
        )
    }
}

/// Contains all the possible errors that can occur during semantic analysis.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Error {
    BreakOutsideOfLoop(BreakOutsideOfLoop),
    ContinueOutsideOfLoop(ContinueOutsideOfLoop),
    TypeMismatch(TypeMismatch),
    FunctionNotFound(FunctionNotFound),
    ArgumentCountMismatch(ArgumentCountMismatch),
    VariableNotFound(VariableNotFound),
    TooLargeNumeric(TooLargeNumeric),
    FunctionMainTakeNoArgument(FunctionMainTakeNoArgument),
    FunctionRedeclaration(FunctionRedeclaration),
    NullTypeCasting(NullTypeCasting),
    ArithmeticOnNonNumberType(ArithmeticOnNonNumberType),
    AssignToRValue(AssignToRValue),
    PredicateOnNonNumberType(PredicateOnNonNumberType),
    LogicalOperationOnNonBooleanType(LogicalOperationOnNonBooleanType),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BreakOutsideOfLoop(err) => write!(f, "{err}"),
            Self::ContinueOutsideOfLoop(err) => write!(f, "{err}"),
            Self::TypeMismatch(err) => write!(f, "{err}"),
            Self::FunctionNotFound(err) => write!(f, "{err}"),
            Self::ArgumentCountMismatch(err) => write!(f, "{err}"),
            Self::VariableNotFound(err) => write!(f, "{err}"),
            Self::TooLargeNumeric(err) => write!(f, "{err}"),
            Self::FunctionMainTakeNoArgument(err) => write!(f, "{err}"),
            Self::FunctionRedeclaration(err) => write!(f, "{err}"),
            Self::NullTypeCasting(err) => write!(f, "{err}"),
            Self::ArithmeticOnNonNumberType(err) => write!(f, "{err}"),
            Self::AssignToRValue(err) => write!(f, "{err}"),
            Self::PredicateOnNonNumberType(err) => write!(f, "{err}"),
            Self::LogicalOperationOnNonBooleanType(err) => write!(f, "{err}"),
        }
    }
}
