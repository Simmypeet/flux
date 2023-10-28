//! Contains the definition of [`Value`]

use derive_more::Display;

use super::stack::VariableID;

/// Represents a type of a particular value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
#[allow(missing_docs)]
pub enum Type {
    #[display = "int64"]
    Int64,

    #[display = "float64"]
    Float64,

    #[display = "bool"]
    Bool,

    #[display = "null"]
    Null,
}

/// Represents a unit of value in the interpretation process.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Value {
    /// Represents a value that is yielded from a particular variable.
    LValue(VariableID),

    /// Represents a final value that doesn't have any particular memory location associated with
    /// it.
    RValue(RValue),
}

/// Represents a value in the interpreter.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[allow(missing_docs, clippy::module_name_repetitions)]
pub enum RValue {
    Int64(i64),
    Float64(f64),
    Bool(bool),
    Null,
}

impl RValue {
    /// Gets the type of the value.
    #[must_use]
    pub fn r#type(&self) -> Type {
        match self {
            Self::Int64(_) => Type::Int64,
            Self::Float64(_) => Type::Float64,
            Self::Bool(_) => Type::Bool,
            Self::Null => Type::Null,
        }
    }
}
