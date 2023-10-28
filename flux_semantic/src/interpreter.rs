//! Contains the main interpreter function.

use std::ops::DerefMut;

use flux_base::{
    diagnostic::Handler,
    source_file::{SourceElement, Span},
};
use flux_syntax::syntax_tree::{
    expression::{self, Binary, BinaryOperator, Expression, FunctionCall, PrefixOperator, Primary},
    ConnectedList,
};

use self::value::{RValue, Type, Value};
use crate::{
    arena::ID,
    control_flow_graph::{instruction::Instruction, BasicBlock, ControlFlowGraph},
    error::{
        ArgumentCountMismatch, ArithmeticOnNonNumberType, AssignToRValue, Error, FunctionNotFound,
        LogicalOperationOnNonBooleanType, NullTypeCasting, PredicateOnNonNumberType,
        TooLargeNumeric, TypeMismatch, VariableNotFound,
    },
    program::Program,
};

mod stack;
pub mod value;

/// Interprets the given control flow graph.
///
/// # Parameters
///
/// - `arguments`: An iterator over pairs of argument names and values.
/// - `control_flow_graph`: The control flow graph to interpret.
/// - `program`: The program environment that contains all the functions.
/// - `handler`: The diagnostic handler. This is used to report errors.
///
/// # Returns
///
/// Return `Some(value)` if the interpretation was successful with no errors, otherwise, return
/// `None`.
pub fn interpret(
    arguments: impl Iterator<Item = (String, RValue)>,
    control_flow_graph: &ControlFlowGraph,
    program: &Program,
    handler: &dyn Handler<Error>,
) -> Option<RValue> {
    let mut stack = stack::Stack::new();
    stack.push_frame();

    for (name, value) in arguments {
        assert!(
            stack.add_variable(name, value),
            "should've had a stack frame to add"
        );
    }

    let mut interpreter = Context {
        stack,
        control_flow_graph,
        program,
        current_block: control_flow_graph.entry_id(),
        current_instruction_index: 1, // every control flow graph starts with a scope push
    };

    loop {
        match interpreter.execute(handler) {
            Some(ExecuteResult::Return(value)) => return Some(value),
            Some(ExecuteResult::End) => return Some(RValue::Null),
            Some(ExecuteResult::Continue) => {
                interpreter.current_instruction_index += 1;
            }
            None => return None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Context<'a> {
    stack: stack::Stack,
    control_flow_graph: &'a ControlFlowGraph,
    program: &'a Program,
    current_block: ID<BasicBlock>,
    current_instruction_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
enum ExecuteResult {
    Return(RValue),
    End,
    Continue,
}

impl Context<'_> {
    /// Executes the current instruction.
    #[must_use]
    fn execute(&mut self, handler: &dyn Handler<Error>) -> Option<ExecuteResult> {
        let Some(instruction) = self.control_flow_graph[self.current_block]
            .instructions()
            .get(self.current_instruction_index)
        else {
            return Some(ExecuteResult::End);
        };

        match instruction {
            Instruction::ScopePush => {
                self.stack.push_frame();
            }
            Instruction::ScopePop => {
                assert!(self.stack.pop_frame(), "should've a frame to pop");
            }
            Instruction::Evalueate(expression) => {
                self.evluate_normalized(expression, handler)?;
            }
            Instruction::VariableDeclaration(variable_declaration) => {
                let value = if let Some(initializer) = &variable_declaration.expression {
                    self.evluate_normalized(initializer, handler)?
                } else {
                    RValue::Null
                };

                assert!(
                    self.stack
                        .add_variable(variable_declaration.name.clone(), value),
                    "should've had a frame to declare a variable"
                );
            }
            Instruction::Jump(jump) => {
                self.current_block = *jump;
                self.current_instruction_index = 0;

                return self.execute(handler);
            }
            Instruction::Return(return_inst) => {
                let value = if let Some(expression) = return_inst {
                    self.evluate_normalized(expression, handler)?
                } else {
                    RValue::Null
                };

                return Some(ExecuteResult::Return(value));
            }
            Instruction::ConditionalJump(cond_jump) => {
                let cond = match self.evluate_normalized(&cond_jump.condition, handler)? {
                    RValue::Bool(val) => val,
                    found => {
                        handler.receive(Error::TypeMismatch(TypeMismatch {
                            expected: Type::Bool,
                            found: found.r#type(),
                            expression_span: cond_jump.condition.span(),
                        }));
                        return None;
                    }
                };

                self.current_block = if cond {
                    cond_jump.true_block
                } else {
                    cond_jump.false_block
                };
                self.current_instruction_index = 0;

                return self.execute(handler);
            }
            Instruction::Print(print_statement) => {
                let value = self.evluate_normalized(print_statement, handler)?;

                match value {
                    RValue::Int64(num) => println!("{num}"),
                    RValue::Float64(num) => println!("{num}"),
                    RValue::Bool(bool) => println!("{bool}"),
                    RValue::Null => println!("null"),
                }
            }
        }

        Some(ExecuteResult::Continue)
    }

    fn evluate_normalized(
        &mut self,
        expression: &Expression,
        handler: &dyn Handler<Error>,
    ) -> Option<RValue> {
        let value = self.evaluate(expression, handler)?;
        Some(self.normalize(value))
    }

    /// Evaluates the given expression.
    fn evaluate(&mut self, expression: &Expression, handler: &dyn Handler<Error>) -> Option<Value> {
        match expression {
            Expression::Primary(primary) => self.evaluate_primary(primary, handler),
            Expression::Binary(binary) => self.evalute_binary(binary, handler),
        }
    }

    #[allow(
        clippy::too_many_lines,
        clippy::cast_precision_loss,
        clippy::cast_lossless,
        clippy::cast_possible_truncation
    )]
    fn evaluate_primary(
        &mut self,
        expression: &Primary,
        handler: &dyn Handler<Error>,
    ) -> Option<Value> {
        match expression {
            Primary::Identifier(ident) => {
                let name = ident.span.str();

                self.stack.search_variable(name).map_or_else(
                    || {
                        handler.receive(Error::VariableNotFound(VariableNotFound {
                            span: ident.span.clone(),
                        }));

                        None
                    },
                    |variable_ref| Some(Value::LValue(variable_ref)),
                )
            }
            Primary::Null(..) => Some(Value::RValue(RValue::Null)),
            Primary::Numeric(numeric) => {
                let val = if numeric.decimal().is_some() {
                    numeric
                        .span()
                        .str()
                        .parse::<f64>()
                        .ok()
                        .map(RValue::Float64)
                } else {
                    numeric.span().str().parse::<i64>().ok().map(RValue::Int64)
                };

                val.map_or_else(
                    || {
                        handler.receive(Error::TooLargeNumeric(TooLargeNumeric {
                            span: numeric.span(),
                        }));

                        None
                    },
                    |val| Some(Value::RValue(val)),
                )
            }
            Primary::Prefix(prefix) => {
                let operand = self.evaluate_primary(prefix.operand(), handler)?;
                let operand = self.normalize(operand);

                match (prefix.operator(), operand) {
                    (PrefixOperator::LogicalNot(_), RValue::Bool(val)) => {
                        Some(Value::RValue(RValue::Bool(!val)))
                    }
                    (PrefixOperator::Negate(_), RValue::Int64(val)) => {
                        Some(Value::RValue(RValue::Int64(-val)))
                    }
                    (PrefixOperator::Negate(_), RValue::Float64(val)) => {
                        Some(Value::RValue(RValue::Float64(-val)))
                    }

                    (PrefixOperator::TypeCast(type_cast), RValue::Int64(val)) => {
                        Some(match type_cast.r#type() {
                            expression::Type::Float64(_) => {
                                Value::RValue(RValue::Float64(val as f64))
                            }
                            expression::Type::Int64(_) => Value::RValue(RValue::Int64(val)),
                            expression::Type::Bool(_) => Value::RValue(RValue::Bool(val != 0)),
                        })
                    }
                    (PrefixOperator::TypeCast(type_cast), RValue::Float64(val)) => {
                        Some(match type_cast.r#type() {
                            expression::Type::Float64(_) => Value::RValue(RValue::Float64(val)),
                            expression::Type::Int64(_) => Value::RValue(RValue::Int64(val as i64)),
                            expression::Type::Bool(_) => Value::RValue(RValue::Bool(val != 0.)),
                        })
                    }
                    (PrefixOperator::TypeCast(type_cast), RValue::Bool(val)) => {
                        Some(match type_cast.r#type() {
                            expression::Type::Float64(_) => {
                                Value::RValue(RValue::Float64(if val { 1. } else { 0. }))
                            }
                            expression::Type::Int64(_) => Value::RValue(RValue::Int64(val as i64)),
                            expression::Type::Bool(_) => Value::RValue(RValue::Bool(val)),
                        })
                    }
                    (PrefixOperator::TypeCast(..), RValue::Null) => {
                        handler.receive(Error::NullTypeCasting(NullTypeCasting {
                            span: prefix.operand().span(),
                        }));
                        None
                    }

                    (PrefixOperator::LogicalNot(_), found) => {
                        handler.receive(Error::TypeMismatch(TypeMismatch {
                            expected: Type::Bool,
                            found: found.r#type(),
                            expression_span: prefix.operand().span(),
                        }));
                        None
                    }
                    (PrefixOperator::Negate(_), found) => {
                        handler.receive(Error::TypeMismatch(TypeMismatch {
                            expected: Type::Int64,
                            found: found.r#type(),
                            expression_span: prefix.operand().span(),
                        }));
                        None
                    }
                }
            }
            Primary::Parenthesized(parenthesized) => {
                self.evaluate(parenthesized.expression(), handler)
            }
            Primary::FunctionCall(function_call) => self
                .evaluate_function_call(function_call, handler)
                .map(Value::RValue),
            Primary::True(_) => Some(Value::RValue(RValue::Bool(true))),
            Primary::False(_) => Some(Value::RValue(RValue::Bool(false))),
        }
    }

    /// Normalizes the [`Value`] which can be either rvalue or lvalue into an rvalue.
    fn normalize(&mut self, value: Value) -> RValue {
        match value {
            Value::LValue(variable_id) => {
                *self.stack[variable_id.frame_id][variable_id.variable_id]
            }
            Value::RValue(val) => val,
        }
    }

    #[allow(clippy::too_many_lines)]
    fn evalute_binary(
        &mut self,
        expression: &Binary,
        handler: &dyn Handler<Error>,
    ) -> Option<Value> {
        let lhs_value = self.evaluate(expression.left_operand(), handler)?;
        let rhs_value = self.evaluate(expression.right_operand(), handler)?;

        match expression.operator() {
            BinaryOperator::Equal(_, _) | BinaryOperator::NotEqual(_, _) => {
                let invert = matches!(expression.operator(), BinaryOperator::NotEqual(_, _));

                return Self::equals(
                    (self.normalize(lhs_value), expression.left_operand().span()),
                    (self.normalize(rhs_value), expression.right_operand().span()),
                    handler,
                )
                .map(|x| Value::RValue(RValue::Bool(if invert { !x } else { x })));
            }
            BinaryOperator::Assign(_) => {
                let normalized = self.normalize(rhs_value);
                return self.assign(lhs_value, normalized, expression.span(), handler);
            }
            _ => (),
        }

        // type must equals
        if self.normalize(lhs_value).r#type() != self.normalize(rhs_value).r#type() {
            handler.receive(Error::TypeMismatch(TypeMismatch {
                expected: self.normalize(lhs_value).r#type(),
                found: self.normalize(rhs_value).r#type(),
                expression_span: expression.right_operand().span(),
            }));

            return None;
        }

        match expression.operator() {
            BinaryOperator::Add(_)
            | BinaryOperator::Subtract(_)
            | BinaryOperator::Multiply(_)
            | BinaryOperator::Divide(_)
            | BinaryOperator::Modulo(_) => Self::apply_arithmetic(
                self.normalize(lhs_value),
                self.normalize(rhs_value),
                expression.span(),
                &match expression.operator() {
                    BinaryOperator::Add(_) => Arithmetic::Add,
                    BinaryOperator::Subtract(_) => Arithmetic::Subtract,
                    BinaryOperator::Multiply(_) => Arithmetic::Multiply,
                    BinaryOperator::Divide(_) => Arithmetic::Divide,
                    BinaryOperator::Modulo(_) => Arithmetic::Modulo,
                    _ => unreachable!(),
                },
                handler,
            )
            .map(Value::RValue),

            BinaryOperator::CompoundAdd(_, _)
            | BinaryOperator::CompoundSubtract(_, _)
            | BinaryOperator::CompoundMultiply(_, _)
            | BinaryOperator::CompoundDivide(_, _)
            | BinaryOperator::CompoundModulo(_, _) => {
                let value = Self::apply_arithmetic(
                    self.normalize(lhs_value),
                    self.normalize(rhs_value),
                    expression.span(),
                    &match expression.operator() {
                        BinaryOperator::CompoundAdd(_, _) => Arithmetic::Add,
                        BinaryOperator::CompoundSubtract(_, _) => Arithmetic::Subtract,
                        BinaryOperator::CompoundMultiply(_, _) => Arithmetic::Multiply,
                        BinaryOperator::CompoundDivide(_, _) => Arithmetic::Divide,
                        BinaryOperator::CompoundModulo(_, _) => Arithmetic::Modulo,
                        _ => unreachable!(),
                    },
                    handler,
                );

                self.assign(lhs_value, value?, expression.span(), handler)
            }

            // predicate
            BinaryOperator::LessThan(_)
            | BinaryOperator::LessThanOrEqual(_, _)
            | BinaryOperator::GreaterThan(_)
            | BinaryOperator::GreaterThanOrEqual(_, _) => {
                let predicate = match expression.operator() {
                    BinaryOperator::LessThan(_) => Predicate::Lesser,
                    BinaryOperator::LessThanOrEqual(_, _) => Predicate::LesserOrEqual,
                    BinaryOperator::GreaterThan(_) => Predicate::Greater,
                    BinaryOperator::GreaterThanOrEqual(_, _) => Predicate::GreaterOrEqual,
                    _ => unreachable!(),
                };

                let lhs_normalized = self.normalize(lhs_value);
                let rhs_normalized = self.normalize(rhs_value);

                if lhs_normalized.r#type() != Type::Float64
                    && lhs_normalized.r#type() != Type::Int64
                {
                    handler.receive(Error::PredicateOnNonNumberType(PredicateOnNonNumberType {
                        span: expression.span(),
                        found: lhs_normalized.r#type(),
                    }));
                    return None;
                }

                match (lhs_normalized, rhs_normalized) {
                    (RValue::Int64(lhs), RValue::Int64(rhs)) => {
                        Some(Value::RValue(RValue::Bool(predicate.apply(&lhs, &rhs))))
                    }
                    (RValue::Float64(lhs), RValue::Float64(rhs)) => {
                        Some(Value::RValue(RValue::Bool(predicate.apply(&lhs, &rhs))))
                    }
                    _ => unreachable!(),
                }
            }

            // logic operators
            BinaryOperator::LogicalAnd(_) | BinaryOperator::LogicalOr(_) => {
                let lhs_normalized = self.normalize(lhs_value);
                let rhs_normalized = self.normalize(rhs_value);

                if lhs_normalized.r#type() != Type::Bool {
                    handler.receive(Error::LogicalOperationOnNonBooleanType(
                        LogicalOperationOnNonBooleanType {
                            span: expression.span(),
                            found: lhs_normalized.r#type(),
                        },
                    ));
                    return None;
                }

                let logical_operator = match expression.operator() {
                    BinaryOperator::LogicalAnd(_) => LogicalOperator::And,
                    BinaryOperator::LogicalOr(_) => LogicalOperator::Or,
                    _ => unreachable!(),
                };

                match (lhs_normalized, rhs_normalized) {
                    (RValue::Bool(lhs), RValue::Bool(rhs)) => Some(Value::RValue(RValue::Bool(
                        logical_operator.apply(lhs, rhs),
                    ))),
                    _ => unreachable!(),
                }
            }

            _ => unreachable!(),
        }
    }

    fn assign(
        &mut self,
        lhs_value: Value,
        value: RValue,
        expression_span: Span,
        handler: &dyn Handler<Error>,
    ) -> Option<Value> {
        let Value::LValue(variable_id) = lhs_value else {
            handler.receive(Error::AssignToRValue(AssignToRValue {
                span: expression_span,
            }));
            return None;
        };

        *self.stack[variable_id.frame_id][variable_id.variable_id].deref_mut() = value;

        Some(Value::LValue(variable_id))
    }

    #[allow(clippy::float_cmp, clippy::cast_precision_loss)]
    fn equals(
        (lhs_value, _): (RValue, Span),
        (rhs_value, rhs_span): (RValue, Span),
        handler: &dyn Handler<Error>,
    ) -> Option<bool> {
        match (lhs_value, rhs_value) {
            // early catch null cases
            (RValue::Null, RValue::Null) => Some(true),
            (RValue::Null, _) | (_, RValue::Null) => Some(false),

            // compare bools
            (RValue::Bool(lhs), RValue::Bool(rhs)) => Some(lhs == rhs),

            // compare numbers
            (RValue::Int64(lhs), RValue::Int64(rhs)) => Some(lhs == rhs),
            (RValue::Float64(lhs), RValue::Float64(rhs)) => Some(lhs == rhs),

            // different types error
            (lhs, rhs) => {
                handler.receive(Error::TypeMismatch(TypeMismatch {
                    expected: lhs.r#type(),
                    found: rhs.r#type(),
                    expression_span: rhs_span,
                }));

                None
            }
        }
    }

    fn apply_arithmetic(
        lhs: RValue,
        rhs: RValue,
        expression_span: Span,
        arithmetic: &Arithmetic,
        handler: &dyn Handler<Error>,
    ) -> Option<RValue> {
        // PRECONDITIONS
        debug_assert_eq!(lhs.r#type(), rhs.r#type());
        debug_assert!(lhs.r#type() == Type::Int64 || lhs.r#type() == Type::Float64);

        // only on numbers
        if lhs.r#type() != Type::Int64 && rhs.r#type() != Type::Float64 {
            handler.receive(Error::ArithmeticOnNonNumberType(
                ArithmeticOnNonNumberType {
                    span: expression_span,
                    found: lhs.r#type(),
                },
            ));

            return None;
        }

        match (lhs, rhs) {
            (RValue::Float64(lhs), RValue::Float64(rhs)) => {
                Some(RValue::Float64(arithmetic.apply(lhs, rhs)))
            }

            (RValue::Int64(lhs), RValue::Int64(rhs)) => {
                Some(RValue::Int64(arithmetic.apply(lhs, rhs)))
            }

            _ => unreachable!(),
        }
    }

    fn evaluate_function_call(
        &mut self,
        expression: &FunctionCall,
        handler: &dyn Handler<Error>,
    ) -> Option<RValue> {
        let Some(func) = self
            .program
            .functions_by_name()
            .get(expression.identifier().span.str())
        else {
            handler.receive(Error::FunctionNotFound(FunctionNotFound {
                span: expression.identifier().span.clone(),
            }));

            return None;
        };

        // check if the number of arguments matches
        let argument_count = expression
            .arguments()
            .as_ref()
            .map_or(0, ConnectedList::len);
        if func.parameters().len() != argument_count {
            handler.receive(Error::ArgumentCountMismatch(ArgumentCountMismatch {
                expected: func.parameters().len(),
                found: argument_count,
                span: expression.span(),
            }));

            return None;
        }

        let mut arguments = Vec::new();

        for argument in expression
            .arguments()
            .iter()
            .flat_map(ConnectedList::elements)
        {
            arguments.push(self.evluate_normalized(argument, handler)?);
        }

        interpret(
            func.parameters().iter().cloned().zip(arguments.into_iter()),
            func.control_flow_graph(),
            self.program,
            handler,
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Arithmetic {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
}

impl Arithmetic {
    pub fn apply<T>(&self, lhs: T, rhs: T) -> T
    where
        T: std::ops::Add<Output = T>
            + std::ops::Sub<Output = T>
            + std::ops::Mul<Output = T>
            + std::ops::Div<Output = T>
            + std::ops::Rem<Output = T>,
    {
        match self {
            Self::Add => lhs + rhs,
            Self::Subtract => lhs - rhs,
            Self::Multiply => lhs * rhs,
            Self::Divide => lhs / rhs,
            Self::Modulo => lhs % rhs,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Predicate {
    Greater,
    Lesser,
    GreaterOrEqual,
    LesserOrEqual,
}

impl Predicate {
    pub fn apply<T>(self, lhs: &T, rhs: &T) -> bool
    where
        T: std::cmp::PartialOrd,
    {
        match self {
            Self::Greater => lhs > rhs,
            Self::Lesser => lhs < rhs,
            Self::GreaterOrEqual => lhs >= rhs,
            Self::LesserOrEqual => lhs <= rhs,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum LogicalOperator {
    And,
    Or,
}

impl LogicalOperator {
    pub fn apply(self, lhs: bool, rhs: bool) -> bool {
        match self {
            Self::And => lhs && rhs,
            Self::Or => lhs || rhs,
        }
    }
}
