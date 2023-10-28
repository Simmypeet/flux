//! Contains the definition of [`ControlFlowGraph`]

use derive_more::Deref;
use flux_base::diagnostic::Handler;
use flux_syntax::syntax_tree::statement::{Conditional, Statement, While};
use getset::{CopyGetters, Getters};

use self::instruction::{ConditionalJump, Instruction, VariableDeclaration};
use crate::{
    arena::{Arena, ID},
    error::{BreakOutsideOfLoop, ContinueOutsideOfLoop, Error},
};

pub mod instruction;

/// Represents the control flow graph of a function.
///
/// The list of statements are represented as a series of basic blocks that are connected to each
/// other by jumps.
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters, Deref)]
pub struct ControlFlowGraph {
    /// The list of basic blocks contained in the control flow graph.
    #[deref]
    basic_blocks: Arena<BasicBlock>,

    /// The entry basic block of the control flow graph.
    #[get_copy = "pub"]
    entry_id: ID<BasicBlock>,
}

impl ControlFlowGraph {
    #[must_use]
    fn new() -> Self {
        let mut basic_blocks = Arena::new();

        let entry_id = basic_blocks.insert(BasicBlock {
            instructions: Vec::new(),
        });

        Self {
            basic_blocks,
            entry_id,
        }
    }

    /// Gets the entry basic block of the control flow graph.
    #[must_use]
    pub fn entry(&self) -> &BasicBlock { &self.basic_blocks[self.entry_id] }

    /// Lowers the list of statement syntax tree nodes into a control flow graph.
    pub fn lower(
        statements: impl Iterator<Item = Statement>,
        handler: &dyn Handler<Error>,
    ) -> Self {
        let mut context = LoweringContext::new();
        context.insert_scope_push(context.current_basic_block, Scope::Regular);

        for statement in statements {
            context.lower(statement, handler);
        }

        context.insert_scope_pop(context.current_basic_block);

        context.control_flow_graph
    }
}

/// Contains a list of instructions that are executed sequentially.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct BasicBlock {
    /// List of instructions to execute.
    #[get = "pub"]
    instructions: Vec<Instruction>,
}

impl BasicBlock {
    fn push(&mut self, instruction: Instruction) {
        if !matches!(
            self.instructions.last(),
            Some(
                Instruction::Return(..) | Instruction::ConditionalJump(..) | Instruction::Jump(..),
            ),
        ) {
            self.instructions.push(instruction);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Scope {
    Regular,
    While(WhileInfo),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct WhileInfo {
    header_id: ID<BasicBlock>,
    body_id: ID<BasicBlock>,
    exit_id: ID<BasicBlock>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct LoweringContext {
    control_flow_graph: ControlFlowGraph,
    current_basic_block: ID<BasicBlock>,
    scope_stack: Vec<Scope>,
}

impl LoweringContext {
    fn new() -> Self {
        let control_flow_graph = ControlFlowGraph::new();
        let current_basic_block = control_flow_graph.entry_id;
        let scope_stack = Vec::new();

        Self {
            control_flow_graph,
            current_basic_block,
            scope_stack,
        }
    }

    fn lower(&mut self, statement: Statement, handler: &dyn Handler<Error>) {
        match statement {
            Statement::VariableDeclaration(variable_declaration) => {
                let instruction = Instruction::VariableDeclaration(VariableDeclaration {
                    name: variable_declaration.identifier().span.str().to_owned(),
                    expression: variable_declaration.dissolve().2.map(|x| x.dissolve().1),
                });

                self.current_basic_block_mut().push(instruction);
            }
            Statement::Semi(expression) => {
                let instruction = Instruction::Evalueate(expression.dissolve().0);
                self.current_basic_block_mut().push(instruction);
            }
            Statement::While(while_statement) => self.lower_while(while_statement, handler),
            Statement::Block(block) => {
                self.insert_scope_push(self.current_basic_block, Scope::Regular);

                for statement in block.dissolve().1 {
                    self.lower(statement, handler);
                }

                self.insert_scope_pop(self.current_basic_block);
            }
            Statement::Conditional(conditional) => {
                self.lower_conditional(conditional, handler);
            }
            Statement::Return(return_statement) => {
                let expression = return_statement.dissolve().1;

                self.current_basic_block_mut()
                    .push(Instruction::Return(expression));
            }

            Statement::Continue(continue_statement) => {
                let Some((scope_index, header_id)) =
                    self.scope_stack.iter().rev().enumerate().find_map(
                        |(index, scope)| match scope {
                            Scope::While(while_info) => Some((index, while_info.header_id)),
                            Scope::Regular => None,
                        },
                    )
                else {
                    handler.receive(Error::ContinueOutsideOfLoop(ContinueOutsideOfLoop {
                        continue_statement,
                    }));
                    return;
                };

                let pop_count = self.scope_stack.len() - scope_index;

                for _ in 0..pop_count {
                    self.current_basic_block_mut().push(Instruction::ScopePop);
                }

                self.current_basic_block_mut()
                    .push(Instruction::Jump(header_id));
            }
            Statement::Break(break_statement) => {
                let Some((scope_index, exit_id)) =
                    self.scope_stack.iter().rev().enumerate().find_map(
                        |(index, scope)| match scope {
                            Scope::While(while_info) => Some((index, while_info.exit_id)),
                            Scope::Regular => None,
                        },
                    )
                else {
                    handler.receive(Error::BreakOutsideOfLoop(BreakOutsideOfLoop {
                        break_statement,
                    }));
                    return;
                };

                let pop_count = self.scope_stack.len() - scope_index;

                for _ in 0..pop_count {
                    self.current_basic_block_mut().push(Instruction::ScopePop);
                }

                self.current_basic_block_mut()
                    .push(Instruction::Jump(exit_id));
            }
            Statement::Print(expression) => {
                let instruction = Instruction::Print(expression.dissolve().1);
                self.current_basic_block_mut().push(instruction);
            }
        }
    }

    fn insert_scope_push(&mut self, basic_block_id: ID<BasicBlock>, scope: Scope) {
        self.control_flow_graph.basic_blocks[basic_block_id].push(Instruction::ScopePush);
        self.scope_stack.push(scope);
    }

    fn insert_scope_pop(&mut self, basic_block_id: ID<BasicBlock>) {
        self.control_flow_graph.basic_blocks[basic_block_id].push(Instruction::ScopePop);
        self.scope_stack.pop();
    }

    fn current_basic_block_mut(&mut self) -> &mut BasicBlock {
        &mut self.control_flow_graph.basic_blocks[self.current_basic_block]
    }

    fn lower_conditional(&mut self, conditional: Conditional, handler: &dyn Handler<Error>) {
        let (_, condition, body, else_body) = conditional.dissolve();

        let then_id = self.control_flow_graph.basic_blocks.insert(BasicBlock {
            instructions: Vec::new(),
        });

        if let Some(else_body) = else_body {
            let else_id = self.control_flow_graph.basic_blocks.insert(BasicBlock {
                instructions: Vec::new(),
            });
            let exit_id = self.control_flow_graph.basic_blocks.insert(BasicBlock {
                instructions: Vec::new(),
            });

            // conditional jump between then and else
            self.current_basic_block_mut()
                .push(Instruction::ConditionalJump(ConditionalJump {
                    condition: condition.dissolve().1,
                    true_block: then_id,
                    false_block: else_id,
                }));

            // change current basic block to then
            self.current_basic_block = then_id;

            // add new scope
            self.insert_scope_push(then_id, Scope::Regular);

            self.lower(*body, handler);

            // remove scope
            self.insert_scope_pop(self.current_basic_block);

            // from then to exit
            self.current_basic_block_mut()
                .push(Instruction::Jump(exit_id));

            // change current basic block to else
            self.current_basic_block = else_id;

            // add new scope
            self.insert_scope_push(else_id, Scope::Regular);

            self.lower(*else_body.dissolve().1, handler);

            // remove scope
            self.insert_scope_pop(self.current_basic_block);

            // from else to exit
            self.current_basic_block_mut()
                .push(Instruction::Jump(exit_id));

            // change current basic block to exit
            self.current_basic_block = exit_id;
        } else {
            let exit_id = self.control_flow_graph.basic_blocks.insert(BasicBlock {
                instructions: Vec::new(),
            });

            // conditional jump between then and exit
            self.current_basic_block_mut()
                .push(Instruction::ConditionalJump(ConditionalJump {
                    condition: condition.dissolve().1,
                    true_block: then_id,
                    false_block: exit_id,
                }));

            // change current basic block to then
            self.current_basic_block = then_id;

            // add new scope
            self.insert_scope_push(then_id, Scope::Regular);

            self.lower(*body, handler);

            // remove scope
            self.insert_scope_pop(self.current_basic_block);

            // from then to exit
            self.current_basic_block_mut()
                .push(Instruction::Jump(exit_id));

            // change current basic block to exit
            self.current_basic_block = exit_id;
        }
    }

    fn lower_while(&mut self, while_statement: While, handler: &dyn Handler<Error>) {
        let (_, condition, body) = while_statement.dissolve();
        let condition = condition.dissolve().1;

        let current_basic_block = self.current_basic_block;

        let header_id = self.control_flow_graph.basic_blocks.insert(BasicBlock {
            instructions: Vec::new(),
        });
        let body_id = self.control_flow_graph.basic_blocks.insert(BasicBlock {
            instructions: Vec::new(),
        });
        let exit_id = self.control_flow_graph.basic_blocks.insert(BasicBlock {
            instructions: Vec::new(),
        });

        // from current basic block to header
        self.control_flow_graph.basic_blocks[current_basic_block]
            .push(Instruction::Jump(header_id));

        // from header to body
        self.control_flow_graph.basic_blocks[header_id].push(Instruction::ConditionalJump(
            ConditionalJump {
                condition,
                true_block: body_id,
                false_block: exit_id,
            },
        ));

        // change current basic block to body
        self.current_basic_block = body_id;

        // add new scope
        self.insert_scope_push(
            body_id,
            Scope::While(WhileInfo {
                header_id,
                body_id,
                exit_id,
            }),
        );

        self.lower(*body, handler);

        // remove scope
        self.insert_scope_pop(self.current_basic_block);

        // from body to header
        self.current_basic_block_mut()
            .push(Instruction::Jump(header_id));

        // change current basic block to exit
        self.current_basic_block = exit_id;
    }
}

#[cfg(test)]
mod tests;
