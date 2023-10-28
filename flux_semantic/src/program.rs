//! Contains the definition of [`Program`] containing all the information in the progra,.

use std::collections::{hash_map::Entry, HashMap};

use flux_base::{diagnostic::Handler, source_file::Span};
use flux_syntax::syntax_tree::{self, declaration::Declaration, ConnectedList};
use getset::Getters;
use thiserror::Error;

use crate::{
    control_flow_graph::ControlFlowGraph,
    error::{self, FunctionRedeclaration},
};

/// Represents a function declaration in the program.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct Function {
    /// The control flow graph of the function.
    #[get = "pub"]
    control_flow_graph: ControlFlowGraph,

    /// List of parameters of the function.
    #[get = "pub"]
    parameters: Vec<String>,

    /// The name of the function.
    #[get = "pub"]
    name: String,

    /// The span to the function identifier.
    #[get = "pub"]
    span: Span,
}

/// Represents a program.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct Program {
    /// The control flow graph of the main declaration.
    #[get = "pub"]
    main: ControlFlowGraph,

    /// The list of functions in the program.
    #[get = "pub"]
    functions_by_name: HashMap<String, Function>,
}

/// Represents an error that occurred while converting a [`syntax_tree::program::Program`] to a
/// [`Program`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error("the program has no main function.")]
    NoMainFunction,
}

impl Program {
    /// Converts [`syntax_tree::program::Program`] syntax tree to [`Program`].
    ///
    /// # Errors
    /// [`Error::NoMainFunction`] is returned if the program does not have a main function.
    pub fn new(
        syntax_tree: syntax_tree::program::Program,
        handler: &dyn Handler<error::Error>,
    ) -> Result<Self, Error> {
        let mut main: Option<syntax_tree::declaration::Function> = None;
        let mut functions: HashMap<String, Function> = HashMap::new();

        for declaration in syntax_tree.dissolve() {
            match declaration {
                Declaration::Function(declaration) => {
                    if declaration.identifier().span.str() == "main" {
                        #[allow(clippy::option_if_let_else)]
                        if let Some(main) = main.as_ref() {
                            handler.receive(error::Error::FunctionRedeclaration(
                                FunctionRedeclaration {
                                    existing_function_span: main.identifier().span.clone(),
                                    new_function_span: declaration.identifier().span.clone(),
                                },
                            ));
                        } else {
                            main = Some(declaration.clone());
                        }
                    } else {
                        match functions.entry(declaration.identifier().span.str().to_string()) {
                            Entry::Occupied(entry) => handler.receive(
                                error::Error::FunctionRedeclaration(FunctionRedeclaration {
                                    existing_function_span: entry.get().span().clone(),
                                    new_function_span: declaration.identifier().span.clone(),
                                }),
                            ),
                            Entry::Vacant(entry) => {
                                let (_, name, _, parameters, _, block) = declaration.dissolve();
                                entry.insert(Function {
                                    control_flow_graph: ControlFlowGraph::lower(
                                        block.dissolve().1.into_iter(),
                                        handler,
                                    ),
                                    parameters: parameters
                                        .iter()
                                        .flat_map(ConnectedList::elements)
                                        .map(|x| x.span.str().to_string())
                                        .collect(),
                                    name: name.span.str().to_owned(),
                                    span: name.span.clone(),
                                });
                            }
                        }
                    }
                }
            }
        }

        main.map_or_else(
            || Err(Error::NoMainFunction),
            |main| {
                Ok(Self {
                    main: ControlFlowGraph::lower(
                        main.dissolve().5.dissolve().1.into_iter(),
                        handler,
                    ),
                    functions_by_name: functions,
                })
            },
        )
    }
}
