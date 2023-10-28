//! This crate implements semantic analysis phase of the interpreter.

#![deny(
    missing_debug_implementations,
    missing_copy_implementations,
    missing_docs,
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    rustdoc::broken_intra_doc_links,
    clippy::missing_errors_doc
)]
#![allow(clippy::missing_panics_doc, clippy::missing_const_for_fn)]

pub mod arena;
pub mod control_flow_graph;
pub mod error;
pub mod interpreter;
pub mod program;
