//! This crate provides the syntax tree and parser for the Flux programming language. This phase
//! is responsible for parsing the token stream into a syntax tree.

#![deny(
    missing_debug_implementations,
    missing_copy_implementations,
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    rustdoc::broken_intra_doc_links,
    clippy::missing_errors_doc
)]
#![allow(clippy::missing_panics_doc, clippy::missing_const_for_fn)]

pub mod error;
pub mod parser;
pub mod syntax_tree;
