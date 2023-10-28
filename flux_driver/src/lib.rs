use std::{cell::Cell, fmt::Display, fs::File, path::PathBuf, process::ExitCode};

pub use clap::Parser;
use flux_base::{
    diagnostic::Handler,
    log::Severity,
    source_file::{self, SourceFile},
};
use flux_lexical::token_stream::TokenStream;
use flux_semantic::{
    interpreter::{self, value::RValue},
    program::Program,
};
use flux_syntax::parser;

/// The arguments to the program.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, clap::Parser)]
#[clap(
    name = "flux",
    about = "Flux programming language interpreter.",
    author = "66011245@kmitl.ac.th"
)]
pub struct Argument {
    /// The input file to run the program on.
    pub file: PathBuf,

    /// Prints out the syntax tree of the program.
    #[clap(long = "dump-syntax")]
    pub dump_syntax: bool,
}

/// A struct that implements [`Handler`] but prints all the message to the standard error stream.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Printer {
    printed: Cell<bool>,
}

impl Printer {
    /// Creates a new [`Printer`].
    fn new() -> Self {
        Self {
            printed: Cell::new(false),
        }
    }

    fn has_printed(&self) -> bool { self.printed.get() }
}

impl<E: Display> Handler<E> for Printer {
    fn receive(&self, error: E) {
        eprintln!("{}", error);
        self.printed.set(true);
    }
}

/// Runs the program with the given arguments.
pub fn run(argument: Argument) -> ExitCode {
    let file = match File::open(&argument.file) {
        Ok(file) => file,
        Err(error) => {
            let msg = flux_base::log::Message::new(
                Severity::Error,
                format!("{}: {error}", argument.file.display()),
            );

            eprintln!("{msg}");
            return ExitCode::FAILURE;
        }
    };

    let source_file = match SourceFile::load(file, argument.file.clone()) {
        Ok(file) => file,
        Err(source_file::Error::IoError(error)) => {
            let msg = flux_base::log::Message::new(
                Severity::Error,
                format!("{}: {error}", argument.file.display()),
            );

            eprintln!("{msg}");
            return ExitCode::FAILURE;
        }
        Err(source_file::Error::Utf8Error(error)) => {
            let msg = flux_base::log::Message::new(
                Severity::Error,
                format!("{}: {error}", argument.file.display()),
            );

            eprintln!("{msg}");
            return ExitCode::FAILURE;
        }
    };

    let printer = Printer::new();

    // token stream
    let token_stream = TokenStream::tokenize(&source_file, &printer);

    // early exit
    if printer.has_printed() {
        return ExitCode::FAILURE;
    }

    // parser
    let mut parser = parser::Parser::new(&token_stream);
    let result = parser.parse_program(&printer);

    // early exit
    let (Some(program), false) = (result, printer.has_printed()) else {
        return ExitCode::FAILURE;
    };

    if argument.dump_syntax {
        println!("{:#?}", program);
        return ExitCode::SUCCESS;
    }

    let program = match Program::new(program, &printer) {
        Ok(program) => program,
        Err(error) => {
            let msg = flux_base::log::Message::new(
                Severity::Error,
                format!("{}: {error}", argument.file.display()),
            );

            eprintln!("{msg}");

            return ExitCode::FAILURE;
        }
    };

    let empty_args: [(String, RValue); 0] = [];

    interpreter::interpret(empty_args.into_iter(), program.main(), &program, &printer);

    if printer.has_printed() {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
