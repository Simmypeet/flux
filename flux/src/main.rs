use std::process::ExitCode;

use flux_driver::{Argument, Parser};

fn main() -> ExitCode {
    let argument = Argument::parse();
    flux_driver::run(argument)
}
