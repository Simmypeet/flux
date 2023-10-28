use flux_base::diagnostic::Handler;
use getset::Getters;

use super::declaration::Declaration;
use crate::{
    error::Error,
    parser::{Parser, Reading},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Program {
    #[get = "pub"]
    declarations: Vec<Declaration>,
}

impl Program {
    /// Dissolves the [`Program`] into a list of [`Declaration`]s.
    #[must_use]
    pub fn dissolve(self) -> Vec<Declaration> { self.declarations }
}

impl<'a> Parser<'a> {
    /// Parses a [`Program`].
    pub fn parse_program(&mut self, handler: &dyn Handler<Error>) -> Option<Program> {
        let mut declarations = Vec::new();

        while !self.is_exhausted() {
            let result = self.parse_declaration(handler);

            #[allow(clippy::option_if_let_else)]
            if let Some(x) = result {
                declarations.push(x);
            } else {
                self.stop_at(|reading| {
                    matches!(
                        reading,
                        Reading::IntoDelimited(x) if x.punctuation == '{'
                    )
                });

                self.next_token();
            }
        }

        Some(Program { declarations })
    }
}
