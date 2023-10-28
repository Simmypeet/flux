//! This module provides a trait [`Input`] for representing inputs generated for property based
//! testing

use proptest::{
    prop_assert_eq,
    test_runner::{TestCaseError, TestCaseResult},
};

/// Represents an input generated for testing purposes.
pub trait Input<Output> {
    /// Verifies that the given output complies with this input.
    ///
    /// # Errors
    /// [`proptest::test_runner::TestCaseError`]: for any reason the assertion fails.
    fn assert(self, output: Output) -> TestCaseResult;
}

impl<T, U> Input<&Box<T>> for &Box<U>
where
    for<'a, 'b> &'a U: Input<&'b T>,
{
    fn assert(self, output: &Box<T>) -> TestCaseResult { self.as_ref().assert(output.as_ref()) }
}

impl<T, U> Input<Option<T>> for Option<U>
where
    U: Input<T>,
{
    fn assert(self, output: Option<T>) -> TestCaseResult {
        match (self, output) {
            (Some(input), Some(output)) => input.assert(output),
            (None, None) => Ok(()),
            (Some(_), None) => Err(TestCaseError::fail("expected Some, found None")),
            (None, Some(_)) => Err(TestCaseError::fail("expected None, found Some")),
        }
    }
}

impl<T, U> Input<&Vec<T>> for &Vec<U>
where
    for<'a, 'b> &'a U: Input<&'b T>,
{
    fn assert(self, output: &Vec<T>) -> TestCaseResult {
        prop_assert_eq!(self.len(), output.len());

        for (input, output) in self.iter().zip(output.iter()) {
            input.assert(output)?;
        }

        Ok(())
    }
}
