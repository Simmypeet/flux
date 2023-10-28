use std::ops::DerefMut;

use derive_more::{Deref, Index, IndexMut};

use super::value::RValue;
use crate::arena::{Arena, Map, ID};

/// Represents a stack frame where variables are stored.
#[derive(Debug, Clone, PartialEq, Deref, Index, IndexMut)]
pub struct Frame {
    /// Maps from the variable name to its currently stored value.
    #[deref]
    #[index]
    #[index_mut]
    locals: Map<RValue>,
}

impl Frame {
    /// Creates a new empty stack frame
    pub fn new() -> Self { Self { locals: Map::new() } }

    /// Adds a new variable to the stack frame.
    ///
    /// If there is an existing variable with the same name, it will be overwritten.
    pub fn add_variable(&mut self, name: String, value: RValue) {
        if let Some(id) = self.locals.get_id(&name) {
            *self.locals[id].deref_mut() = value;
        } else {
            assert!(self.locals.insert(name, value).is_ok());
        }
    }
}

impl Default for Frame {
    fn default() -> Self { Self::new() }
}

/// Represents an identifier to a particular variable in the stack.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableID {
    /// The identifier of the frame where the variable is stored.
    pub frame_id: ID<Frame>,

    /// The identifier of the variable in the frame.
    pub variable_id: ID<RValue>,
}

/// Represents a stack frames of a particular function call.
#[derive(Debug, Clone, PartialEq, Deref, Index, IndexMut)]
pub struct Stack {
    #[deref]
    #[index]
    #[index_mut]
    frames: Arena<Frame>,
}

impl Stack {
    /// Creates a new empty stack.
    pub fn new() -> Self {
        Self {
            frames: Arena::new(),
        }
    }

    /// Adds a new variable into the topmost frame.
    ///
    /// # Returns
    ///
    /// Returns `true` if the variable was added successfully, `false` if there aren't any frames in
    /// the stack.
    #[must_use]
    pub fn add_variable(&mut self, name: String, value: RValue) -> bool {
        self.frames.last_mut().map_or(false, |frame| {
            frame.add_variable(name, value);
            true
        })
    }

    /// Adds a new frame to the stack.
    pub fn push_frame(&mut self) { self.frames.insert(Frame::new()); }

    /// Removes the topmost frame from the stack.
    ///
    /// # Returns
    ///
    /// Returns `true` if the frame was removed successfully, `false` if there aren't any frames in
    /// the stack.
    #[must_use]
    pub fn pop_frame(&mut self) -> bool { self.frames.pop().is_some() }

    /// Performs variable lookup by name and returns a [`VariableID`] if the variable exists.
    pub fn search_variable(&self, name: &str) -> Option<VariableID> {
        for (frame_index, frame) in self.frames.iter().enumerate().rev() {
            if frame.get_id(name).is_some() {
                return Some(VariableID {
                    frame_id: ID::new(frame_index),
                    variable_id: frame.get_id(name).unwrap(),
                });
            }
        }

        None
    }
}
