use std::fmt::{Debug, Display};

use crate::rewind::Rewind;

pub trait Context: Default + Clone + Debug + Display + PartialEq + Eq {
    fn combine(&self, other: &Self) -> Self;
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct NoContext;

impl Display for NoContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{unknown}}")
    }
}

impl Context for NoContext {
    fn combine(&self, _other: &Self) -> Self {
        Self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position(pub usize, pub usize);

impl Default for Position {
    fn default() -> Self {
        Position(1, 1)
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "line {line}, column {column}",
            line = self.0,
            column = self.1
        )
    }
}

impl Context for Position {
    fn combine(&self, _other: &Self) -> Self {
        *self
    }
}

pub struct PositionTracker<S: Iterator<Item = char>> {
    source: Rewind<S>,
    position: Position,
    line_lengths: Vec<usize>,
}

impl<S: Iterator<Item = char>> PositionTracker<S> {
    pub fn new(source: S) -> Self {
        Self {
            source: Rewind::with_capacity(source, 8),
            position: Default::default(),
            line_lengths: Vec::with_capacity(256),
        }
    }
    pub fn position(&self) -> Position {
        self.position
    }
    pub fn rewind(&mut self, c: char) {
        if c == '\n' {
            self.position.0 -= 1;
            self.position.1 = *self.line_lengths.get(self.position.0).unwrap(); // line_lengths should always have recorded previous lines, so this should never panic
        } else {
            self.position.1 -= 1;
        }
        self.source.rewind(c);
    }
}

impl<S: Iterator<Item = char>> Iterator for PositionTracker<S> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.source.next();
        if let Some(c) = next {
            if c == '\n' {
                if self.line_lengths.len() < self.position.0 {
                    assert_eq!(self.line_lengths.len() + 1, self.position.0); // line_lengths should always have recorded previous lines, so this should never panic
                    self.line_lengths.push(self.position.1)
                }
                self.position.0 += 1;
                self.position.1 = 1;
            } else {
                self.position.1 += 1;
            }
        }
        next
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Range(pub Position, pub Position);

impl Display for Range {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{begin} : {end}", begin = self.0, end = self.1)
    }
}

impl Context for Range {
    fn combine(&self, other: &Self) -> Self {
        Range(self.0, other.1)
    }
}
