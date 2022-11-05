use std::{
    cmp::Ordering,
    fmt::{Debug, Display},
};

use crate::rewind::Rewind;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position(pub usize, pub usize);

impl Default for Position {
    fn default() -> Self {
        Position(1, 1)
    }
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Position {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.0.cmp(&other.0) {
            Ordering::Equal => self.1.cmp(&other.1),
            ord => ord,
        }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Context {
    pub begin: Position,
    pub end: Position,
}

pub const NO_CONTEXT: Context = Context {
    begin: Position(0, 0),
    end: Position(0, 0),
};

impl Context {
    pub const fn new(begin: Position, end: Position) -> Self {
        Self { begin, end }
    }
    pub const fn new_maybe(begin: Option<Position>, end: Option<Position>) -> Self {
        match (begin, end) {
            (Some(begin), Some(end)) => Self { begin, end },
            _ => NO_CONTEXT,
        }
    }
}

impl Default for Context {
    fn default() -> Self {
        NO_CONTEXT
    }
}

impl Display for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            &NO_CONTEXT => write!(f, "{{unknown}}"),
            Self { begin, end } => write!(f, "{begin} : {end}"),
        }
    }
}
