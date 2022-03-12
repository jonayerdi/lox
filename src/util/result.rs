use std::fmt::Display;

use thiserror::Error;

use crate::position::Position;

pub type Result<T> = core::result::Result<T, LoxError>;

#[derive(Error, Debug)]
pub enum LoxError {
    #[error("[ERROR(IO)] {error}")]
    IO { error: std::io::Error },
    #[error("[ERROR(SCAN)] {msg} (at {position})")]
    Scan { msg: String, position: Position },
    #[error("[ERROR(PARSE)] {msg} (at {position})")]
    Parse { msg: String, position: Position },
    #[error("[ERROR] {msg}")]
    Other { msg: String },
}

impl LoxError {
    pub fn io(error: std::io::Error) -> Self {
        Self::IO { error }
    }
    pub fn scan<D: Display>(msg: D, position: Position) -> Self {
        Self::Scan {
            msg: msg.to_string(),
            position,
        }
    }
    pub fn parse<D: Display>(msg: D, position: Position) -> Self {
        Self::Parse {
            msg: msg.to_string(),
            position,
        }
    }
    pub fn other<D: Display>(msg: D) -> Self {
        Self::Other {
            msg: msg.to_string(),
        }
    }
}

impl From<std::io::Error> for LoxError {
    fn from(error: std::io::Error) -> Self {
        Self::io(error)
    }
}

impl PartialEq for LoxError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::IO { error: l_error }, Self::IO { error: r_error }) => {
                l_error.to_string() == r_error.to_string()
            }
            (
                Self::Scan {
                    msg: l_msg,
                    position: l_position,
                },
                Self::Scan {
                    msg: r_msg,
                    position: r_position,
                },
            ) => l_msg == r_msg && l_position == r_position,
            (
                Self::Parse {
                    msg: l_msg,
                    position: l_position,
                },
                Self::Parse {
                    msg: r_msg,
                    position: r_position,
                },
            ) => l_msg == r_msg && l_position == r_position,
            (Self::Other { msg: l_msg }, Self::Other { msg: r_msg }) => l_msg == r_msg,
            _ => false,
        }
    }
}

impl Eq for LoxError {}
