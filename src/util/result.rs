use std::fmt::Display;

use thiserror::Error;

use crate::context::Context;

pub type Result<T, C> = core::result::Result<T, LoxError<C>>;

#[derive(Error, Debug)]
pub enum LoxError<C: Context> {
    #[error("[ERROR(IO)] {error}")]
    IO { error: std::io::Error },
    #[error("[ERROR(SCAN)] {msg} (at {context})")]
    Scan { msg: String, context: C },
    #[error("[ERROR(PARSE)] {msg} (at {context})")]
    Parse { msg: String, context: C },
    #[error("[ERROR] {msg}")]
    Other { msg: String },
}

impl<C: Context> LoxError<C> {
    pub fn io(error: std::io::Error) -> Self {
        Self::IO { error }
    }
    pub fn scan<D: Display>(msg: D, context: C) -> Self {
        Self::Scan {
            msg: msg.to_string(),
            context,
        }
    }
    pub fn parse<D: Display>(msg: D, context: C) -> Self {
        Self::Parse {
            msg: msg.to_string(),
            context,
        }
    }
    pub fn other<D: Display>(msg: D) -> Self {
        Self::Other {
            msg: msg.to_string(),
        }
    }
}

impl<C: Context> From<std::io::Error> for LoxError<C> {
    fn from(error: std::io::Error) -> Self {
        Self::io(error)
    }
}

impl<C: Context> PartialEq for LoxError<C> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::IO { error: l_error }, Self::IO { error: r_error }) => {
                l_error.to_string() == r_error.to_string()
            }
            (
                Self::Scan {
                    msg: l_msg,
                    context: l_context,
                },
                Self::Scan {
                    msg: r_msg,
                    context: r_context,
                },
            ) => l_msg == r_msg && l_context == r_context,
            (
                Self::Parse {
                    msg: l_msg,
                    context: l_context,
                },
                Self::Parse {
                    msg: r_msg,
                    context: r_context,
                },
            ) => l_msg == r_msg && l_context == r_context,
            (Self::Other { msg: l_msg }, Self::Other { msg: r_msg }) => l_msg == r_msg,
            _ => false,
        }
    }
}

impl<C: Context> Eq for LoxError<C> {}
