use std::fmt::Display;

use thiserror::Error;

use crate::{expression::ExpressionContext, token::TokenContext};

pub type Result<T> = core::result::Result<T, LoxError>;

#[derive(Error, Debug)]
pub enum LoxError {
    #[error("[ERROR(IO)] {error}")]
    IO { error: std::io::Error },
    #[error("[ERROR(SCAN)] {msg} (at {context})")]
    Scan { msg: String, context: TokenContext },
    #[error("[ERROR(PARSE)] {msg} (at {context})")]
    Parse {
        msg: String,
        context: ExpressionContext,
    },
    #[error("[ERROR(INTERPRET)] {msg}")]
    Interpret { msg: String },
    #[error("[ERROR(CLI)] {msg}")]
    CLI { msg: String },
}

impl LoxError {
    pub fn io(error: std::io::Error) -> Self {
        Self::IO { error }
    }
    pub fn scan<D: Display>(msg: D, context: TokenContext) -> Self {
        Self::Scan {
            msg: msg.to_string(),
            context,
        }
    }
    pub fn parse<D: Display>(msg: D, context: ExpressionContext) -> Self {
        Self::Parse {
            msg: msg.to_string(),
            context,
        }
    }
    pub fn interpret<D: Display>(msg: D) -> Self {
        Self::Interpret {
            msg: msg.to_string(),
        }
    }
    pub fn cli<D: Display>(msg: D) -> Self {
        Self::CLI {
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
            (Self::Interpret { msg: l_msg }, Self::Interpret { msg: r_msg }) => l_msg == r_msg,
            (Self::CLI { msg: l_msg }, Self::CLI { msg: r_msg }) => l_msg == r_msg,
            _ => false,
        }
    }
}

impl Eq for LoxError {}
