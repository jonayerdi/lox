use std::{error::Error, fmt::Display};

use crate::{expression::ExpressionContext, token::TokenContext};

pub type Result<T> = core::result::Result<T, LoxError>;

#[derive(Debug)]
pub enum LoxError {
    IO {
        error: std::io::Error,
    },
    Scan {
        msg: String,
        context: TokenContext,
    },
    Parse {
        msg: String,
        context: ExpressionContext,
    },
    Interpret {
        msg: String,
    },
    CLI {
        msg: String,
    },
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

impl Display for LoxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxError::IO { error } => write!(f, "[ERROR(IO)] {error}"),
            LoxError::Scan { msg, context } => write!(f, "[ERROR(SCAN)] {msg} (at {context})"),
            LoxError::Parse { msg, context } => write!(f, "[ERROR(PARSE)] {msg} (at {context})"),
            LoxError::Interpret { msg } => write!(f, "[ERROR(INTERPRET)] {msg}"),
            LoxError::CLI { msg } => write!(f, "[ERROR(CLI)] {msg}"),
        }
    }
}

impl Error for LoxError {}

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
