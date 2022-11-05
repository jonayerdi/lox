use std::fmt::Display;

use crate::context::Context;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenValue {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals
    Identifier(String),
    String(String),
    Number(String),
    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

impl TokenValue {
    pub fn token_type(&self) -> &'static str {
        match self {
            TokenValue::LeftParen => "grouping",
            TokenValue::RightParen => "grouping",
            TokenValue::LeftBrace => "grouping",
            TokenValue::RightBrace => "grouping",
            TokenValue::Comma => "operator",
            TokenValue::Dot => "operator",
            TokenValue::Minus => "operator",
            TokenValue::Plus => "operator",
            TokenValue::Semicolon => "operator",
            TokenValue::Slash => "operator",
            TokenValue::Star => "operator",
            TokenValue::Bang => "operator",
            TokenValue::BangEqual => "operator",
            TokenValue::Equal => "operator",
            TokenValue::EqualEqual => "operator",
            TokenValue::Greater => "operator",
            TokenValue::GreaterEqual => "operator",
            TokenValue::Less => "operator",
            TokenValue::LessEqual => "operator",
            TokenValue::Identifier(_) => "identifier",
            TokenValue::String(_) => "literal",
            TokenValue::Number(_) => "literal",
            TokenValue::And => "keyword",
            TokenValue::Class => "keyword",
            TokenValue::Else => "keyword",
            TokenValue::False => "keyword",
            TokenValue::Fun => "keyword",
            TokenValue::For => "keyword",
            TokenValue::If => "keyword",
            TokenValue::Nil => "keyword",
            TokenValue::Or => "keyword",
            TokenValue::Print => "keyword",
            TokenValue::Return => "keyword",
            TokenValue::Super => "keyword",
            TokenValue::This => "keyword",
            TokenValue::True => "keyword",
            TokenValue::Var => "keyword",
            TokenValue::While => "keyword",
        }
    }
}

impl Display for TokenValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenValue::LeftParen => write!(f, "("),
            TokenValue::RightParen => write!(f, ")"),
            TokenValue::LeftBrace => write!(f, "{{"),
            TokenValue::RightBrace => write!(f, "}}"),
            TokenValue::Comma => write!(f, ","),
            TokenValue::Dot => write!(f, "."),
            TokenValue::Minus => write!(f, "-"),
            TokenValue::Plus => write!(f, "+"),
            TokenValue::Semicolon => write!(f, ";"),
            TokenValue::Slash => write!(f, "/"),
            TokenValue::Star => write!(f, "*"),
            TokenValue::Bang => write!(f, "!"),
            TokenValue::BangEqual => write!(f, "!="),
            TokenValue::Equal => write!(f, "="),
            TokenValue::EqualEqual => write!(f, "=="),
            TokenValue::Greater => write!(f, ">"),
            TokenValue::GreaterEqual => write!(f, ">="),
            TokenValue::Less => write!(f, "<"),
            TokenValue::LessEqual => write!(f, "<="),
            TokenValue::Identifier(i) => write!(f, "{}", i),
            TokenValue::String(s) => write!(f, "\"{}\"", s),
            TokenValue::Number(n) => write!(f, "{}", n),
            TokenValue::And => write!(f, "and"),
            TokenValue::Class => write!(f, "class"),
            TokenValue::Else => write!(f, "else"),
            TokenValue::False => write!(f, "false"),
            TokenValue::Fun => write!(f, "fun"),
            TokenValue::For => write!(f, "for"),
            TokenValue::If => write!(f, "if"),
            TokenValue::Nil => write!(f, "nil"),
            TokenValue::Or => write!(f, "or"),
            TokenValue::Print => write!(f, "print"),
            TokenValue::Return => write!(f, "return"),
            TokenValue::Super => write!(f, "super"),
            TokenValue::This => write!(f, "this"),
            TokenValue::True => write!(f, "true"),
            TokenValue::Var => write!(f, "var"),
            TokenValue::While => write!(f, "while"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<C: Context> {
    pub value: TokenValue,
    pub context: C,
}

impl<C: Context> From<(TokenValue, C)> for Token<C> {
    fn from(args: (TokenValue, C)) -> Self {
        Self {
            value: args.0,
            context: args.1,
        }
    }
}
