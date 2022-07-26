use std::fmt::Display;

use crate::position::Position;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
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

impl Token {
    pub fn token_type(&self) -> &'static str {
        match self {
            Token::LeftParen => "grouping",
            Token::RightParen => "grouping",
            Token::LeftBrace => "grouping",
            Token::RightBrace => "grouping",
            Token::Comma => "operator",
            Token::Dot => "operator",
            Token::Minus => "operator",
            Token::Plus => "operator",
            Token::Semicolon => "operator",
            Token::Slash => "operator",
            Token::Star => "operator",
            Token::Bang => "operator",
            Token::BangEqual => "operator",
            Token::Equal => "operator",
            Token::EqualEqual => "operator",
            Token::Greater => "operator",
            Token::GreaterEqual => "operator",
            Token::Less => "operator",
            Token::LessEqual => "operator",
            Token::Identifier(_) => "identifier",
            Token::String(_) => "literal",
            Token::Number(_) => "literal",
            Token::And => "keyword",
            Token::Class => "keyword",
            Token::Else => "keyword",
            Token::False => "keyword",
            Token::Fun => "keyword",
            Token::For => "keyword",
            Token::If => "keyword",
            Token::Nil => "keyword",
            Token::Or => "keyword",
            Token::Print => "keyword",
            Token::Return => "keyword",
            Token::Super => "keyword",
            Token::This => "keyword",
            Token::True => "keyword",
            Token::Var => "keyword",
            Token::While => "keyword",
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::Minus => write!(f, "-"),
            Token::Plus => write!(f, "+"),
            Token::Semicolon => write!(f, ";"),
            Token::Slash => write!(f, "/"),
            Token::Star => write!(f, "*"),
            Token::Bang => write!(f, "!"),
            Token::BangEqual => write!(f, "!="),
            Token::Equal => write!(f, "="),
            Token::EqualEqual => write!(f, "=="),
            Token::Greater => write!(f, ">"),
            Token::GreaterEqual => write!(f, ">="),
            Token::Less => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),
            Token::Identifier(i) => write!(f, "{}", i),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Number(n) => write!(f, "{}", n),
            Token::And => write!(f, "and"),
            Token::Class => write!(f, "class"),
            Token::Else => write!(f, "else"),
            Token::False => write!(f, "false"),
            Token::Fun => write!(f, "fun"),
            Token::For => write!(f, "for"),
            Token::If => write!(f, "if"),
            Token::Nil => write!(f, "nil"),
            Token::Or => write!(f, "or"),
            Token::Print => write!(f, "print"),
            Token::Return => write!(f, "return"),
            Token::Super => write!(f, "super"),
            Token::This => write!(f, "this"),
            Token::True => write!(f, "true"),
            Token::Var => write!(f, "var"),
            Token::While => write!(f, "while"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenWithPosition {
    pub token: Token,
    pub position: Position,
}

impl From<(Token, Position)> for TokenWithPosition {
    fn from(tp: (Token, Position)) -> Self {
        Self {
            token: tp.0,
            position: tp.1,
        }
    }
}

impl From<TokenWithPosition> for (Token, Position) {
    fn from(tp: TokenWithPosition) -> (Token, Position) {
        (tp.token, tp.position)
    }
}

impl AsRef<Token> for TokenWithPosition {
    fn as_ref(&self) -> &Token {
        &self.token
    }
}

impl AsRef<Position> for TokenWithPosition {
    fn as_ref(&self) -> &Position {
        &self.position
    }
}
