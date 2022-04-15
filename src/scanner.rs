use std::{collections::HashMap, fmt::Display, ops::ControlFlow};

use crate::{
    position::{Position, PositionTracker},
    result::LoxError,
    token::Token,
};

use lazy_static::lazy_static;

lazy_static! {
    pub static ref KEYWORDS: HashMap<&'static str, Token> = {
        let mut m = HashMap::new();
        m.insert("and", Token::And);
        m.insert("class", Token::Class);
        m.insert("else", Token::Else);
        m.insert("false", Token::False);
        m.insert("for", Token::For);
        m.insert("fun", Token::Fun);
        m.insert("if", Token::If);
        m.insert("nil", Token::Nil);
        m.insert("or", Token::Or);
        m.insert("print", Token::Print);
        m.insert("return", Token::Return);
        m.insert("super", Token::Super);
        m.insert("this", Token::This);
        m.insert("true", Token::True);
        m.insert("var", Token::Var);
        m.insert("while", Token::While);
        m
    };
}

#[derive(Debug, PartialEq)]
pub enum ScannerItem {
    Token(Token, Position),
    Error(LoxError),
}

pub struct Scanner<S: Iterator<Item = char>> {
    source: PositionTracker<S>,
    position: Position,
}

impl<S: Iterator<Item = char>> Scanner<S> {
    pub fn new(source: S) -> Self {
        Self {
            source: PositionTracker::new(source),
            position: Default::default(),
        }
    }
    fn token(&self, token: Token) -> ControlFlow<ScannerItem> {
        ControlFlow::Break(ScannerItem::Token(token, self.position))
    }
    fn error<D: Display>(&self, msg: D) -> ControlFlow<ScannerItem> {
        ControlFlow::Break(ScannerItem::Error(LoxError::scan(msg, self.position)))
    }
    fn try_match(&mut self, expected: char) -> bool {
        if let Some(c) = self.source.next() {
            if c == expected {
                return true;
            } else {
                self.source.rewind(c);
            }
        }
        false
    }
    fn advance_until_match(&mut self, expected: char) {
        for c in self.source.by_ref() {
            if c == expected {
                return;
            }
        }
    }
    fn match_string(&mut self) -> ControlFlow<ScannerItem> {
        if let Some(c) = self.source.next() {
            if c != '"' {
                return self.error(format!(
                    "Expected '\"' as the first character in match_string, got '{c}'",
                ));
            }
        } else {
            return self.error("Called match_string, but source is at EOF");
        }
        let mut string = String::with_capacity(64);
        loop {
            match self.source.next() {
                None => {
                    break self.error("Reached EOF in match_string before string termination");
                }
                Some('"') => {
                    break self.token(Token::String(string));
                }
                Some(c) => string.push(c),
            }
        }
    }
    fn match_number(&mut self) -> ControlFlow<ScannerItem> {
        let mut number = String::with_capacity(32);
        while let Some(c) = self.source.next() {
            match c {
                c if c.is_ascii_digit() => number.push(c),
                c => {
                    self.source.rewind(c);
                    break;
                }
            }
        }
        match self.source.next() {
            Some('.') => {
                number.push('.');
                while let Some(c) = self.source.next() {
                    match c {
                        c if c.is_ascii_digit() => number.push(c),
                        c => {
                            self.source.rewind(c);
                            break;
                        }
                    }
                }
            }
            Some(c) => self.source.rewind(c),
            None => {}
        }
        match number.parse::<f64>() {
            Ok(_) => self.token(Token::Number(number)),
            Err(error) => self.error(format!("Error parsing \"{number}\" as a number: {error}")),
        }
    }
    fn match_identifier(&mut self) -> ControlFlow<ScannerItem> {
        let mut identifier = String::with_capacity(32);
        while let Some(c) = self.source.next() {
            match c {
                c if c.is_ascii_alphanumeric() || c == '_' => identifier.push(c),
                c => {
                    self.source.rewind(c);
                    break;
                }
            }
        }
        self.token(match KEYWORDS.get(identifier.as_str()) {
            Some(keyword_token) => keyword_token.clone(),
            None => Token::Identifier(identifier),
        })
    }
}

impl<S: Iterator<Item = char>> Iterator for Scanner<S> {
    type Item = ScannerItem;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            self.position = self.source.position();
            let c0 = self.source.next()?;
            let item = match c0 {
                // Single-character and no lookahead needed
                '(' => self.token(Token::LeftParen),
                ')' => self.token(Token::RightParen),
                '{' => self.token(Token::LeftBrace),
                '}' => self.token(Token::RightBrace),
                ',' => self.token(Token::Comma),
                '.' => self.token(Token::Dot),
                '-' => self.token(Token::Minus),
                '+' => self.token(Token::Plus),
                ';' => self.token(Token::Semicolon),
                '*' => self.token(Token::Star),
                // One or two character tokens, lookahead needed
                '!' => {
                    if self.try_match('=') {
                        self.token(Token::BangEqual)
                    } else {
                        self.token(Token::Bang)
                    }
                }
                '=' => {
                    if self.try_match('=') {
                        self.token(Token::EqualEqual)
                    } else {
                        self.token(Token::Equal)
                    }
                }
                '<' => {
                    if self.try_match('=') {
                        self.token(Token::LessEqual)
                    } else {
                        self.token(Token::Less)
                    }
                }
                '>' => {
                    if self.try_match('=') {
                        self.token(Token::GreaterEqual)
                    } else {
                        self.token(Token::Greater)
                    }
                }
                // Division or line comment
                '/' => {
                    if self.try_match('/') {
                        self.advance_until_match('\n'); // Line comment: Skip until next line
                        ControlFlow::Continue(())
                    } else {
                        self.token(Token::Slash)
                    }
                }
                // String or numeric literals
                '"' => {
                    self.source.rewind('"');
                    self.match_string()
                }
                c if c.is_ascii_digit() => {
                    self.source.rewind(c);
                    self.match_number()
                }
                // Identifiers
                c if c.is_ascii_alphabetic() => {
                    self.source.rewind(c);
                    self.match_identifier()
                }
                // Whitespace
                c if c.is_ascii_whitespace() => ControlFlow::Continue(()),
                // Invalid character
                c => self.error(format!("Unexpected character: '{c}'")),
            };
            if let ControlFlow::Break(item) = item {
                break Some(item);
            }
        }
    }
}
