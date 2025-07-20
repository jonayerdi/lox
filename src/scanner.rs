use std::{collections::HashMap, fmt::Display, ops::ControlFlow, sync::LazyLock};

use crate::{
    context::{Position, PositionTracker},
    result::{LoxError, Result},
    token::{Token, TokenContext, TokenValue},
};

static KEYWORDS: LazyLock<HashMap<&'static str, TokenValue>> = LazyLock::new(|| {
    let mut m = HashMap::new();
    m.insert("and", TokenValue::And);
    m.insert("class", TokenValue::Class);
    m.insert("else", TokenValue::Else);
    m.insert("false", TokenValue::False);
    m.insert("for", TokenValue::For);
    m.insert("fun", TokenValue::Fun);
    m.insert("if", TokenValue::If);
    m.insert("nil", TokenValue::Nil);
    m.insert("or", TokenValue::Or);
    m.insert("print", TokenValue::Print);
    m.insert("return", TokenValue::Return);
    m.insert("super", TokenValue::Super);
    m.insert("this", TokenValue::This);
    m.insert("true", TokenValue::True);
    m.insert("var", TokenValue::Var);
    m.insert("while", TokenValue::While);
    m
});

pub type ScannerItem = Result<Token>;

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
    fn token(&self, token: TokenValue) -> ControlFlow<ScannerItem> {
        ControlFlow::Break(ScannerItem::Ok(Token {
            value: token,
            context: TokenContext::new(self.position),
        }))
    }
    fn error<D: Display>(&self, msg: D) -> ControlFlow<ScannerItem> {
        ControlFlow::Break(ScannerItem::Err(LoxError::scan(
            msg,
            TokenContext::new(self.position),
        )))
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
                    break self.token(TokenValue::String(string));
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
            Ok(_) => self.token(TokenValue::Number(number)),
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
            None => TokenValue::Identifier(identifier),
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
                '(' => self.token(TokenValue::LeftParen),
                ')' => self.token(TokenValue::RightParen),
                '{' => self.token(TokenValue::LeftBrace),
                '}' => self.token(TokenValue::RightBrace),
                ',' => self.token(TokenValue::Comma),
                '.' => self.token(TokenValue::Dot),
                '-' => self.token(TokenValue::Minus),
                '+' => self.token(TokenValue::Plus),
                ';' => self.token(TokenValue::Semicolon),
                '*' => self.token(TokenValue::Star),
                // One or two character tokens, lookahead needed
                '!' => {
                    if self.try_match('=') {
                        self.token(TokenValue::BangEqual)
                    } else {
                        self.token(TokenValue::Bang)
                    }
                }
                '=' => {
                    if self.try_match('=') {
                        self.token(TokenValue::EqualEqual)
                    } else {
                        self.token(TokenValue::Equal)
                    }
                }
                '<' => {
                    if self.try_match('=') {
                        self.token(TokenValue::LessEqual)
                    } else {
                        self.token(TokenValue::Less)
                    }
                }
                '>' => {
                    if self.try_match('=') {
                        self.token(TokenValue::GreaterEqual)
                    } else {
                        self.token(TokenValue::Greater)
                    }
                }
                // Division or line comment
                '/' => {
                    if self.try_match('/') {
                        self.advance_until_match('\n'); // Line comment: Skip until next line
                        ControlFlow::Continue(())
                    } else {
                        self.token(TokenValue::Slash)
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
