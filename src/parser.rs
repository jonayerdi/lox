use std::fmt::Display;

use crate::{
    expression::{BinaryOperator, Expr, Expression, LiteralValue, UnaryOperator},
    position::Position,
    result::LoxError,
    rewind::Rewind,
    token::Token,
};

/// Recursive descent parser for Lox grammar
pub struct Parser<T: AsRef<Token> + AsRef<Position>, S: Iterator<Item = T>> {
    source: Rewind<S>,
    positions: Vec<Position>,
}

impl<T: AsRef<Token> + AsRef<Position>, S: Iterator<Item = T>> Parser<T, S> {
    pub fn new(source: S) -> Self {
        Self {
            source: Rewind::with_capacity(source, 8),
            positions: Vec::with_capacity(64),
        }
    }
    fn next_token(&mut self) -> Option<T> {
        let token = self.source.next();
        if let Some(token) = token.as_ref() {
            self.positions.push(*AsRef::<Position>::as_ref(token));
        }
        token
    }
    fn rewind_token(&mut self, token: T) {
        self.positions.pop().expect(
            "Invalid parser state: Tried to call rewind_token, but positions stack is empty",
        );
        self.source.rewind(token)
    }
    fn error<D: Display>(&self, msg: D) -> LoxError {
        LoxError::parse(msg, self.positions.last().map(|p| *p).unwrap_or_default())
    }
    pub fn parse(&mut self) -> Result<Expr, LoxError> {
        todo!()
    }
    // GRAMMAR
    // expression     → equality ;
    // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    // comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    // term           → factor ( ( "-" | "+" ) factor )* ;
    // factor         → unary ( ( "/" | "*" ) unary )* ;
    // unary          → ( "!" | "-" ) unary | primary ;
    // primary        → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    pub fn expression(&mut self) -> Result<Expr, LoxError> {
        self.equality()
    }
    // equality → comparison ( ( "!=" | "==" ) comparison )* ;
    pub fn equality(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.comparison()?;
        loop {
            if let Some(token) = self.next_token() {
                let operator = match token.as_ref() {
                    Token::EqualEqual => Some(BinaryOperator::Eq),
                    Token::BangEqual => Some(BinaryOperator::Neq),
                    _ => {
                        self.rewind_token(token);
                        None
                    }
                };
                if let Some(operator) = operator {
                    let right = self.comparison()?;
                    expr = Expression::binary(expr, operator, right);
                    continue;
                }
            }
            return Ok(expr);
        }
    }
    // comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    pub fn comparison(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.term()?;
        loop {
            if let Some(token) = self.next_token() {
                let operator = match token.as_ref() {
                    Token::Greater => Some(BinaryOperator::Gt),
                    Token::GreaterEqual => Some(BinaryOperator::Geq),
                    Token::Less => Some(BinaryOperator::Lt),
                    Token::LessEqual => Some(BinaryOperator::Leq),
                    _ => {
                        self.rewind_token(token);
                        None
                    }
                };
                if let Some(operator) = operator {
                    let right = self.term()?;
                    expr = Expression::binary(expr, operator, right);
                    continue;
                }
            }
            return Ok(expr);
        }
    }
    // term → factor ( ( "-" | "+" ) factor )* ;
    pub fn term(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.factor()?;
        loop {
            if let Some(token) = self.next_token() {
                let operator = match token.as_ref() {
                    Token::Plus => Some(BinaryOperator::Add),
                    Token::Minus => Some(BinaryOperator::Sub),
                    _ => {
                        self.rewind_token(token);
                        None
                    }
                };
                if let Some(operator) = operator {
                    let right = self.factor()?;
                    expr = Expression::binary(expr, operator, right);
                    continue;
                }
            }
            return Ok(expr);
        }
    }
    // factor → unary ( ( "/" | "*" ) unary )* ;
    pub fn factor(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.unary()?;
        loop {
            if let Some(token) = self.next_token() {
                let operator = match token.as_ref() {
                    Token::Star => Some(BinaryOperator::Mul),
                    Token::Slash => Some(BinaryOperator::Div),
                    _ => {
                        self.rewind_token(token);
                        None
                    }
                };
                if let Some(operator) = operator {
                    let right = self.unary()?;
                    expr = Expression::binary(expr, operator, right);
                    continue;
                }
            }
            return Ok(expr);
        }
    }
    // unary → ( "!" | "-" ) unary | primary ;
    pub fn unary(&mut self) -> Result<Expr, LoxError> {
        if let Some(token) = self.next_token() {
            let operator = match token.as_ref() {
                Token::Plus => Some(UnaryOperator::Not),
                Token::Minus => Some(UnaryOperator::Neg),
                _ => {
                    self.rewind_token(token);
                    None
                }
            };
            if let Some(operator) = operator {
                return Ok(Expression::unary(operator, self.unary()?));
            }
        }
        self.primary()
    }
    // primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    pub fn primary(&mut self) -> Result<Expr, LoxError> {
        if let Some(token) = self.next_token() {
            return match token.as_ref() {
                Token::Nil => Ok(Expression::literal(LiteralValue::Nil)),
                Token::True => Ok(Expression::literal(LiteralValue::Boolean(true))),
                Token::False => Ok(Expression::literal(LiteralValue::Boolean(false))),
                Token::Number(num) => Ok(Expression::literal(LiteralValue::Number(
                    num.parse().map_err(|e| {
                        self.error(format!("Error parsing \'{num}\' as number: {e}"))
                    })?,
                ))),
                Token::String(s) => Ok(Expression::literal(LiteralValue::String(s.clone()))),
                Token::LeftParen => {
                    let expr = self.expression()?;
                    if let Some(right_paren) = self.next_token() {
                        if AsRef::<Token>::as_ref(&right_paren) == &Token::RightParen {
                            Ok(Expression::grouping(expr))
                        } else {
                            Err(self.error(format!(
                                "Expected ')' after expression, found {} \'{}\'",
                                AsRef::<Token>::as_ref(&right_paren).token_type(),
                                AsRef::<Token>::as_ref(&right_paren)
                            )))
                        }
                    } else {
                        Err(self.error(format!("Expected ')' after expression")))
                    }
                }
                _ => Err(self.error(format!(
                    "Expected expression, found {} \'{}\'",
                    AsRef::<Token>::as_ref(&token).token_type(),
                    AsRef::<Token>::as_ref(&token)
                ))),
            };
        }
        Err(self.error(format!("Expected expression, found EOF")))
    }
}
