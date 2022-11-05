use std::fmt::Display;

use crate::{
    expression::{
        BinaryOperator, Expr, Expression, ExpressionContext, LiteralValue, UnaryOperator,
    },
    result::LoxError,
    rewind::Rewind,
    token::{Token, TokenValue},
};

/// Recursive descent parser for Lox grammar
pub struct Parser<S: Iterator<Item = Token>> {
    source: Rewind<S>,
}

impl<S: Iterator<Item = Token>> Parser<S> {
    pub fn new(source: S) -> Self {
        Self {
            source: Rewind::with_capacity(source, 8),
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        self.source.next()
    }

    fn rewind_token(&mut self, token: Token) {
        self.source.rewind(token)
    }

    fn error<D: Display>(&self, msg: D, tokens: Vec<Token>) -> LoxError {
        LoxError::parse(msg, ExpressionContext::new(tokens))
    }

    /// Advances the parser to the next statement.
    ///
    /// Can be used to keep parsing after an error in order to check the rest of the code.
    pub fn synchronize(&mut self) {
        while let Some(token) = self.next_token() {
            match token.value {
                TokenValue::Class
                | TokenValue::Fun
                | TokenValue::Var
                | TokenValue::For
                | TokenValue::If
                | TokenValue::While
                | TokenValue::Print
                | TokenValue::Return => {
                    self.rewind_token(token);
                    break;
                }
                _ => continue,
            }
        }
    }

    pub fn parse(&mut self) -> Result<Expr, LoxError> {
        self.expression()
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
                let operator = match token.value {
                    TokenValue::EqualEqual => Some(BinaryOperator::Eq),
                    TokenValue::BangEqual => Some(BinaryOperator::Neq),
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
                let operator = match token.value {
                    TokenValue::Greater => Some(BinaryOperator::Gt),
                    TokenValue::GreaterEqual => Some(BinaryOperator::Geq),
                    TokenValue::Less => Some(BinaryOperator::Lt),
                    TokenValue::LessEqual => Some(BinaryOperator::Leq),
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
                let operator = match token.value {
                    TokenValue::Plus => Some(BinaryOperator::Add),
                    TokenValue::Minus => Some(BinaryOperator::Sub),
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
                let operator = match token.value {
                    TokenValue::Star => Some(BinaryOperator::Mul),
                    TokenValue::Slash => Some(BinaryOperator::Div),
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
            let operator = match token.value {
                TokenValue::Bang => Some(UnaryOperator::Not),
                TokenValue::Minus => Some(UnaryOperator::Neg),
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
            return match &token.value {
                TokenValue::Nil => Ok(Expression::literal(LiteralValue::Nil)),
                TokenValue::True => Ok(Expression::literal(LiteralValue::Boolean(true))),
                TokenValue::False => Ok(Expression::literal(LiteralValue::Boolean(false))),
                TokenValue::Number(num) => match num.parse() {
                    Ok(num) => Ok(Expression::literal(LiteralValue::Number(num))),
                    Err(e) => Err(self.error(
                        format!("Error parsing \'{num}\' as number: {e}"),
                        vec![token],
                    )),
                },
                TokenValue::String(s) => Ok(Expression::literal(LiteralValue::String(s.clone()))),
                TokenValue::LeftParen => {
                    let expr = self.expression()?;
                    if let Some(right_paren) = self.next_token() {
                        if right_paren.value == TokenValue::RightParen {
                            Ok(Expression::grouping(expr))
                        } else {
                            Err(self.error(
                                format!(
                                    "Expected ')' after expression, found {} \'{}\'",
                                    right_paren.value.token_type(),
                                    right_paren.value
                                ),
                                vec![token],
                            ))
                        }
                    } else {
                        Err(self.error(format!("Expected ')' after expression"), vec![token]))
                    }
                }
                _ => Err(self.error(
                    format!(
                        "Expected expression, found {} \'{}\'",
                        token.value.token_type(),
                        token.value
                    ),
                    vec![token],
                )),
            };
        }
        Err(self.error(format!("Expected expression, found EOF"), vec![]))
    }
}
