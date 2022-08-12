use std::fmt::Display;

use crate::{
    context::Context,
    expression::{BinaryOperator, Expr, Expression, LiteralValue, UnaryOperator},
    result::LoxError,
    rewind::Rewind,
    token::{Token, TokenData},
};

/// Recursive descent parser for Lox grammar
pub struct Parser<C: Context, S: Iterator<Item = TokenData<C>>> {
    source: Rewind<S>,
    contexts: Vec<C>,
}

impl<C: Context, S: Iterator<Item = TokenData<C>>> Parser<C, S> {
    pub fn new(source: S) -> Self {
        Self {
            source: Rewind::with_capacity(source, 8),
            contexts: Vec::with_capacity(64),
        }
    }

    fn next_token(&mut self) -> Option<TokenData<C>> {
        let token = self.source.next();
        if let Some(token) = token.as_ref() {
            self.contexts.push(token.context.clone());
        }
        token
    }

    fn rewind_token(&mut self, token: TokenData<C>) {
        self.contexts.pop().expect(
            "Invalid parser state: Tried to call rewind_token, but positions stack is empty",
        );
        self.source.rewind(token)
    }

    fn error<D: Display>(&self, msg: D) -> LoxError<C> {
        LoxError::parse(
            msg,
            self.contexts.last().map(|p| p.clone()).unwrap_or_default(),
        )
    }

    /// Advances the parser to the next statement.
    ///
    /// Can be used to keep parsing after an error in order to check the rest of the code.
    pub fn synchronize(&mut self) {
        while let Some(token) = self.next_token() {
            match token.as_ref() {
                Token::Class
                | Token::Fun
                | Token::Var
                | Token::For
                | Token::If
                | Token::While
                | Token::Print
                | Token::Return => {
                    self.rewind_token(token);
                    break;
                }
                _ => continue,
            }
        }
    }

    pub fn parse(&mut self) -> Result<Expr, LoxError<C>> {
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
    pub fn expression(&mut self) -> Result<Expr, LoxError<C>> {
        self.equality()
    }

    // equality → comparison ( ( "!=" | "==" ) comparison )* ;
    pub fn equality(&mut self) -> Result<Expr, LoxError<C>> {
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
    pub fn comparison(&mut self) -> Result<Expr, LoxError<C>> {
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
    pub fn term(&mut self) -> Result<Expr, LoxError<C>> {
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
    pub fn factor(&mut self) -> Result<Expr, LoxError<C>> {
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
    pub fn unary(&mut self) -> Result<Expr, LoxError<C>> {
        if let Some(token) = self.next_token() {
            let operator = match token.as_ref() {
                Token::Bang => Some(UnaryOperator::Not),
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
    pub fn primary(&mut self) -> Result<Expr, LoxError<C>> {
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
                        if right_paren.as_ref() == &Token::RightParen {
                            Ok(Expression::grouping(expr))
                        } else {
                            Err(self.error(format!(
                                "Expected ')' after expression, found {} \'{}\'",
                                right_paren.as_ref().token_type(),
                                right_paren.as_ref()
                            )))
                        }
                    } else {
                        Err(self.error(format!("Expected ')' after expression")))
                    }
                }
                _ => Err(self.error(format!(
                    "Expected expression, found {} \'{}\'",
                    token.as_ref().token_type(),
                    token.as_ref()
                ))),
            };
        }
        Err(self.error(format!("Expected expression, found EOF")))
    }
}
