use crate::{
    expression::{BinaryOperator, Expr, Expression, LiteralValue, UnaryOperator},
    position::Position,
    rewind::Rewind,
    token::Token,
};

/// Recursive descent parser for Lox grammar
pub struct Parser<T: AsRef<Token> + AsRef<Position>, S: Iterator<Item = T>> {
    source: Rewind<S>,
}

impl<T: AsRef<Token> + AsRef<Position>, S: Iterator<Item = T>> Parser<T, S> {
    pub fn new(source: S) -> Self {
        Self {
            source: Rewind::with_capacity(source, 8),
        }
    }
    pub fn parse(&mut self) -> Expr {
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
    pub fn expression(&mut self) -> Expr {
        self.equality()
    }
    // equality → comparison ( ( "!=" | "==" ) comparison )* ;
    pub fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();
        loop {
            if let Some(token) = self.source.next() {
                let operator = match token.as_ref() {
                    Token::EqualEqual => Some(BinaryOperator::Eq),
                    Token::BangEqual => Some(BinaryOperator::Neq),
                    _ => {
                        self.source.rewind(token);
                        None
                    }
                };
                if let Some(operator) = operator {
                    let right = self.comparison();
                    expr = Expression::binary(expr, operator, right);
                    continue;
                }
            }
            return expr;
        }
    }
    // comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    pub fn comparison(&mut self) -> Expr {
        let mut expr = self.term();
        loop {
            if let Some(token) = self.source.next() {
                let operator = match token.as_ref() {
                    Token::Greater => Some(BinaryOperator::Gt),
                    Token::GreaterEqual => Some(BinaryOperator::Geq),
                    Token::Less => Some(BinaryOperator::Lt),
                    Token::LessEqual => Some(BinaryOperator::Leq),
                    _ => {
                        self.source.rewind(token);
                        None
                    }
                };
                if let Some(operator) = operator {
                    let right = self.term();
                    expr = Expression::binary(expr, operator, right);
                    continue;
                }
            }
            return expr;
        }
    }
    // term → factor ( ( "-" | "+" ) factor )* ;
    pub fn term(&mut self) -> Expr {
        let mut expr = self.factor();
        loop {
            if let Some(token) = self.source.next() {
                let operator = match token.as_ref() {
                    Token::Plus => Some(BinaryOperator::Add),
                    Token::Minus => Some(BinaryOperator::Sub),
                    _ => {
                        self.source.rewind(token);
                        None
                    }
                };
                if let Some(operator) = operator {
                    let right = self.factor();
                    expr = Expression::binary(expr, operator, right);
                    continue;
                }
            }
            return expr;
        }
    }
    // factor → unary ( ( "/" | "*" ) unary )* ;
    pub fn factor(&mut self) -> Expr {
        let mut expr = self.unary();
        loop {
            if let Some(token) = self.source.next() {
                let operator = match token.as_ref() {
                    Token::Star => Some(BinaryOperator::Mul),
                    Token::Slash => Some(BinaryOperator::Div),
                    _ => {
                        self.source.rewind(token);
                        None
                    }
                };
                if let Some(operator) = operator {
                    let right = self.unary();
                    expr = Expression::binary(expr, operator, right);
                    continue;
                }
            }
            return expr;
        }
    }
    // unary → ( "!" | "-" ) unary | primary ;
    pub fn unary(&mut self) -> Expr {
        if let Some(token) = self.source.next() {
            let operator = match token.as_ref() {
                Token::Plus => Some(UnaryOperator::Not),
                Token::Minus => Some(UnaryOperator::Neg),
                _ => {
                    self.source.rewind(token);
                    None
                }
            };
            if let Some(operator) = operator {
                return Expression::unary(operator, self.unary());
            }
        }
        self.primary()
    }
    // primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    pub fn primary(&mut self) -> Expr {
        if let Some(token) = self.source.next() {
            return match token.as_ref() {
                Token::Nil => Expression::literal(LiteralValue::Nil),
                Token::True => Expression::literal(LiteralValue::Boolean(true)),
                Token::False => Expression::literal(LiteralValue::Boolean(false)),
                Token::Number(num) => Expression::literal(LiteralValue::Number(*num)),
                Token::String(s) => Expression::literal(LiteralValue::String(s.clone())),
                Token::LeftParen => {
                    let expr = self.expression();
                    if let Some(right_paren) = self.source.next() {
                        if AsRef::<Token>::as_ref(&right_paren) == &Token::RightParen {
                            Expression::grouping(expr)
                        } else {
                            todo!()
                        }
                    } else {
                        todo!()
                    }
                }
                _ => todo!(),
            };
        }
        todo!();
    }
}
