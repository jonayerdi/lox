use std::fmt::Display;

use crate::{
    expression::{
        BinaryOperator, Expr, Expression, ExpressionContext, LiteralValue, UnaryOperator, LogicalOperator,
    },
    result::LoxError,
    rewind::Rewind,
    statement::Statement,
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

    fn error<D: Display>(&self, msg: D, token: Option<Token>) -> LoxError {
        LoxError::parse(msg, ExpressionContext::new(token))
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

    // GRAMMAR
    // program        → declaration * EOF ;
    pub fn parse(&mut self) -> Result<Vec<Statement>, LoxError> {
        let mut statements = Vec::new();
        while let Some(token) = self.next_token() {
            self.rewind_token(token);
            statements.push(self.declaration()?);
        }
        Ok(statements)
    }

    // declaration    → varDecl | statement ;
    pub fn declaration(&mut self) -> Result<Statement, LoxError> {
        match self.next_token() {
            Some(token) => Ok(match token.value {
                TokenValue::Var => self.variable_declaration()?,
                _ => {
                    self.rewind_token(token);
                    self.statement()?
                }
            }),
            None => Err(self.error(format!("Expected declaration, found EOF"), None)),
        }
    }

    // varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
    pub fn variable_declaration(&mut self) -> Result<Statement, LoxError> {
        let identifier = match self.next_token() {
            Some(Token {
                value: TokenValue::Identifier(identifier),
                context: _,
            }) => identifier,
            Some(Token { value, context: _ }) => {
                return Err(self.error(
                    format!("Expected identifier after \"var\", found {value}"),
                    None,
                ))
            }
            None => {
                return Err(self.error(
                    format!("Expected identifier after \"var\", found EOF"),
                    None,
                ))
            }
        };
        match self.next_token() {
            Some(Token {
                value: TokenValue::Equal,
                context: _,
            }) => {
                let initializer = self.expression()?;
                match self.next_token() {
                    Some(token) => match token.value {
                        TokenValue::Semicolon => Ok(Statement::var(identifier, Some(initializer))),
                        _ => Err(self.error(
                            format!(
                                "Expected ';' after variable declaration statement, found \"{}\"",
                                token.value
                            ),
                            None,
                        )),
                    },
                    None => Err(self.error(
                        format!("Expected ';' after variable declaration statement, found EOF"),
                        None,
                    )),
                }
            }
            Some(Token {
                value: TokenValue::Semicolon,
                context: _,
            }) => Ok(Statement::var(identifier, None)),
            Some(Token { value, context: _ }) => Err(self.error(
                format!("Expected ';' after variable declaration statement, found {value}"),
                None,
            )),
            None => Err(self.error(
                format!("Expected ';' after variable declaration statement, found EOF"),
                None,
            )),
        }
    }

    // statement → exprStmt | ifStmt | printStmt | block ;
    pub fn statement(&mut self) -> Result<Statement, LoxError> {
        match self.next_token() {
            Some(token) => Ok(match token.value {
                TokenValue::Print => self.print_statement()?,
                TokenValue::If => self.if_statement()?,
                TokenValue::LeftBrace => self.block_statement()?,
                _ => {
                    self.rewind_token(token);
                    self.expression_statement()?
                }
            }),
            None => Err(self.error(format!("Expected statement, found EOF"), None)),
        }
    }

    // printStmt → "print" expression ";" ;
    pub fn print_statement(&mut self) -> Result<Statement, LoxError> {
        let value = self.expression()?;
        match self.next_token() {
            Some(token) => match token.value {
                TokenValue::Semicolon => Ok(Statement::print(value)),
                _ => Err(self.error(
                    format!(
                        "Expected ';' after print statement, found \"{}\"",
                        token.value
                    ),
                    None,
                )),
            },
            None => Err(self.error(
                format!("Expected ';' after print statement, found EOF"),
                None,
            )),
        }
    }

    // ifStmt → "if" "(" expression ")" statement ( "else" statement )? ;
    pub fn if_statement(&mut self) -> Result<Statement, LoxError> {
        match self.next_token() {
            Some(token) => match token.value {
                TokenValue::LeftParen => {
                    let condition  = self.expression()?;
                    match self.next_token() {
                        Some(token) => match token.value {
                            TokenValue::RightParen => {
                                let then_branch = self.statement()?;
                                let token = self.next_token();
                                let else_branch = match token {
                                    Some(token) => {
                                        match token.value {
                                            TokenValue::Else => Some(self.statement()?),
                                            _ => {
                                                self.rewind_token(token);
                                                None
                                            }
                                        }
                                    }
                                    _ => None,
                                };
                                Ok(Statement::ifelse(condition, then_branch, else_branch))
                            },
                            _ => Err(self.error(
                                format!(
                                    "Expected ')' after if condition, found \"{}\"",
                                    token.value
                                ),
                                None,
                            )),
                        },
                        None => Err(self.error(
                            format!("Expected ')' after if condition, found EOF"),
                            None,
                        )),
                    }
                },
                _ => Err(self.error(
                    format!(
                        "Expected '(' after if, found \"{}\"",
                        token.value
                    ),
                    None,
                )),
            },
            None => Err(self.error(
                format!("Expected '(' after if, found EOF"),
                None,
            )),
        }
    }

    // block → "{" declaration* "}" ;
    pub fn block_statement(&mut self) -> Result<Statement, LoxError> {
        let mut statements = Vec::with_capacity(16);
        while let Some(token) = self.next_token() {
            match token.value {
                TokenValue::RightBrace => return Ok(Statement::block(statements)),
                _ => {
                    self.rewind_token(token);
                    statements.push(self.declaration()?);
                },
            }
        }
        Err(self.error(
            format!("Expected '}}' after block statement, found EOF"),
            None,
        ))
    }

    // exprStmt → expression ";" ;
    pub fn expression_statement(&mut self) -> Result<Statement, LoxError> {
        let value = self.expression()?;
        match self.next_token() {
            Some(token) => match token.value {
                TokenValue::Semicolon => Ok(Statement::expression(value)),
                _ => Err(self.error(
                    format!(
                        "Expected ';' after expression statement, found \"{}\"",
                        token.value
                    ),
                    None,
                )),
            },
            None => Err(self.error(
                format!("Expected ';' after expression statement, found EOF"),
                None,
            )),
        }
    }

    // GRAMMAR
    // expression     → assignment ;
    // assignment     → IDENTIFIER "=" assignment | equality ;
    // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    // comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    // term           → factor ( ( "-" | "+" ) factor )* ;
    // factor         → unary ( ( "/" | "*" ) unary )* ;
    // unary          → ( "!" | "-" ) unary | primary ;
    // primary        → IDENTIFIER | NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    pub fn expression(&mut self) -> Result<Expr, LoxError> {
        self.assignment()
    }

    // assignment     → IDENTIFIER "=" assignment | logic_or ;
    pub fn assignment(&mut self) -> Result<Expr, LoxError> {
        let expr = self.logic_or()?;
        match self.next_token() {
            Some(token) => match token.value {
                TokenValue::Equal => {
                    let value = self.assignment()?;
                    match expr.as_ref() {
                        Expression::Variable(var_expr) => {
                            Ok(Expression::assignment(var_expr.identifier.clone(), value))
                        }
                        _ => Err(self.error(
                            format!("Expected identifier for assignment, found \"{expr}\""),
                            None,
                        )),
                    }
                }
                _ => {
                    self.rewind_token(token);
                    Ok(expr)
                }
            },
            _ => Ok(expr),
        }
    }

    // logic_or       → logic_and ( "or" logic_and )* ;
    pub fn logic_or(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.logic_and()?;
        loop {
            if let Some(token) = self.next_token() {
                match token.value {
                    TokenValue::Or => {
                        let right: Box<Expression> = self.logic_and()?;
                        expr = Expression::logical(expr, LogicalOperator::Or, right);
                        continue;
                    }
                    _ => {
                        self.rewind_token(token);
                    }
                }
            }
            return Ok(expr);
        }
    }

    // logic_and      → equality ( "and" equality )* ;
    pub fn logic_and(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.equality()?;
        loop {
            if let Some(token) = self.next_token() {
                match token.value {
                    TokenValue::And => {
                        let right: Box<Expression> = self.equality()?;
                        expr = Expression::logical(expr, LogicalOperator::And, right);
                        continue;
                    }
                    _ => {
                        self.rewind_token(token);
                    }
                }
            }
            return Ok(expr);
        }
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

    // primary → IDENTIFIER | NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    pub fn primary(&mut self) -> Result<Expr, LoxError> {
        if let Some(token) = self.next_token() {
            return match &token.value {
                TokenValue::Identifier(identifier) => Ok(Expression::variable(identifier.clone())),
                TokenValue::Nil => Ok(Expression::literal(LiteralValue::Nil)),
                TokenValue::True => Ok(Expression::literal(LiteralValue::Boolean(true))),
                TokenValue::False => Ok(Expression::literal(LiteralValue::Boolean(false))),
                TokenValue::Number(num) => match num.parse() {
                    Ok(num) => Ok(Expression::literal(LiteralValue::Number(num))),
                    Err(e) => Err(self.error(
                        format!("Error parsing '{num}' as number: {e}"),
                        Some(token),
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
                                    "Expected ')' after expression, found {} '{}'",
                                    right_paren.value.token_type(),
                                    right_paren.value
                                ),
                                Some(token),
                            ))
                        }
                    } else {
                        Err(self.error(format!("Expected ')' after expression"), Some(token)))
                    }
                }
                _ => Err(self.error(
                    format!(
                        "Expected expression, found {} '{}'",
                        token.value.token_type(),
                        token.value
                    ),
                    Some(token),
                )),
            };
        }
        Err(self.error(format!("Expected expression, found EOF"), None))
    }
}
