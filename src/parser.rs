use std::fmt::Display;

use crate::{
    expression::{
        BinaryOperator, Expr, Expression, ExpressionContext, LiteralValue, LogicalOperator,
        UnaryOperator,
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
        LoxError::parse(false, msg, ExpressionContext::new(token))
    }

    fn error_eof<D: Display>(&self, msg: D, token: Option<Token>) -> LoxError {
        LoxError::parse(true, msg, ExpressionContext::new(token))
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

    // declaration    → funDecl | varDecl | statement ;
    pub fn declaration(&mut self) -> Result<Statement, LoxError> {
        match self.next_token() {
            Some(token) => Ok(match token.value {
                TokenValue::Fun => self.function_declaration()?,
                TokenValue::Var => self.variable_declaration()?,
                _ => {
                    self.rewind_token(token);
                    self.statement()?
                }
            }),
            None => Err(self.error_eof("Expected declaration, found EOF", None)),
        }
    }

    // funDecl        → "fun" function ;
    // function       → IDENTIFIER "(" parameters? ")" block ;
    // parameters     → IDENTIFIER ( "," IDENTIFIER )* ;
    pub fn function_declaration(&mut self) -> Result<Statement, LoxError> {
        let identifier = match self.next_token() {
            Some(Token {
                value: TokenValue::Identifier(identifier),
                context: _,
            }) => identifier,
            Some(Token { value, context: _ }) => {
                return Err(self.error(
                    format!("Expected identifier after \"fun\", found {value}"),
                    None,
                ))
            }
            None => {
                return Err(self.error_eof("Expected identifier after \"fun\", found EOF", None))
            }
        };
        match self.next_token() {
            Some(Token {
                value: TokenValue::LeftParen,
                context: _,
            }) => {}
            Some(Token { value, context: _ }) => {
                return Err(self.error(
                    format!("Expected \"(\" before \"fun\" parameters, found {value}"),
                    None,
                ))
            }
            None => {
                return Err(
                    self.error_eof("Expected \"(\" before \"fun\" parameters, found EOF", None)
                )
            }
        }
        let parameters = self.parameters()?;
        match self.next_token() {
            Some(Token {
                value: TokenValue::RightParen,
                context: _,
            }) => {}
            Some(Token { value, context: _ }) => {
                return Err(self.error(
                    format!("Expected \")\" after \"fun\" parameters, found {value}"),
                    None,
                ))
            }
            None => {
                return Err(
                    self.error_eof("Expected \")\" after \"fun\" parameters, found EOF", None)
                )
            }
        }
        let body = match self.next_token() {
            Some(Token {
                value: TokenValue::LeftBrace,
                context: _,
            }) => self.block_statement()?,
            Some(Token { value, context: _ }) => {
                return Err(self.error(
                    format!("Expected \"{{\" before \"fun\" body, found {value}"),
                    None,
                ))
            }
            None => {
                return Err(self.error_eof("Expected \"{{\" before \"fun\" body, found EOF", None))
            }
        };
        Ok(Statement::function(identifier, parameters, body))
    }

    // parameters     → IDENTIFIER ( "," IDENTIFIER )* ;
    pub fn parameters(&mut self) -> Result<Vec<String>, LoxError> {
        let mut params = Vec::with_capacity(16);
        loop {
            match self.next_token() {
                Some(Token {
                    value: TokenValue::Identifier(identifier),
                    context: _,
                }) => {
                    if params.len() >= 255 {
                        return Err(self.error(
                            "Function definition with more than 255 parameters not supported.",
                            None,
                        ));
                    }
                    params.push(identifier);
                    match self.next_token() {
                        Some(Token {
                            value: TokenValue::Comma,
                            context: _,
                        }) => {}
                        Some(token) => {
                            self.rewind_token(token);
                            return Ok(params);
                        }
                        None => return Ok(params),
                    }
                }
                Some(token) => {
                    self.rewind_token(token);
                    return Ok(params);
                }
                None => return Ok(params),
            }
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
                return Err(self.error_eof("Expected identifier after \"var\", found EOF", None))
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
                    None => Err(self.error_eof(
                        "Expected ';' after variable declaration statement, found EOF",
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
            None => Err(self.error_eof(
                "Expected ';' after variable declaration statement, found EOF",
                None,
            )),
        }
    }

    // statement → exprStmt | forStmt | ifStmt | printStmt | returnStmt | whileStmt | block ;
    pub fn statement(&mut self) -> Result<Statement, LoxError> {
        match self.next_token() {
            Some(token) => Ok(match token.value {
                TokenValue::Print => self.print_statement()?,
                TokenValue::If => self.if_statement()?,
                TokenValue::While => self.while_statement()?,
                TokenValue::For => self.for_statement()?,
                TokenValue::Return => self.return_statement()?,
                TokenValue::LeftBrace => self.block_statement()?,
                _ => {
                    self.rewind_token(token);
                    self.expression_statement()?
                }
            }),
            None => Err(self.error_eof("Expected statement, found EOF", None)),
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
            None => Err(self.error_eof("Expected ';' after print statement, found EOF", None)),
        }
    }

    // ifStmt → "if" "(" expression ")" statement ( "else" statement )? ;
    pub fn if_statement(&mut self) -> Result<Statement, LoxError> {
        match self.next_token() {
            Some(token) => match token.value {
                TokenValue::LeftParen => {
                    let condition = self.expression()?;
                    match self.next_token() {
                        Some(token) => match token.value {
                            TokenValue::RightParen => {
                                let then_branch = self.statement()?;
                                let token = self.next_token();
                                let else_branch = match token {
                                    Some(token) => match token.value {
                                        TokenValue::Else => Some(self.statement()?),
                                        _ => {
                                            self.rewind_token(token);
                                            None
                                        }
                                    },
                                    _ => None,
                                };
                                Ok(Statement::if_stmt(condition, then_branch, else_branch))
                            }
                            _ => Err(self.error(
                                format!(
                                    "Expected ')' after if condition, found \"{}\"",
                                    token.value
                                ),
                                None,
                            )),
                        },
                        None => {
                            Err(self.error_eof("Expected ')' after if condition, found EOF", None))
                        }
                    }
                }
                _ => Err(self.error(
                    format!("Expected '(' after if, found \"{}\"", token.value),
                    None,
                )),
            },
            None => Err(self.error_eof("Expected '(' after if, found EOF", None)),
        }
    }

    // whileStmt → "while" "(" expression ")" statement ;
    pub fn while_statement(&mut self) -> Result<Statement, LoxError> {
        match self.next_token() {
            Some(token) => {
                match token.value {
                    TokenValue::LeftParen => {
                        let condition = self.expression()?;
                        match self.next_token() {
                            Some(token) => match token.value {
                                TokenValue::RightParen => {
                                    let body = self.statement()?;
                                    Ok(Statement::while_stmt(condition, body))
                                }
                                _ => Err(self.error(
                                    format!(
                                        "Expected ')' after while condition, found \"{}\"",
                                        token.value
                                    ),
                                    None,
                                )),
                            },
                            None => Err(self
                                .error_eof("Expected ')' after while condition, found EOF", None)),
                        }
                    }
                    _ => Err(self.error(
                        format!("Expected '(' after while, found \"{}\"", token.value),
                        None,
                    )),
                }
            }
            None => Err(self.error_eof("Expected '(' after while, found EOF", None)),
        }
    }

    // forStmt → "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement ;
    pub fn for_statement(&mut self) -> Result<Statement, LoxError> {
        match self.next_token() {
            Some(token) => match token.value {
                TokenValue::LeftParen => {
                    if let Some(token) = self.next_token() {
                        let initializer = match token.value {
                            TokenValue::Semicolon => None,
                            TokenValue::Var => Some(self.variable_declaration()?),
                            _ => {
                                {
                                    self.rewind_token(token);
                                    let decl = self.expression_statement()?;
                                    if let Some(token) = self.next_token() {
                                        if token.value == TokenValue::Semicolon {
                                            Ok(Some(decl))
                                        } else {
                                            Err(self.error(
                                            format!("Expected ';' after for initializer, found \"{}\"", token.value),
                                            None,
                                        ))
                                        }
                                    } else {
                                        Err(self.error_eof(
                                            "Expected ';' after for initializer, found EOF",
                                            None,
                                        ))
                                    }
                                }?
                            }
                        };
                        if let Some(token) = self.next_token() {
                            let condition = match token.value {
                                TokenValue::Semicolon => None,
                                _ => {
                                    self.rewind_token(token);
                                    let expr = self.expression()?;
                                    if let Some(token) = self.next_token() {
                                        if token.value == TokenValue::Semicolon {
                                            Ok(Some(expr))
                                        } else {
                                            Err(self.error(
                                                format!("Expected ';' after for condition, found \"{}\"", token.value),
                                                None,
                                            ))
                                        }
                                    } else {
                                        Err(self.error_eof(
                                            "Expected ';' after for condition, found EOF",
                                            None,
                                        ))
                                    }
                                }?,
                            };
                            if let Some(token) = self.next_token() {
                                let increment = match token.value {
                                    TokenValue::Semicolon => None,
                                    _ => {
                                        self.rewind_token(token);
                                        let expr = self.expression()?;
                                        if let Some(token) = self.next_token() {
                                            if token.value == TokenValue::RightParen {
                                                Ok(Some(expr))
                                            } else {
                                                Err(self.error(
                                                    format!("Expected ')' after for increment, found \"{}\"", token.value),
                                                    None,
                                                ))
                                            }
                                        } else {
                                            Err(self.error_eof(
                                                "Expected ')' after for increment, found EOF",
                                                None,
                                            ))
                                        }
                                    }?,
                                };
                                let mut body = self.statement()?;
                                if let Some(increment) = increment {
                                    body = Statement::block(vec![
                                        body,
                                        Statement::expression(increment),
                                    ]);
                                }
                                let condition = match condition {
                                    Some(condition) => condition,
                                    None => Expression::literal(LiteralValue::Boolean(true)),
                                };
                                body = Statement::while_stmt(condition, body);
                                if let Some(initializer) = initializer {
                                    body = Statement::block(vec![initializer, body]);
                                }
                                Ok(body)
                            } else {
                                Err(self.error_eof(
                                    "Expected condition or ';' after for condition, found EOF",
                                    None,
                                ))
                            }
                        } else {
                            Err(self.error_eof(
                                "Expected condition or ';' after for initializer, found EOF",
                                None,
                            ))
                        }
                    } else {
                        Err(self.error_eof("Expected initializer or ';' in for, found EOF", None))
                    }
                }
                _ => Err(self.error(
                    format!("Expected '(' after for, found \"{}\"", token.value),
                    None,
                )),
            },
            None => Err(self.error_eof("Expected '(' after for, found EOF", None)),
        }
    }

    // returnStmt     → "return" expression? ";" ;
    pub fn return_statement(&mut self) -> Result<Statement, LoxError> {
        let value = match self.next_token() {
            Some(Token {
                value: TokenValue::Semicolon,
                context: _,
            }) => None,
            Some(token) => {
                self.rewind_token(token);
                let value = self.expression()?;
                match self.next_token() {
                    Some(Token {
                        value: TokenValue::Semicolon,
                        context: _,
                    }) => Some(value),
                    Some(token) => {
                        return Err(self.error(
                            format!(
                                "Expected ';' after return statement expression, found {}",
                                token.value
                            ),
                            None,
                        ))
                    }
                    None => {
                        return Err(self.error_eof(
                            "Expected ';' after return statement expression, found EOF",
                            None,
                        ))
                    }
                }
            }
            None => {
                return Err(self.error_eof(
                    "Expected expression or ';' after return statement, found EOF",
                    None,
                ))
            }
        };
        Ok(Statement::return_stmt(value))
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
                }
            }
        }
        Err(self.error_eof("Expected '}' after block statement, found EOF", None))
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
            None => Err(self.error_eof("Expected ';' after expression statement, found EOF", None)),
        }
    }

    // GRAMMAR
    // expression     → assignment ;
    // assignment     → IDENTIFIER "=" assignment | equality ;
    // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    // comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    // term           → factor ( ( "-" | "+" ) factor )* ;
    // factor         → unary ( ( "/" | "*" ) unary )* ;
    // unary          → ( "!" | "-" ) unary | call ;
    // call           → primary ( "(" arguments? ")" )* ;
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

    // unary          → ( "!" | "-" ) unary | call ;
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
        self.call()
    }

    // call           → primary ( "(" arguments? ")" )* ;
    // arguments      → expression ( "," expression )* ;
    pub fn call(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.primary()?;
        loop {
            if let Some(token) = self.next_token() {
                if token.value == TokenValue::LeftParen {
                    expr = self.finish_call(expr)?;
                    continue;
                } else {
                    self.rewind_token(token);
                }
            }
            break Ok(expr);
        }
    }

    // arguments      → expression ( "," expression )* ;
    fn finish_call(&mut self, callee: Expr) -> Result<Expr, LoxError> {
        let mut arguments = Vec::with_capacity(16);
        let mut next_argument = true;
        loop {
            if let Some(token) = self.next_token() {
                match &token.value {
                    TokenValue::RightParen => {
                        // We will allow trailing ',' in function call arguments list
                        //if next_argument && !arguments.is_empty() {
                        //    return Err(self.error(
                        //        "Unexpected trailing ',' in function call arguments list",
                        //        None,
                        //    ));
                        //}
                        return Ok(Expression::call(callee, arguments));
                    }
                    TokenValue::Comma => {
                        if next_argument {
                            return Err(
                                self.error("Expected function call argument, found ','", None)
                            );
                        }
                        next_argument = true;
                    }
                    _ => {
                        if !next_argument {
                            return Err(self.error(
                                format!(
                                    "Expected ',' or ')' after function call argument, found {} '{}'",
                                    token.value.token_type(),
                                    token.value
                                ),
                                Some(token),
                            ));
                        }
                        self.rewind_token(token);
                        if arguments.len() >= 255 {
                            return Err(self.error(
                                "Function call with more than 255 arguments not supported",
                                None,
                            ));
                        }
                        arguments.push(self.expression()?);
                        next_argument = false;
                    }
                }
            } else {
                return Err(self.error_eof(
                    "Expected ',' or ')' after function call argument, found EOF",
                    None,
                ));
            }
        }
    }

    // primary → IDENTIFIER | NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    pub fn primary(&mut self) -> Result<Expr, LoxError> {
        if let Some(token) = self.next_token() {
            return match &token.value {
                TokenValue::Identifier(identifier) => Ok(Expression::variable(identifier.clone())),
                TokenValue::Nil => Ok(Expression::literal(LiteralValue::Nil)),
                TokenValue::True => Ok(Expression::literal(LiteralValue::Boolean(true))),
                TokenValue::False => Ok(Expression::literal(LiteralValue::Boolean(false))),
                TokenValue::Number(num) => {
                    match num.parse() {
                        Ok(num) => Ok(Expression::literal(LiteralValue::Number(num))),
                        Err(e) => Err(self
                            .error(format!("Error parsing '{num}' as number: {e}"), Some(token))),
                    }
                }
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
                        Err(self.error_eof("Expected ')' after expression, found EOF", Some(token)))
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
        Err(self.error_eof("Expected expression, found EOF", None))
    }
}
