use std::fmt::Display;

use crate::expression::Expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Print(PrintStatement),
    Expression(ExpressionStatement),
}

impl Statement {
    pub fn print(expression: Expr) -> Self {
        Self::Print(PrintStatement { expression })
    }
    pub fn expression(expression: Expr) -> Self {
        Self::Expression(ExpressionStatement { expression })
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Print(stmt) => write!(f, "{}", stmt),
            Statement::Expression(stmt) => write!(f, "{}", stmt),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrintStatement {
    pub expression: Expr,
}

impl Display for PrintStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "print {}", self.expression)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    pub expression: Expr,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression)
    }
}
