use std::fmt::Display;

use crate::expression::Expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Print(PrintStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
    Var(VarStatement),
}

impl Statement {
    pub fn print(expression: Expr) -> Self {
        Self::Print(PrintStatement { expression })
    }
    pub fn expression(expression: Expr) -> Self {
        Self::Expression(ExpressionStatement { expression })
    }
    pub fn block(statements: Vec<Statement>) -> Self {
        Self::Block(BlockStatement { statements })
    }
    pub fn var(identifier: String, initializer: Option<Expr>) -> Self {
        Self::Var(VarStatement {
            identifier,
            initializer,
        })
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Print(stmt) => write!(f, "{}", stmt),
            Statement::Expression(stmt) => write!(f, "{}", stmt),
            Statement::Block(stmt) => write!(f, "{}", stmt),
            Statement::Var(stmt) => write!(f, "{}", stmt),
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
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{\n")?;
        for statement in &self.statements {
            write!(f, "{}\n", statement)?;
        }
        write!(f, "}}\n")
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

#[derive(Debug, Clone, PartialEq)]
pub struct VarStatement {
    pub identifier: String,
    pub initializer: Option<Expr>,
}

impl Display for VarStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.initializer {
            Some(initializer) => write!(
                f,
                "var {identifier} = {initializer}",
                identifier = self.identifier,
            ),
            None => write!(f, "var {identifier}", identifier = self.identifier,),
        }
    }
}
