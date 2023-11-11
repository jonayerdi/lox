use std::fmt::Display;

use crate::expression::Expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Print(PrintStatement),
    If(IfStatement),
    While(WhileStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
    Var(VarStatement),
}

pub type Stmt = Box<Statement>;

impl Statement {
    pub fn print(expression: Expr) -> Self {
        Self::Print(PrintStatement { expression })
    }
    pub fn if_stmt(
        condition: Expr,
        then_branch: Statement,
        else_branch: Option<Statement>,
    ) -> Self {
        Self::If(IfStatement {
            condition,
            then_branch: Box::new(then_branch),
            else_branch: else_branch.and_then(|e| Some(Box::new(e))),
        })
    }
    pub fn while_stmt(condition: Expr, body: Statement) -> Self {
        Self::While(WhileStatement {
            condition,
            body: Box::new(body),
        })
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
            Statement::If(stmt) => write!(f, "{}", stmt),
            Statement::While(stmt) => write!(f, "{}", stmt),
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
pub struct IfStatement {
    pub condition: Expr,
    pub then_branch: Stmt,
    pub else_branch: Option<Stmt>,
}

impl Display for IfStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if ({}) {}", self.condition, self.then_branch)?;
        if let Some(else_branch) = &self.else_branch {
            write!(f, " else {}", else_branch)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    pub condition: Expr,
    pub body: Stmt,
}

impl Display for WhileStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "while ({}) {}", self.condition, self.body)
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
