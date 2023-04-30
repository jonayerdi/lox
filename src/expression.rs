use std::fmt::Display;

use crate::{context::NO_CONTEXT, token::Token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpressionContext {
    pub token: Option<Token>,
}

impl ExpressionContext {
    pub const fn new(token: Option<Token>) -> Self {
        Self { token }
    }
}

impl Display for ExpressionContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{context}",
            context = match &self.token {
                Some(token) => token.context,
                None => NO_CONTEXT,
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(LiteralExpression),
    Grouping(GroupingExpression),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
}

pub type Expr = Box<Expression>;

impl Expression {
    pub fn literal(value: LiteralValue) -> Box<Self> {
        Box::new(Self::Literal(LiteralExpression { value }))
    }
    pub fn grouping(expression: Expr) -> Box<Self> {
        Box::new(Self::Grouping(GroupingExpression { expression }))
    }
    pub fn unary(operator: UnaryOperator, right: Expr) -> Box<Self> {
        Box::new(Self::Unary(UnaryExpression { operator, right }))
    }
    pub fn binary(left: Expr, operator: BinaryOperator, right: Expr) -> Box<Self> {
        Box::new(Self::Binary(BinaryExpression {
            left,
            operator,
            right,
        }))
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(expr) => write!(f, "{}", expr),
            Expression::Grouping(expr) => write!(f, "{}", expr),
            Expression::Unary(expr) => write!(f, "{}", expr),
            Expression::Binary(expr) => write!(f, "{}", expr),
        }
    }
}

impl From<LiteralExpression> for Expression {
    fn from(expr: LiteralExpression) -> Self {
        Expression::Literal(expr)
    }
}

impl From<GroupingExpression> for Expression {
    fn from(expr: GroupingExpression) -> Self {
        Expression::Grouping(expr)
    }
}

impl From<UnaryExpression> for Expression {
    fn from(expr: UnaryExpression) -> Self {
        Expression::Unary(expr)
    }
}

impl From<BinaryExpression> for Expression {
    fn from(expr: BinaryExpression) -> Self {
        Expression::Binary(expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralExpression {
    pub value: LiteralValue,
}

impl Display for LiteralExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct GroupingExpression {
    pub expression: Expr,
}

impl Display for GroupingExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", self.expression)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub right: Expr,
}

impl Display for UnaryExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.operator, self.right)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub left: Expr,
    pub operator: BinaryOperator,
    pub right: Expr,
}

impl Display for BinaryExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.left, self.operator, self.right)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Not,
    Neg,
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Not => write!(f, "{}", "!"),
            UnaryOperator::Neg => write!(f, "{}", "-"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    // Comparison operators
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
    // Arithmetic operators
    Add,
    Sub,
    Mul,
    Div,
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Eq => write!(f, "{}", "=="),
            BinaryOperator::Neq => write!(f, "{}", "!="),
            BinaryOperator::Lt => write!(f, "{}", "<"),
            BinaryOperator::Leq => write!(f, "{}", "<="),
            BinaryOperator::Gt => write!(f, "{}", ">"),
            BinaryOperator::Geq => write!(f, "{}", ">="),
            BinaryOperator::Add => write!(f, "{}", "+"),
            BinaryOperator::Sub => write!(f, "{}", "-"),
            BinaryOperator::Mul => write!(f, "{}", "*"),
            BinaryOperator::Div => write!(f, "{}", "/"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Nil,
    Boolean(bool),
    String(String),
    Number(f64),
}

impl Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralValue::Nil => write!(f, "nil"),
            LiteralValue::Boolean(v) => write!(f, "{}", v),
            LiteralValue::String(v) => write!(f, "\"{}\"", v),
            LiteralValue::Number(v) => write!(f, "{}", v),
        }
    }
}
