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
    Variable(VariableExpression),
    Assignment(AssignmentExpression),
    Logical(LogicalExpression),
    Grouping(GroupingExpression),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
    Call(CallExpression),
}

pub type Expr = Box<Expression>;

impl Expression {
    pub fn literal(value: LiteralValue) -> Box<Self> {
        Box::new(Self::Literal(LiteralExpression { value }))
    }
    pub fn variable(identifier: String) -> Box<Self> {
        Box::new(Self::Variable(VariableExpression { identifier }))
    }
    pub fn assignment(identifier: String, value: Expr) -> Box<Self> {
        Box::new(Self::Assignment(AssignmentExpression { identifier, value }))
    }
    pub fn logical(left: Expr, operator: LogicalOperator, right: Expr) -> Box<Self> {
        Box::new(Self::Logical(LogicalExpression {
            left,
            operator,
            right,
        }))
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
    pub fn call(callee: Expr, arguments: Vec<Expr>) -> Box<Self> {
        Box::new(Self::Call(CallExpression { callee, arguments }))
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(expr) => write!(f, "{}", expr),
            Expression::Variable(expr) => write!(f, "{}", expr),
            Expression::Assignment(expr) => write!(f, "{}", expr),
            Expression::Logical(expr) => write!(f, "{}", expr),
            Expression::Grouping(expr) => write!(f, "{}", expr),
            Expression::Unary(expr) => write!(f, "{}", expr),
            Expression::Binary(expr) => write!(f, "{}", expr),
            Expression::Call(expr) => write!(f, "{}", expr),
        }
    }
}

impl From<LiteralExpression> for Expression {
    fn from(expr: LiteralExpression) -> Self {
        Expression::Literal(expr)
    }
}

impl From<VariableExpression> for Expression {
    fn from(expr: VariableExpression) -> Self {
        Expression::Variable(expr)
    }
}

impl From<AssignmentExpression> for Expression {
    fn from(expr: AssignmentExpression) -> Self {
        Expression::Assignment(expr)
    }
}

impl From<LogicalExpression> for Expression {
    fn from(expr: LogicalExpression) -> Self {
        Expression::Logical(expr)
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

impl From<CallExpression> for Expression {
    fn from(expr: CallExpression) -> Self {
        Expression::Call(expr)
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
pub struct VariableExpression {
    pub identifier: String,
}

impl Display for VariableExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.identifier)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentExpression {
    pub identifier: String,
    pub value: Expr,
}

impl Display for AssignmentExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.identifier, self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LogicalExpression {
    pub left: Expr,
    pub operator: LogicalOperator,
    pub right: Expr,
}

impl Display for LogicalExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.left, self.operator, self.right)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicalOperator {
    And,
    Or,
}

impl Display for LogicalOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicalOperator::And => write!(f, "{}", "and"),
            LogicalOperator::Or => write!(f, "{}", "or"),
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

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub callee: Expr,
    pub arguments: Vec<Expr>,
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.callee)?;
        for argument in self.arguments.iter().take(self.arguments.len() - 1) {
            write!(f, "{argument},")?;
        }
        if let Some(argument) = self.arguments.last() {
            write!(f, "{argument}")?;
        }
        write!(f, ")")
    }
}
