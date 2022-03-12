#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(LiteralExpression),
    Grouping(GroupingExpression),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
}

impl Expression {
    pub fn literal(value: LiteralValue) -> Box<Self> {
        Box::new(Self::Literal(LiteralExpression { value } ))
    }
    pub fn grouping(expression: Box<Expression>) -> Box<Self> {
        Box::new(Self::Grouping(GroupingExpression { expression }))
    }
    pub fn unary(operator: UnaryOperator, right: Box<Expression>) -> Box<Self> {
        Box::new(Self::Unary(UnaryExpression { operator, right }))
    }
    pub fn binary(left: Box<Expression>, operator: BinaryOperator, right: Box<Expression>) -> Box<Self> {
        Box::new(Self::Binary(BinaryExpression { left, operator, right }))
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

#[derive(Debug, Clone, PartialEq)]
pub struct GroupingExpression {
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator: BinaryOperator,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Not,
    Neg,
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

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Nil,
    Boolean(bool),
    String(String),
    Number(f64),
}
