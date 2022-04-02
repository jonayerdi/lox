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
    pub expression: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub right: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub left: Expr,
    pub operator: BinaryOperator,
    pub right: Expr,
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
