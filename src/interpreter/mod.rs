pub mod types;

use thiserror::Error;

use crate::{
    expression::{BinaryOperator, Expression, UnaryOperator},
    result::LoxError,
};

use self::types::LoxValue;

#[derive(Error, Debug)]
pub enum InterpreterError {
    #[error("Error evaluating expression \"{expression}\": {msg}")]
    Expression { expression: Expression, msg: String },
    #[error("Error: {msg}")]
    Other { msg: String },
}

impl From<InterpreterError> for LoxError {
    fn from(error: InterpreterError) -> Self {
        LoxError::other(error)
    }
}

pub type Result<T> = core::result::Result<T, InterpreterError>;

#[derive(Debug, Clone)]
pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn evaluate_expression(&mut self, expression: &Expression) -> Result<LoxValue> {
        fn type_err<T>(
            expression: &Expression,
            result: core::result::Result<T, String>,
            ctx: impl std::fmt::Display,
        ) -> Result<T> {
            let expression = expression.clone();
            result.map_err(move |msg| InterpreterError::Expression {
                expression,
                msg: format!("{ctx} -> {msg}"),
            })
        }
        let expression_clone = expression.clone();
        match expression_clone {
            Expression::Literal(literal_value) => Ok(literal_value.value.into()),
            Expression::Grouping(grouping_expression) => {
                self.evaluate_expression(&grouping_expression.expression)
            }
            Expression::Unary(unary_expression) => {
                //let make_err = |msg| InterpreterError::Expression { expression, msg };
                let right_value = self.evaluate_expression(&unary_expression.right)?;
                match unary_expression.operator {
                    UnaryOperator::Not => Ok(LoxValue::Boolean(!type_err(
                        &expression,
                        right_value.boolean_value(),
                        "Operator ! requires boolean operand",
                    )?)),
                    UnaryOperator::Neg => Ok(LoxValue::Number(-type_err(
                        &expression,
                        right_value.numeric_value(),
                        "Operator - requires numeric operand",
                    )?)),
                }
            }
            Expression::Binary(binary_expression) => {
                let left_value = self.evaluate_expression(&binary_expression.left)?;
                let right_value = self.evaluate_expression(&binary_expression.right)?;
                match binary_expression.operator {
                    BinaryOperator::Eq => Ok(LoxValue::Boolean(left_value.eq(&right_value))),
                    BinaryOperator::Neq => Ok(LoxValue::Boolean(!left_value.eq(&right_value))),
                    BinaryOperator::Lt => Ok(LoxValue::Boolean(
                        type_err(
                            &expression,
                            left_value.numeric_value(),
                            "Operator < requires numeric operands",
                        )? < type_err(
                            &expression,
                            right_value.numeric_value(),
                            "Operator < requires numeric operands",
                        )?,
                    )),
                    BinaryOperator::Leq => Ok(LoxValue::Boolean(
                        type_err(
                            &expression,
                            left_value.numeric_value(),
                            "Operator <= requires numeric operands",
                        )? <= type_err(
                            &expression,
                            right_value.numeric_value(),
                            "Operator <= requires numeric operands",
                        )?,
                    )),
                    BinaryOperator::Gt => Ok(LoxValue::Boolean(
                        type_err(
                            &expression,
                            left_value.numeric_value(),
                            "Operator > requires numeric operands",
                        )? > type_err(
                            &expression,
                            right_value.numeric_value(),
                            "Operator > requires numeric operands",
                        )?,
                    )),
                    BinaryOperator::Geq => Ok(LoxValue::Boolean(
                        type_err(
                            &expression,
                            left_value.numeric_value(),
                            "Operator >= requires numeric operands",
                        )? >= type_err(
                            &expression,
                            right_value.numeric_value(),
                            "Operator >= requires numeric operands",
                        )?,
                    )),
                    BinaryOperator::Add => match (&left_value, &right_value) {
                        (LoxValue::String(s1), LoxValue::String(s2)) => {
                            Ok(LoxValue::String(format!("{s1}{s2}")))
                        }
                        _ => Ok(LoxValue::Number(
                            type_err(
                                &expression,
                                left_value.numeric_value(),
                                "Operator + requires numeric or string operands",
                            )? + type_err(
                                &expression,
                                right_value.numeric_value(),
                                "Operator + requires numeric or string operands",
                            )?,
                        )),
                    },
                    BinaryOperator::Sub => Ok(LoxValue::Number(
                        type_err(
                            &expression,
                            left_value.numeric_value(),
                            "Operator - requires numeric operands",
                        )? - type_err(
                            &expression,
                            right_value.numeric_value(),
                            "Operator - requires numeric operands",
                        )?,
                    )),
                    BinaryOperator::Mul => Ok(LoxValue::Number(
                        type_err(
                            &expression,
                            left_value.numeric_value(),
                            "Operator * requires numeric operands",
                        )? * type_err(
                            &expression,
                            right_value.numeric_value(),
                            "Operator * requires numeric operands",
                        )?,
                    )),
                    BinaryOperator::Div => Ok(LoxValue::Number(
                        type_err(
                            &expression,
                            left_value.numeric_value(),
                            "Operator / requires numeric operands",
                        )? / type_err(
                            &expression,
                            right_value.numeric_value(),
                            "Operator / requires numeric operands",
                        )?,
                    )),
                }
            }
        }
    }
}
