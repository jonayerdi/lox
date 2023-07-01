pub mod environment;
pub mod run;
pub mod types;

use std::io::{self, Write};

use thiserror::Error;

use crate::{
    expression::{BinaryOperator, Expression, UnaryOperator, LogicalOperator},
    result::LoxError,
    statement::Statement,
};

use self::{environment::Env, types::LoxValue};

#[derive(Error, Debug)]
pub enum InterpreterError {
    #[error("Error during IO operation: {error}")]
    IO { error: std::io::Error },
    #[error("Error evaluating statement \"{statement}\": {msg}")]
    Statement { statement: Statement, msg: String },
    #[error("Error evaluating expression \"{expression}\": {msg}")]
    Expression { expression: Expression, msg: String },
}

impl From<io::Error> for InterpreterError {
    fn from(error: io::Error) -> Self {
        InterpreterError::IO { error }
    }
}

impl From<InterpreterError> for LoxError {
    fn from(error: InterpreterError) -> Self {
        LoxError::interpret(error)
    }
}

pub type Result<T> = core::result::Result<T, InterpreterError>;

#[derive(Default, Debug, Clone)]
pub struct Interpreter {
    pub env: Env,
}

impl Interpreter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn interpret<W: Write>(&mut self, statements: &[Statement], output: &mut W) -> Result<()> {
        for statement in statements {
            self.execute_statement(statement, output)?;
        }
        Ok(())
    }

    pub fn execute_statement<W: Write>(
        &mut self,
        statement: &Statement,
        output: &mut W,
    ) -> Result<()> {
        fn type_err<T>(
            statement: &Statement,
            result: core::result::Result<T, String>,
            ctx: impl std::fmt::Display,
        ) -> Result<T> {
            let statement = statement.clone();
            result.map_err(move |msg| InterpreterError::Statement {
                statement,
                msg: format!("{ctx} -> {msg}"),
            })
        }
        match statement {
            Statement::Print(print_stmt) => {
                let value = self.evaluate_expression(&print_stmt.expression)?;
                writeln!(output, "{}", value)?;
                Ok(())
            },
            Statement::If(if_stmt) => {
                let condition = self.evaluate_expression(&if_stmt.condition)?;
                let condition = type_err(
                    &statement,
                    condition.boolean_value(),
                    "if statement requires boolean condition",
                )?;
                if condition {
                    self.execute_statement(&if_stmt.then_branch, output)?;
                } else if let Some(else_branch) = &if_stmt.else_branch {
                    self.execute_statement(else_branch, output)?;
                }
                Ok(())
            },
            Statement::While(while_stmt) => {
                loop {
                    let condition = self.evaluate_expression(&while_stmt.condition)?;
                    let condition = type_err(
                        &statement,
                        condition.boolean_value(),
                        "while statement requires boolean condition",
                    )?;
                    if condition {
                        self.execute_statement(&while_stmt.body, output)?;
                    } else {
                        return Ok(());
                    }
                }
            },
            Statement::Expression(expr_stmt) => {
                let _value = self.evaluate_expression(&expr_stmt.expression)?;
                //writeln!(output, "{}", value)?;
                Ok(())
            },
            Statement::Block(block_stmt) => {
                self.env.enter();
                for statement in &block_stmt.statements {
                    self.execute_statement(statement, output)?;
                }
                self.env.exit();
                Ok(())
            },
            Statement::Var(var_stmt) => {
                let value = match &var_stmt.initializer {
                    Some(initializer) => self.evaluate_expression(initializer)?,
                    None => LoxValue::Nil,
                };
                self.env.set(&var_stmt.identifier, value);
                Ok(())
            },
        }
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
            Expression::Literal(literal_expression) => Ok(literal_expression.value.into()),
            Expression::Variable(variable_expression) => {
                let expression = expression.clone();
                match self.env.get(&variable_expression.identifier) {
                    Some(value) => Ok(value.clone()),
                    None => Err(InterpreterError::Expression {
                        expression,
                        msg: format!(
                            "Undefined variable \"{}\"",
                            &variable_expression.identifier
                        ),
                    }),
                }
            },
            Expression::Assignment(assignment_expression) => {
                let value = self.evaluate_expression(&assignment_expression.value)?;
                let expression = expression.clone();
                self.env
                    .assign(&assignment_expression.identifier, value.clone())
                    .map(|_| value)
                    .map_err(|_| InterpreterError::Expression {
                        expression,
                        msg: format!(
                            "Assignment to undefined variable \"{}\"",
                            &assignment_expression.identifier
                        ),
                    })
            },
            Expression::Logical(logical_expression) => {
                let left = self.evaluate_expression(&logical_expression.left)?;
                match logical_expression.operator {
                    LogicalOperator::And => {
                        let condition = type_err(
                            &expression,
                            left.boolean_value(),
                            "Operator and requires boolean condition",
                        )?;
                        if !condition {
                            return Ok(left);
                        }
                    },
                    LogicalOperator::Or => {
                        let condition = type_err(
                            &expression,
                            left.boolean_value(),
                            "Operator or requires boolean condition",
                        )?;
                        if condition {
                            return Ok(left);
                        }
                    },
                }
                self.evaluate_expression(&logical_expression.right)
            },
            Expression::Grouping(grouping_expression) => {
                self.evaluate_expression(&grouping_expression.expression)
            },
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
            },
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
            },
        }
    }
}
