pub mod environment;
pub mod run;
pub mod types;

use std::{
    error::Error,
    fmt::Display,
    io::{self, stderr, stdin, stdout, BufRead, Write},
};

use crate::{
    expression::{BinaryOperator, Expression, LogicalOperator, UnaryOperator},
    result::LoxError,
    statement::Statement,
};

use self::{
    environment::{Env, Environment},
    types::LoxValue,
};

#[derive(Debug)]
pub enum InterpreterError {
    IO { error: std::io::Error },
    Statement { statement: Statement, msg: String },
    Expression { expression: Expression, msg: String },
    NativeFunction { function: String, msg: String },
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpreterError::IO { error } => write!(f, "Error during IO operation: {error}"),
            InterpreterError::Statement { statement, msg } => {
                write!(f, "Error evaluating statement \"{statement}\": {msg}")
            }
            InterpreterError::Expression { expression, msg } => {
                write!(f, "Error evaluating expression \"{expression}\": {msg}")
            }
            InterpreterError::NativeFunction { function, msg } => {
                write!(f, "Error executing native function \"{function}\": {msg}")
            }
        }
    }
}

impl Error for InterpreterError {}

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

pub struct Interpreter {
    pub env: Env,
    pub stdin: Box<dyn BufRead>,
    pub stdout: Box<dyn Write>,
    pub stderr: Box<dyn Write>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self {
            env: Environment::new_global(),
            stdin: Box::new(stdin().lock()),
            stdout: Box::new(stdout().lock()),
            stderr: Box::new(stderr().lock()),
        }
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_stdin(mut self, stdin: impl BufRead + 'static) -> Self {
        self.stdin = Box::new(stdin);
        self
    }

    pub fn with_stdout(mut self, stdout: impl Write + 'static) -> Self {
        self.stdout = Box::new(stdout);
        self
    }

    pub fn with_stderr(mut self, stderr: impl Write + 'static) -> Self {
        self.stderr = Box::new(stderr);
        self
    }

    pub fn interpret(&mut self, statements: &[Statement]) -> Result<()> {
        for statement in statements {
            self.execute_statement(statement)?;
        }
        Ok(())
    }

    pub fn execute_statement(&mut self, statement: &Statement) -> Result<()> {
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
                writeln!(self.stdout, "{}", value)?;
                Ok(())
            }
            Statement::If(if_stmt) => {
                let condition = self.evaluate_expression(&if_stmt.condition)?;
                let condition = type_err(
                    statement,
                    condition.boolean_value(),
                    "if statement requires boolean condition",
                )?;
                if condition {
                    self.execute_statement(&if_stmt.then_branch)?;
                } else if let Some(else_branch) = &if_stmt.else_branch {
                    self.execute_statement(else_branch)?;
                }
                Ok(())
            }
            Statement::While(while_stmt) => loop {
                let condition = self.evaluate_expression(&while_stmt.condition)?;
                let condition = type_err(
                    statement,
                    condition.boolean_value(),
                    "while statement requires boolean condition",
                )?;
                if condition {
                    self.execute_statement(&while_stmt.body)?;
                } else {
                    return Ok(());
                }
            },
            Statement::Expression(expr_stmt) => {
                let _value = self.evaluate_expression(&expr_stmt.expression)?;
                //writeln!(output, "{}", value)?;
                Ok(())
            }
            Statement::Block(block_stmt) => {
                self.env.enter();
                for statement in &block_stmt.statements {
                    self.execute_statement(statement)?;
                }
                self.env.exit();
                Ok(())
            }
            Statement::Var(var_stmt) => {
                let value = match &var_stmt.initializer {
                    Some(initializer) => self.evaluate_expression(initializer)?,
                    None => LoxValue::Nil,
                };
                self.env.set(&var_stmt.identifier, value);
                Ok(())
            }
            Statement::Function(function_stmt) => {
                let identifier = function_stmt.identifier.to_string();
                let function_stmt = function_stmt.clone();
                let function = function_stmt.into();
                let function = LoxValue::Function(function);
                self.env.set(identifier, function);
                Ok(())
            }
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
                        msg: format!("Undefined variable \"{}\"", &variable_expression.identifier),
                    }),
                }
            }
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
            }
            Expression::Logical(logical_expression) => {
                let left = self.evaluate_expression(&logical_expression.left)?;
                match logical_expression.operator {
                    LogicalOperator::And => {
                        let condition = type_err(
                            expression,
                            left.boolean_value(),
                            "Operator and requires boolean condition",
                        )?;
                        if !condition {
                            return Ok(left);
                        }
                    }
                    LogicalOperator::Or => {
                        let condition = type_err(
                            expression,
                            left.boolean_value(),
                            "Operator or requires boolean condition",
                        )?;
                        if condition {
                            return Ok(left);
                        }
                    }
                }
                self.evaluate_expression(&logical_expression.right)
            }
            Expression::Grouping(grouping_expression) => {
                self.evaluate_expression(&grouping_expression.expression)
            }
            Expression::Unary(unary_expression) => {
                //let make_err = |msg| InterpreterError::Expression { expression, msg };
                let right_value = self.evaluate_expression(&unary_expression.right)?;
                match unary_expression.operator {
                    UnaryOperator::Not => Ok(LoxValue::Boolean(!type_err(
                        expression,
                        right_value.boolean_value(),
                        "Operator ! requires boolean operand",
                    )?)),
                    UnaryOperator::Neg => Ok(LoxValue::Number(-type_err(
                        expression,
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
                            expression,
                            left_value.numeric_value(),
                            "Operator < requires numeric operands",
                        )? < type_err(
                            expression,
                            right_value.numeric_value(),
                            "Operator < requires numeric operands",
                        )?,
                    )),
                    BinaryOperator::Leq => Ok(LoxValue::Boolean(
                        type_err(
                            expression,
                            left_value.numeric_value(),
                            "Operator <= requires numeric operands",
                        )? <= type_err(
                            expression,
                            right_value.numeric_value(),
                            "Operator <= requires numeric operands",
                        )?,
                    )),
                    BinaryOperator::Gt => Ok(LoxValue::Boolean(
                        type_err(
                            expression,
                            left_value.numeric_value(),
                            "Operator > requires numeric operands",
                        )? > type_err(
                            expression,
                            right_value.numeric_value(),
                            "Operator > requires numeric operands",
                        )?,
                    )),
                    BinaryOperator::Geq => Ok(LoxValue::Boolean(
                        type_err(
                            expression,
                            left_value.numeric_value(),
                            "Operator >= requires numeric operands",
                        )? >= type_err(
                            expression,
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
                                expression,
                                left_value.numeric_value(),
                                "Operator + requires numeric or string operands",
                            )? + type_err(
                                expression,
                                right_value.numeric_value(),
                                "Operator + requires numeric or string operands",
                            )?,
                        )),
                    },
                    BinaryOperator::Sub => Ok(LoxValue::Number(
                        type_err(
                            expression,
                            left_value.numeric_value(),
                            "Operator - requires numeric operands",
                        )? - type_err(
                            expression,
                            right_value.numeric_value(),
                            "Operator - requires numeric operands",
                        )?,
                    )),
                    BinaryOperator::Mul => Ok(LoxValue::Number(
                        type_err(
                            expression,
                            left_value.numeric_value(),
                            "Operator * requires numeric operands",
                        )? * type_err(
                            expression,
                            right_value.numeric_value(),
                            "Operator * requires numeric operands",
                        )?,
                    )),
                    BinaryOperator::Div => Ok(LoxValue::Number(
                        type_err(
                            expression,
                            left_value.numeric_value(),
                            "Operator / requires numeric operands",
                        )? / type_err(
                            expression,
                            right_value.numeric_value(),
                            "Operator / requires numeric operands",
                        )?,
                    )),
                }
            }
            Expression::Call(call_expression) => {
                let callee = self.evaluate_expression(&call_expression.callee)?;
                let arguments_iter = call_expression
                    .arguments
                    .iter()
                    .map(|argument| self.evaluate_expression(argument));
                let mut arguments = Vec::with_capacity(16);
                for argument in arguments_iter {
                    arguments.push(argument?);
                }
                let function = type_err(
                    expression,
                    callee.function_value(),
                    "Call expression requires callee to be a function",
                )?;
                if arguments.len() == function.arity() {
                    function.call(self, arguments)
                } else {
                    Err(InterpreterError::Expression {
                        expression: expression.clone(),
                        msg: format!("Function \"{function}\" requires {arity} arguments, but {provided} were provided", arity=function.arity(), provided=arguments.len()),
                    })
                }
            }
        }
    }
}
