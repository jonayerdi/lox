use std::{fmt::Display, rc::Rc};

use crate::expression::LiteralValue;

use crate::interpreter::{Interpreter, InterpreterError};
use crate::statement::FunctionStatement;

#[derive(Debug, Clone)]
pub enum LoxValue {
    Nil,
    Boolean(bool),
    String(String),
    Number(f64),
    Function(LoxFunction),
    //Object(ObjectData),
}

impl LoxValue {
    pub fn new_nil() -> LoxValue {
        LoxValue::Nil
    }

    pub fn new_boolean(value: bool) -> LoxValue {
        LoxValue::Boolean(value)
    }

    pub fn new_string(value: String) -> LoxValue {
        LoxValue::String(value)
    }

    pub fn new_number(value: f64) -> LoxValue {
        LoxValue::Number(value)
    }

    pub fn boolean_value(&self) -> Result<bool, String> {
        match self {
            LoxValue::Nil => Ok(false),
            LoxValue::Boolean(value) => Ok(*value),
            _ => Ok(true),
        }
    }

    pub fn numeric_value(&self) -> Result<f64, String> {
        if let Self::Number(num) = self {
            Ok(*num)
        } else {
            Err(format!("Cannot convert {self} to number"))
        }
    }

    pub fn function_value(&self) -> Result<LoxFunction, String> {
        if let Self::Function(function) = self {
            Ok(function.clone())
        } else {
            Err(format!("Cannot convert {self} to function"))
        }
    }
}

impl PartialEq for LoxValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) => true,
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Function(l0), Self::Function(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl From<LiteralValue> for LoxValue {
    fn from(literal_value: LiteralValue) -> Self {
        match literal_value {
            LiteralValue::Nil => Self::Nil,
            LiteralValue::Boolean(b) => Self::Boolean(b),
            LiteralValue::String(s) => Self::String(s),
            LiteralValue::Number(n) => Self::Number(n),
        }
    }
}

impl Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxValue::Nil => write!(f, "nil"),
            LoxValue::Boolean(v) => write!(f, "{}", v),
            LoxValue::String(v) => write!(f, "\"{}\"", v),
            LoxValue::Number(v) => write!(f, "{}", v),
            LoxValue::Function(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Clone)]
pub struct LoxFunction {
    function_name: String,
    function_arity: usize,
    function: Rc<dyn Fn(&mut Interpreter, Vec<LoxValue>) -> Result<LoxValue, InterpreterError>>,
}

impl LoxFunction {
    pub fn new(
        name: impl ToString,
        arity: usize,
        function: impl Fn(&mut Interpreter, Vec<LoxValue>) -> Result<LoxValue, InterpreterError>
            + 'static,
    ) -> Self {
        Self {
            function_name: name.to_string(),
            function_arity: arity,
            function: Rc::new(function),
        }
    }
    pub fn name(&self) -> &str {
        &self.function_name
    }
    pub fn arity(&self) -> usize {
        self.function_arity
    }
    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxValue>,
    ) -> Result<LoxValue, InterpreterError> {
        (self.function)(interpreter, arguments)
    }
}

impl From<FunctionStatement> for LoxFunction {
    fn from(value: FunctionStatement) -> Self {
        LoxFunction::new(
            value.identifier,
            value.parameters.len(),
            move |interpreter, args| {
                interpreter.env.enter();
                for (param, arg) in value.parameters.iter().zip(args) {
                    interpreter.env.set(param, arg);
                }
                let retval = match interpreter.execute_statement(&value.body)? {
                    std::ops::ControlFlow::Continue(()) => LoxValue::Nil,
                    std::ops::ControlFlow::Break(retval) => retval,
                };
                interpreter.env.exit();
                Ok(retval)
            },
        )
    }
}

impl Display for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fun {}>", self.name())
    }
}

impl std::fmt::Debug for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fun {}>", self.name())
    }
}

impl PartialEq for LoxFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name()
    }
}

impl Eq for LoxFunction {}
