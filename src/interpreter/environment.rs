use std::collections::{hash_map::Entry, HashMap};

use crate::interpreter::types::{LoxFunction, LoxValue};
use crate::interpreter::InterpreterError;

#[derive(Debug, Clone)]
pub struct Environment {
    pub enclosing: Option<Box<Environment>>,
    pub variables: HashMap<String, LoxValue>,
}

pub type Env = Box<Environment>;

impl Environment {
    pub fn new_global() -> Box<Self> {
        Box::new(Self {
            enclosing: Default::default(),
            variables: Self::builtin_variables(),
        })
    }
    pub fn new_empty() -> Box<Self> {
        Box::new(Self {
            enclosing: Default::default(),
            variables: Default::default(),
        })
    }
    pub fn enter(self: &mut Box<Self>) {
        let mut inner = Self::new_empty();
        std::mem::swap(self, &mut inner);
        self.enclosing = Some(inner);
    }
    pub fn exit(self: &mut Box<Self>) {
        let enclosing = self
            .enclosing
            .take()
            .expect("Tried to exit top-level Envirionment");
        drop(std::mem::replace(self, enclosing));
    }
    pub fn set(&mut self, identifier: impl ToString, value: LoxValue) -> Option<LoxValue> {
        self.variables.insert(identifier.to_string(), value)
    }
    pub fn get(&self, identifier: &str) -> Option<&LoxValue> {
        match self.variables.get(identifier) {
            Some(v) => Some(v),
            None => match &self.enclosing {
                Some(env) => env.get(identifier),
                None => None,
            },
        }
    }
    pub fn assign(&mut self, identifier: impl ToString, value: LoxValue) -> Result<(), ()> {
        match self.variables.entry(identifier.to_string()) {
            Entry::Occupied(mut entry) => {
                entry.insert(value);
                Ok(())
            }
            _ => match &mut self.enclosing {
                Some(env) => env.assign(identifier, value),
                None => Err(()),
            },
        }
    }
    pub fn builtin_variables() -> HashMap<String, LoxValue> {
        let mut variables = HashMap::with_capacity(32);
        variables.extend(
            Self::builtin_functions().map(|f| (f.name().to_string(), LoxValue::Function(f))),
        );
        variables
    }
    pub fn builtin_functions() -> impl Iterator<Item = LoxFunction> {
        [
            LoxFunction::new(
                "clock",
                0,
                |_interpreter, _args| match std::time::SystemTime::now()
                    .duration_since(std::time::SystemTime::UNIX_EPOCH)
                {
                    Ok(time) => Ok(LoxValue::Number(time.as_secs_f64())),
                    Err(error) => Err(InterpreterError::NativeFunction {
                        function: format!("clock"),
                        msg: format!("{error}"),
                    }),
                },
            ),
            LoxFunction::new("exit", 0, |_interpreter, _args| std::process::exit(0)),
        ]
        .into_iter()
    }
}
