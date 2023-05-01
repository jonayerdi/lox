use std::collections::HashMap;

use crate::interpreter::types::LoxValue;

#[derive(Default, Debug, Clone)]
pub struct Environment {
    pub variables: HashMap<String, LoxValue>,
}

impl Environment {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn get(&self, identifier: &str) -> Option<&LoxValue> {
        self.variables.get(identifier)
    }
    pub fn set(&mut self, identifier: impl ToString, value: LoxValue) -> Option<LoxValue> {
        self.variables.insert(identifier.to_string(), value)
    }
}
