use std::collections::{hash_map::Entry, HashMap};

use crate::interpreter::types::LoxValue;

#[derive(Default, Debug, Clone)]
pub struct Environment {
    pub enclosing: Option<Box<Environment>>,
    pub variables: HashMap<String, LoxValue>,
}

pub type Env = Box<Environment>;

impl Environment {
    pub fn new() -> Box<Self> {
        Box::new(Self::default())
    }
    pub fn enter(self: &mut Box<Self>) {
        let mut inner = Self::new();
        std::mem::swap(self, &mut inner);
        self.enclosing = Some(inner);
    }
    pub fn exit(self: &mut Box<Self>) {
        let enclosing = self.enclosing.take().expect("Tried to exit top-level Envirionment");
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
            }
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
}
