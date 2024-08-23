//! This file is instance rs
//!
//! - Instance
//!     - Instance
//!
//!
//!

use std::{
    collections::HashMap,
    fmt::Display,
    hash::{Hash, Hasher},
};

use crate::joker::{error::JokerError, interpreter::InterpreterError, token::Token, types::Object};

use super::{Binder, Caller, Class, Object as OEnum};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instance {
    class: Box<Class>,
    fields: HashMap<String, Object>,
}

impl Hash for Instance {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.class.hash(state);
        for (key, value) in &self.fields {
            key.hash(state);
            value.hash(state);
        }
    }
}

impl Instance {
    pub fn new(class: Box<Class>) -> Instance {
        Instance {
            class,
            fields: HashMap::new(),
        }
    }
    pub fn getter(&self, name: &Token) -> Result<Option<Object>, JokerError> {
        match self.fields.get(&name.lexeme) {
            Some(ins_value) => Ok(Some(ins_value.clone())),
            None => match self.class.get_field(&name.lexeme) {
                Some(cls_value) => match cls_value {
                    Some(value) => Ok(Some(value.clone())),
                    None => Err(JokerError::Interpreter(InterpreterError::report_error(
                        name,
                        format!(
                            "class attribute '{}' is declared, but not define.",
                            name.lexeme
                        ),
                    ))),
                },
                None => {
                    if let Some(method) = self.class.get_method(&name.lexeme) {
                        return Ok(Some(Object::new(OEnum::Caller(Caller::Func(
                            method.bind(self.clone()),
                        )))));
                    }
                    Ok(None)
                }
            },
        }
    }
    pub fn setter(&mut self, name: &str, value: Object) -> Result<(), JokerError> {
        self.fields.insert(name.to_string(), value);
        Ok(())
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Instance(class: {}, fields: {:?})",
            self.class, self.fields
        )
    }
}
