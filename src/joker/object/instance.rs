//! This file is instance rs
//!
//! - Instance
//!     - ClassInstance
//!
//!
//!

use std::{
    collections::HashMap,
    fmt::Display,
    hash::{Hash, Hasher},
};

use crate::joker::{error::JokerError, interpreter::InterpreterError, token::Token, types::Object};

use super::{Caller, Class, Function, Object as OEnum};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Instance {
    Class(ClassInstance),
}

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instance::Class(class_instance) => Display::fmt(class_instance, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassInstance {
    class: Class,
    fields: HashMap<String, Object>,
    // methods: Option<HashMap<String, InstanceFunction>>
}

impl Hash for ClassInstance {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.class.hash(state);
        for (key, value) in &self.fields {
            key.hash(state);
            value.hash(state);
        }
    }
}

impl ClassInstance {
    pub fn new(class: Class) -> ClassInstance {
        ClassInstance {
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
                            Function::Method(method.bind(self.clone())),
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

impl Display for ClassInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ClassInstance(class: {}, fields: {:?})",
            self.class, self.fields
        )
    }
}
