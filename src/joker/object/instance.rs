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

use crate::joker::{error::JokerError, types::Object};

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
    pub fn getter(&self, name: &str) -> Option<Object> {
        match self.fields.get(name) {
            Some(ins_value) => Some(ins_value.clone()),
            None => match self.class.get_field(name) {
                Some(cls_value) => Some(cls_value.clone()),
                None => self.class.get_method(name).map(|method| {
                    Object::new(OEnum::Caller(Caller::Func(Function::Method(
                        method.bind(self.clone()),
                    ))))
                }),
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
