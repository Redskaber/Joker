use std::{
    collections::HashMap,
    fmt::Display,
    hash::{Hash, Hasher},
};

use crate::joker::{
    callable::Callable,
    error::JokerError,
    interpreter::Interpreter,
    object::{ClassInstance, Instance, Object as OEnum, UpCast},
    types::Object,
};

use super::{Caller, MethodFunction};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    name: String,
    methods: Option<HashMap<String, MethodFunction>>,
}

impl UpCast<OEnum> for Class {
    fn upcast(&self) -> OEnum {
        OEnum::Caller(Caller::Class(self.clone()))
    }
    fn upcast_into(self) -> OEnum {
        OEnum::Caller(Caller::Class(self))
    }
}

impl Class {
    pub fn new(name: String, methods: Option<HashMap<String, MethodFunction>>) -> Class {
        Class { name, methods }
    }
    pub fn get_method(&self, name: &str) -> Option<&MethodFunction> {
        match &self.methods {
            Some(methods) => methods.get(name),
            None => None,
        }
    }
}

impl Hash for Class {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        if let Some(methods) = &self.methods {
            for (name, fun) in methods {
                name.hash(state);
                fun.hash(state);
            }
        }
    }
}

impl Callable for Class {
    fn call(
        &self,
        interpreter: &Interpreter,
        arguments: &[Object],
    ) -> Result<Option<Object>, JokerError> {
        let instance = ClassInstance::new(self.clone());
        match self.get_method("init") {
            Some(initializer) => initializer.bind(instance).call(interpreter, arguments),
            None => Ok(Some(Object::new(OEnum::Instance(Instance::Class(
                instance,
            ))))),
        }
    }
    fn arity(&self) -> usize {
        self.get_method("init")
            .map_or(0, |initializer| initializer.arity())
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Class(name: {}, methods: {:?})", self.name, self.methods)
    }
}
