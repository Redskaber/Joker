use std::{
    collections::HashMap,
    fmt::Display,
    hash::{Hash, Hasher},
};

use crate::joker::{
    callable::Callable,
    error::JokerError,
    interpreter::{Interpreter, InterpreterError},
    object::{Instance, Object as OEnum, UpCast},
    token::Token,
    types::{DeepClone, Object},
};

use super::{
    Binder, BinderFunction, Caller, Function, InstanceFunction, MethodFunction, UserFunction,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    name: String,
    fields: Option<HashMap<String, Option<Object>>>,
    class_methods: Option<HashMap<String, MethodFunction>>,
    instance_methods: Option<HashMap<String, InstanceFunction>>,
    static_methods: Option<HashMap<String, UserFunction>>,
}

impl DeepClone for Class {
    fn deep_clone(&self) -> Self {
        self.clone()
    }
}

impl DeepClone for Box<Class> {
    fn deep_clone(&self) -> Self {
        self.clone()
    }
}

impl UpCast<OEnum> for Box<Class> {
    fn upcast(&self) -> OEnum {
        OEnum::Caller(Caller::Class(self.clone()))
    }
    fn upcast_into(self) -> OEnum {
        OEnum::Caller(Caller::Class(self))
    }
}

impl Class {
    pub fn new(
        name: String,
        fields: Option<HashMap<String, Option<Object>>>,
        class_methods: Option<HashMap<String, MethodFunction>>,
        instance_methods: Option<HashMap<String, InstanceFunction>>,
        static_methods: Option<HashMap<String, UserFunction>>,
    ) -> Class {
        Class {
            name,
            fields,
            class_methods,
            instance_methods,
            static_methods,
        }
    }
    pub fn get_field(&self, name: &str) -> Option<&Option<Object>> {
        match &self.fields {
            Some(fields) => fields.get(name),
            None => None,
        }
    }
    // get instance used
    pub fn get_method(&self, name: &str) -> Option<BinderFunction> {
        if let Some(instances) = &self.instance_methods {
            if let Some(instance) = instances.get(name) {
                return Some(BinderFunction::Instance(instance.clone()));
            }
        }
        if let Some(methods) = &self.class_methods {
            if let Some(method) = methods.get(name) {
                return Some(BinderFunction::Method(method.clone()));
            }
        }
        if name.eq("init") {
            return None;
        }
        if let Some(statics) = &self.static_methods {
            if let Some(static_) = statics.get(name) {
                return Some(BinderFunction::User(static_.clone()));
            }
        }
        None
    }
    // used class find
    pub fn getter(&self, name: &Token) -> Result<Option<Object>, JokerError> {
        if let Some(fields) = &self.fields {
            if let Some(object) = fields.get(&name.lexeme) {
                match object {
                    Some(_) => return Ok(object.clone()),
                    None => {
                        return Err(JokerError::Interpreter(InterpreterError::report_error(
                            name,
                            format!(
                                "Error: class attribute '{}' is declared, but not define.",
                                name.lexeme
                            ),
                        )))
                    }
                }
            }
        }

        if let Some(class_methods) = &self.class_methods {
            if class_methods.contains_key(&name.lexeme) {
                return Err(JokerError::Interpreter(InterpreterError::report_error(
                    name,
                    format!(
                        "Error: class method '{}' is exist, but outside not visited.",
                        name.lexeme
                    ),
                )));
            }
        }

        if let Some(static_methods) = &self.static_methods {
            if let Some(static_) = static_methods.get(&name.lexeme) {
                return Ok(Some(Object::new(OEnum::Caller(Caller::Func(
                    Function::User(static_.clone()),
                )))));
            }
        }
        Ok(None)
    }
}

impl Hash for Class {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        if let Some(methods) = &self.class_methods {
            for (name, fun) in methods {
                name.hash(state);
                fun.hash(state);
            }
        }
        if let Some(instances) = &self.instance_methods {
            for (name, fun) in instances {
                name.hash(state);
                fun.hash(state);
            }
        }
        if let Some(statics) = &self.static_methods {
            for (name, fun) in statics {
                name.hash(state);
                fun.hash(state);
            }
        }
    }
}

impl Callable for Box<Class> {
    fn call(
        &self,
        interpreter: &Interpreter,
        arguments: &[Object],
    ) -> Result<Option<Object>, JokerError> {
        let instance: Instance = Instance::new(self.clone());
        match self.get_method("init") {
            Some(initializer) => initializer.bind(instance).call(interpreter, arguments),
            None => Ok(Some(Object::new(OEnum::Instance(Box::new(instance))))),
        }
    }
    fn arity(&self) -> usize {
        self.get_method("init")
            .map_or(0, |initializer| initializer.arity())
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Class(name: {}, fields: {:?}, class_methods: {:?}, instance_methods: {:?}, static_methods: {:?})", 
        self.name, self.fields, self.class_methods, self.instance_methods, self.static_methods)
    }
}
