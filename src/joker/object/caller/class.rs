use std::{
    collections::HashMap,
    fmt::Display,
    hash::{Hash, Hasher},
};

use crate::joker::{
    callable::Callable,
    error::JokerError,
    interpreter::Interpreter,
    object::{Instance, Object as OEnum, UpCast},
    types::Object,
};

use super::{Binder, BinderFunction, Caller, InstanceFunction, MethodFunction, UserFunction};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    name: String,
    fields: Option<HashMap<String, Option<Object>>>,
    class_methods: Option<HashMap<String, MethodFunction>>,
    instance_methods: Option<HashMap<String, InstanceFunction>>,
    static_methods: Option<HashMap<String, UserFunction>>,
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
