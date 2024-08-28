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
    parse::ParserError,
    token::Token,
    types::{DeepClone, FromObject, Object},
};

use super::{Binder, BinderFunction, Caller, Function, MethodFunction, UserFunction};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    pub name: String,
    pub super_class: Option<Box<Class>>,
    pub fields: Option<HashMap<String, Option<Object>>>,
    methods: Option<HashMap<String, MethodFunction>>,
    functions: Option<HashMap<String, UserFunction>>,
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
        super_class: Option<Box<Class>>,
        fields: Option<HashMap<String, Option<Object>>>,
        methods: Option<HashMap<String, MethodFunction>>,
        functions: Option<HashMap<String, UserFunction>>,
    ) -> Class {
        Class {
            name,
            super_class,
            fields,
            methods,
            functions,
        }
    }
    pub fn get_field(&self, name: &str) -> Option<&Option<Object>> {
        match &self.fields {
            Some(fields) => fields.get(name),
            None => match &self.super_class {
                Some(super_class) => super_class.get_field(name),
                None => None,
            },
        }
    }
    // get instance used
    pub fn get_method(&self, name: &str) -> Option<BinderFunction> {
        if let Some(methods) = &self.methods {
            if let Some(md) = methods.get(name) {
                return Some(BinderFunction::Method(md.clone()));
            }
        }
        if name.eq("init") {
            if let Some(super_class) = &self.super_class {
                return super_class.get_method(name);
            }
            return None;
        }
        if let Some(functions) = &self.functions {
            if let Some(func) = functions.get(name) {
                return Some(BinderFunction::User(func.clone()));
            }
        }

        if let Some(super_class) = &self.super_class {
            return super_class.get_method(name);
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

        if let Some(methods) = &self.methods {
            if let Some(md) = methods.get(&name.lexeme) {
                return Ok(Some(Object::new(OEnum::Caller(Caller::Func(
                    Function::Method(md.clone()),
                )))));
            }
        }

        if let Some(functions) = &self.functions {
            if let Some(func) = functions.get(&name.lexeme) {
                return Ok(Some(Object::new(OEnum::Caller(Caller::Func(
                    Function::User(func.clone()),
                )))));
            }
        }

        if let Some(super_class) = &self.super_class {
            return super_class.getter(name);
        }

        Ok(None)
    }
}

impl Hash for Class {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.super_class.hash(state);
        if let Some(fields) = &self.fields {
            for (name, obj) in fields {
                name.hash(state);
                obj.hash(state);
            }
        }
        if let Some(methods) = &self.methods {
            for (name, md) in methods {
                name.hash(state);
                md.hash(state);
            }
        }
        if let Some(functions) = &self.functions {
            for (name, fun) in functions {
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
        let super_name = match &self.super_class {
            Some(super_obj) => &super_obj.name,
            None => "None",
        };
        write!(
            f,
            "Class(name: {}, super_class: {}, fields: {:?}, methods: {:?}, functions: {:?})",
            self.name, super_name, self.fields, self.methods, self.functions
        )
    }
}

impl FromObject for Class {
    type Err = JokerError;
    fn from_object(obj: &Object) -> Result<Self, Self::Err> {
        if let OEnum::Caller(Caller::Class(class)) = &*obj.get() {
            Ok(*class.clone())
        } else {
            Err(JokerError::Parser(ParserError::report_error(
                &Token::eof(0),
                format!("object '{}' don't translate to class.", obj),
            )))
        }
    }
}
