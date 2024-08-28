//! This file is joker types rs
//!
//!
//!

use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    fmt::{Debug, Display},
    hash::Hash,
    rc::Rc,
};

use crate::joker::ast::Call;

use super::{
    ast::{Expr, Lambda, Literal, Variable},
    callable::StructError,
    error::JokerError,
    object::{Caller, Function, Literal as ObL, Object as OEnum},
    resolver::{Resolver, ResolverError},
    token::Token,
};

pub trait DeepClone {
    fn deep_clone(&self) -> Self;
}

#[derive(Clone, PartialEq, Eq)]
pub struct Object {
    inner: Rc<RefCell<OEnum>>,
}

impl DeepClone for Object {
    fn deep_clone(&self) -> Self {
        let new_ref_cell = RefCell::new((*self.inner.borrow()).deep_clone());
        Object {
            inner: Rc::new(new_ref_cell),
        }
    }
}

impl Hash for Object {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.borrow().hash(state)
    }
}

impl Object {
    pub fn new(inner: OEnum) -> Self {
        Object {
            inner: Rc::new(RefCell::new(inner)),
        }
    }
    pub fn set(&self, inner: OEnum) {
        *self.inner.borrow_mut() = inner;
    }
    pub fn get(&self) -> Ref<OEnum> {
        self.inner.borrow()
    }
    pub fn get_mut(&self) -> RefMut<OEnum> {
        self.inner.borrow_mut()
    }
    pub fn parse<F: FromObject>(&self) -> Result<F, F::Err> {
        FromObject::from_object(self)
    }
}

pub trait FromObject: Sized {
    type Err;
    fn from_object(obj: &Object) -> Result<Self, Self::Err>;
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.inner.borrow(), f)
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Object(inner: {})", self.inner.borrow())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    I32,
    F64,
    Str,
    Bool,
    Null,
    Fn,
    Class { class_name: String },
    Instance { class_name: String },
    UserDefined(Token),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::F64 => write!(f, "f64"),
            Type::Str => write!(f, "str"),
            Type::Bool => write!(f, "bool"),
            Type::Null => write!(f, "null"),
            Type::Fn => write!(f, "Fn"),
            Type::Class { class_name } => write!(f, "class({class_name})"),
            Type::Instance { class_name } => write!(f, "instance({class_name})"),
            Type::UserDefined(name) => write!(f, "{}", name.lexeme),
        }
    }
}

pub struct TypeEnvironment {
    types: HashMap<String, Type>,
}

impl TypeEnvironment {
    pub fn new() -> Self {
        TypeEnvironment {
            types: HashMap::from([
                (String::from("i32"), Type::I32),
                (String::from("f64"), Type::F64),
                (String::from("str"), Type::Str),
                (String::from("bool"), Type::Bool),
                (String::from("null"), Type::Null),
                (String::from("Fn"), Type::Fn),
            ]),
        }
    }

    pub fn declare_type(&mut self, name: String, ty: Type) {
        self.types.insert(name, ty);
    }

    pub fn get_type(&self, name: &str) -> Option<&Type> {
        self.types.get(name)
    }
}

pub struct TypeInferrer;

impl TypeInferrer {
    // parse time:
    pub fn parse_type(type_name: Token) -> Type {
        println!("type_name: {:?}", type_name);
        match type_name.lexeme.as_str() {
            "i32" => Type::I32,
            "f64" => Type::F64,
            "str" => Type::Str,
            "bool" => Type::Bool,
            "null" => Type::Null,
            "Fn" => Type::Fn,
            // parse don't parse, move to static resolve parse.
            _ => Type::UserDefined(type_name),
        }
    }
    // resolve time:
    pub fn infer_type(resolver: &Resolver, expr: &Expr) -> Result<Type, JokerError> {
        match expr {
            Expr::Literal(Literal { value }) => match value {
                OEnum::Literal(ObL::I32(_)) => Ok(Type::I32),
                OEnum::Literal(ObL::F64(_)) => Ok(Type::F64),
                OEnum::Literal(ObL::Bool(_)) => Ok(Type::Bool),
                OEnum::Literal(ObL::Str(_)) => Ok(Type::Str),
                OEnum::Literal(ObL::Null) => Ok(Type::Null),
                OEnum::Caller(Caller::Func(Function::Native(_))) => Ok(Type::Fn),
                OEnum::Caller(Caller::Func(Function::User(_))) => Ok(Type::Fn),
                OEnum::Caller(Caller::Func(Function::Method(_))) => Ok(Type::Fn),
                OEnum::Caller(Caller::Lambda(_)) => Ok(Type::Fn),
                OEnum::Caller(Caller::Class(class)) => Ok(Type::Class {
                    class_name: class.name.clone(),
                }),
                OEnum::Instance(instance) => Ok(Type::Instance {
                    class_name: instance.class.borrow().name.clone(),
                }),
            },
            Expr::Lambda(Lambda {
                pipe: _,
                params: _,
                body: _,
            }) => Ok(Type::Fn),
            Expr::Call(Call {
                callee,
                paren: _,
                arguments: _,
            }) => TypeInferrer::infer_type(resolver, callee),
            Expr::Variable(Variable { name }) => {
                if let Some(ty) = resolver.type_env.borrow().get_type(&name.lexeme) {
                    Ok(ty.clone())
                } else {
                    Err(JokerError::Resolver(ResolverError::Struct(
                        StructError::report_error(
                            name,
                            format!("Unknown type for variable '{}'", name.lexeme),
                        ),
                    )))
                }
            }
            _ => Err(JokerError::Resolver(ResolverError::Struct(
                StructError::report_error(
                    &Token::eof(0),
                    String::from("Unsupported type inference"),
                ),
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_object_inner_change() {
        let obj = Object::new(OEnum::Literal(crate::joker::object::Literal::I32(100)));
        println!("Object: obj {:#?}", obj);
        *obj.inner.borrow_mut() = OEnum::Literal(crate::joker::object::Literal::Bool(false));
        println!("Object: obj {:#?}", obj);
        obj.set(OEnum::Literal(crate::joker::object::Literal::Bool(true)));
        println!("Object: obj {:#?}", obj);

        let en: Ref<OEnum> = obj.get();
        fn p(en: &OEnum) {
            println!("OEnum: en {}", en);
        }
        p(&en);
    }

    #[test]
    fn test_object_inner_ref() {
        let obj = Object::new(OEnum::Literal(crate::joker::object::Literal::I32(100)));
        println!(
            "Object: obj {:#?}, count: {}",
            obj,
            Rc::strong_count(&obj.inner)
        ); // 1
        let ref_obj = obj.clone(); // Rc::clone
        println!(
            "Object: ref_obj {:#?}, count: {}",
            ref_obj,
            Rc::strong_count(&ref_obj.inner)
        ); // 2
        println!(
            "Object: obj {:#?}, count: {}",
            obj,
            Rc::strong_count(&obj.inner)
        ); // 2
        assert_eq!(obj.inner, ref_obj.inner); // good
    }

    #[test]
    fn test_object_clone() {
        let obj = Object::new(OEnum::Literal(crate::joker::object::Literal::I32(100)));
        println!(
            "Object: obj {:#?}, count: {}",
            obj,
            Rc::strong_count(&obj.inner)
        ); // 100 1

        let ref_obj = obj.clone(); // Rc::clone
        println!(
            "Object: ref_obj {:#?}, count: {}",
            ref_obj,
            Rc::strong_count(&ref_obj.inner)
        ); // 100 2
        println!(
            "Object: obj {:#?}, count: {}",
            obj,
            Rc::strong_count(&obj.inner)
        ); // 100 2

        ref_obj.set(OEnum::Literal(crate::joker::object::Literal::Bool(false)));
        println!(
            "Object: ref_obj {:#?}, count: {}",
            ref_obj,
            Rc::strong_count(&ref_obj.inner)
        ); // false 2
        println!(
            "Object: obj {:#?}, count: {}",
            obj,
            Rc::strong_count(&obj.inner)
        ); // false 2
    }

    #[test]
    fn test_object_clone_ptr() {
        let obj = Object::new(OEnum::Literal(crate::joker::object::Literal::I32(100)));
        println!(
            "Object: obj {:#?}, count: {}",
            obj,
            Rc::strong_count(&obj.inner)
        ); // 100 1

        let ref_obj = obj.clone(); // Rc::clone
        println!(
            "Object: ref_obj {:#?}, count: {}",
            ref_obj,
            Rc::strong_count(&ref_obj.inner)
        ); // 100 2

        println!(
            "Object: obj {:#?}, count: {}",
            obj,
            Rc::strong_count(&obj.inner)
        ); // 100 2
        println!(
            "Object: std::ptr::addr_eq(obj, &ref_obj): {}",
            std::ptr::addr_eq(&obj, &ref_obj)
        ); // false
    }

    #[test]
    fn test_object_deep_clone() {
        let obj = Object::new(OEnum::Literal(crate::joker::object::Literal::I32(100)));
        println!(
            "Object: obj {:#?}, count: {}",
            obj,
            Rc::strong_count(&obj.inner)
        ); // 100 1

        let deep_obj = obj.deep_clone();
        println!(
            "Object: deep_obj {:#?}, count: {}",
            deep_obj,
            Rc::strong_count(&deep_obj.inner)
        ); // 100 1
        deep_obj.set(OEnum::Literal(crate::joker::object::Literal::I32(10000)));
        println!(
            "Object: deep_obj {:#?}, count: {}",
            deep_obj,
            Rc::strong_count(&deep_obj.inner)
        ); // 100 1

        println!(
            "Object: obj {:#?}, count: {}",
            obj,
            Rc::strong_count(&obj.inner)
        ); // 100 1
        println!(
            "Object: std::ptr::addr_eq(obj, &deep_obj): {}",
            std::ptr::addr_eq(&obj, &deep_obj)
        ); // false
    }
}
