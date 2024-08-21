//! This file is caller rs
//!
//! - Caller
//!     - Function
//!     - Lambda
//!     - Method
//!
//!

use std::fmt::Display;

use crate::joker::{
    callable::Callable,
    error::JokerError,
    interpreter::Interpreter,
    object::{Object as OEnum, UpCast},
    types::Object,
};

use super::{Class, Function, Lambda};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Caller {
    Func(Function),
    Lambda(Lambda),
    Class(Class),
}

impl UpCast<OEnum> for Caller {
    fn upcast(&self) -> OEnum {
        match self {
            Caller::Func(fun) => UpCast::upcast(fun),
            Caller::Lambda(lambda) => UpCast::upcast(lambda),
            Caller::Class(class) => UpCast::upcast(class),
        }
    }
    fn upcast_into(self) -> OEnum {
        match self {
            Caller::Func(fun) => UpCast::upcast_into(fun),
            Caller::Lambda(lambda) => UpCast::upcast_into(lambda),
            Caller::Class(class) => UpCast::upcast_into(class),
        }
    }
}

impl Display for Caller {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Caller::Func(func) => Display::fmt(func, f),
            Caller::Lambda(lambda) => Display::fmt(lambda, f),
            Caller::Class(class) => Display::fmt(class, f),
        }
    }
}

impl Callable for Caller {
    fn call(
        &self,
        interpreter: &Interpreter,
        arguments: &[Object],
    ) -> Result<Option<Object>, JokerError> {
        match self {
            Caller::Func(func) => Callable::call(func, interpreter, arguments),
            Caller::Lambda(lambda) => Callable::call(lambda, interpreter, arguments),
            Caller::Class(class) => Callable::call(class, interpreter, arguments),
        }
    }
    fn arity(&self) -> usize {
        match self {
            Caller::Func(func) => Callable::arity(func),
            Caller::Lambda(lambda) => Callable::arity(lambda),
            Caller::Class(class) => Callable::arity(class),
        }
    }
}
