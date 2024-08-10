//! This file is type.rs
//!
//!
//!
use std::{
    fmt::{Debug, Display},
    rc::Rc,
};

use super::{
    ast::FunStmt, callable::Callable, env::Env, error::JokerError, interpreter::Interpreter,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Literal(Literal),
    Caller(Caller),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Literal(literal) => Display::fmt(literal, f),
            Object::Caller(caller) => Display::fmt(caller, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    I32(i32),
    F64(f64),
    Str(String),
    Bool(bool),
    Null,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::I32(i32_) => write!(f, "{i32_}"),
            Literal::F64(f64_) => write!(f, "{f64_}"),
            Literal::Str(str_) => write!(f, "\"{str_}\""),
            Literal::Bool(bool_) => write!(f, "{bool_}"),
            Literal::Null => write!(f, "null"),
        }
    }
}

pub fn literal_str(text: String) -> Object {
    Object::Literal(Literal::Str(text))
}

pub fn literal_f64(f64_: f64) -> Object {
    Object::Literal(Literal::F64(f64_))
}

pub fn literal_i32(i32_: i32) -> Object {
    Object::Literal(Literal::I32(i32_))
}

pub fn literal_bool(bool_: bool) -> Object {
    Object::Literal(Literal::Bool(bool_))
}

pub fn literal_null() -> Object {
    Object::Literal(Literal::Null)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Caller {
    Func(Function),
}
impl Display for Caller {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Caller::Func(func) => Display::fmt(func, f),
        }
    }
}
impl Callable for Caller {
    fn call(&self, interpreter: &Interpreter, arguments: &[Object]) -> Result<Object, JokerError> {
        match self {
            Caller::Func(func) => Callable::call(func, interpreter, arguments),
        }
    }
    fn arity(&self) -> usize {
        match self {
            Caller::Func(func) => Callable::arity(func),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Function {
    Native(NativeFunction),
    User(UserFunction),
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Native(native) => Display::fmt(native, f),
            Function::User(user) => Display::fmt(user, f),
        }
    }
}
impl Callable for Function {
    fn call(&self, interpreter: &Interpreter, arguments: &[Object]) -> Result<Object, JokerError> {
        match self {
            Function::Native(native) => Callable::call(native, interpreter, arguments),
            Function::User(user) => Callable::call(user, interpreter, arguments),
        }
    }
    fn arity(&self) -> usize {
        match self {
            Function::Native(native) => Callable::arity(native),
            Function::User(user) => Callable::arity(user),
        }
    }
}

#[derive(Clone)]
pub struct NativeFunction {
    pub fun: Rc<dyn Callable>,
}
impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.fun, &other.fun)
    }
}

impl NativeFunction {
    pub fn new(builder: impl Fn() -> Self) -> NativeFunction {
        builder()
    }
}
impl Callable for NativeFunction {
    fn call(&self, interpreter: &Interpreter, arguments: &[Object]) -> Result<Object, JokerError> {
        self.fun.call(interpreter, arguments)
    }
    fn arity(&self) -> usize {
        self.fun.arity()
    }
}

impl Display for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NativeFun(<callable>)")
    }
}

impl Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NativeFun({})", self.fun)
    }
}

#[derive(Clone)]
pub struct UserFunction {
    fun_stmt: Rc<FunStmt>,
}
impl PartialEq for UserFunction {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.fun_stmt, &other.fun_stmt)
    }
}

impl UserFunction {
    pub fn new(fun_stmt: &FunStmt) -> UserFunction {
        UserFunction {
            fun_stmt: Rc::new(fun_stmt.clone()),
        }
    }
}
impl Callable for UserFunction {
    fn call(&self, interpreter: &Interpreter, arguments: &[Object]) -> Result<Object, JokerError> {
        let mut fun_env: Env = Env::new_with_enclosing(Rc::clone(&interpreter.global));

        for pos in 0..arguments.len() {
            fun_env.define(
                &self.fun_stmt.params.get(pos).unwrap().lexeme,
                arguments.get(pos).unwrap().clone(),
            );
        }
        interpreter.execute_block(&self.fun_stmt.body, fun_env)?;
        Ok(Object::Literal(Literal::Null))
    }
    fn arity(&self) -> usize {
        self.fun_stmt.params.len()
    }
}

impl Display for UserFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "UserFun(<callable>)")
    }
}

impl Debug for UserFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "UserFun({})", self.fun_stmt)
    }
}
