//! This file is type.rs
//!
//!
//!   #[derive(Debug, Clone, PartialEq)]
//! - Object
//!     - Literal
//!     - Caller
//!
//!
//!   #[derive(Debug, Clone, PartialEq)]
//! - Literal
//!     - I32
//!     - F64
//!     - Str
//!     - Bool
//!     - Null
//!
//!   #[derive(Debug, Clone, PartialEq)]
//! - Caller: impl Callable
//!     - Function
//!
//!
//! - Function
//!     - NativeFunction
//!     - UserFunction
//!
use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
    rc::Rc,
};

use super::{
    abort::{AbortError, ControlFlowAbort},
    ast::{ExprStmt, FunStmt, Lambda as LambdaExpr, Stmt},
    callable::{CallError, Callable, StructError},
    env::Env,
    error::JokerError,
    interpreter::Interpreter,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone)]
pub enum Literal {
    I32(i32),
    F64(f64),
    Str(String),
    Bool(bool),
    Null,
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Literal::I32(a), Literal::I32(b)) => a == b,
            (Literal::F64(a), Literal::F64(b)) => a.eq(b) || (a.is_nan() && b.is_nan()), // 特殊处理 NaN
            (Literal::Str(a), Literal::Str(b)) => a == b,
            (Literal::Bool(a), Literal::Bool(b)) => a == b,
            (Literal::Null, Literal::Null) => true,
            _ => false,
        }
    }
}

impl Eq for Literal {}

impl Hash for Literal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Literal::I32(val) => val.hash(state),
            Literal::F64(val) => {
                // 对于 f64，使用 to_bits 来确保 NaN 有相同的哈希码
                // 并且正无穷和负无穷有不同的哈希码
                val.to_bits().hash(state);
            }
            Literal::Str(val) => val.hash(state),
            Literal::Bool(val) => val.hash(state),
            Literal::Null => 0.hash(state), // 或者选择一个固定的值作为 null 的哈希码
        }
    }
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Caller {
    Func(Function),
    Lambda(Lambda),
}

impl Display for Caller {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Caller::Func(func) => Display::fmt(func, f),
            Caller::Lambda(lambda) => Display::fmt(lambda, f),
        }
    }
}

impl Callable for Caller {
    fn call(&self, interpreter: &Interpreter, arguments: &[Object]) -> Result<Object, JokerError> {
        match self {
            Caller::Func(func) => Callable::call(func, interpreter, arguments),
            Caller::Lambda(lambda) => Callable::call(lambda, interpreter, arguments),
        }
    }
    fn arity(&self) -> usize {
        match self {
            Caller::Func(func) => Callable::arity(func),
            Caller::Lambda(lambda) => Callable::arity(lambda),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

impl Eq for NativeFunction {}

impl Hash for NativeFunction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.fun).hash(state); // 使用 Rc 的指针地址作为哈希码
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
        Display::fmt(&self.fun, f)
    }
}

impl Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NativeFun(<callable>)")
    }
}

#[derive(Clone)]
pub struct UserFunction {
    stmt: Rc<FunStmt>,
    closure: Rc<RefCell<Env>>,
}

impl PartialEq for UserFunction {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.stmt, &other.stmt)
    }
}
impl Eq for UserFunction {}

impl Hash for UserFunction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.stmt).hash(state)
    }
}

impl UserFunction {
    pub fn new(stmt: &FunStmt, closure: Rc<RefCell<Env>>) -> UserFunction {
        UserFunction {
            stmt: Rc::new(stmt.clone()),
            closure,
        }
    }
}

impl Callable for UserFunction {
    fn call(&self, interpreter: &Interpreter, arguments: &[Object]) -> Result<Object, JokerError> {
        let mut fun_env: Env = Env::new_with_enclosing(self.closure.clone());

        for (name, value) in self.stmt.params.iter().zip(arguments) {
            fun_env.define(name.lexeme.clone(), value.clone());
        }
        if let Err(err) = interpreter.execute_block(&self.stmt.body, fun_env) {
            match err {
                JokerError::Abort(AbortError::ControlFlow(ControlFlowAbort::Return(
                    return_value,
                ))) => {
                    return Ok(return_value);
                }
                _ => return Err(err),
            }
        }
        Ok(Object::Literal(Literal::Null))
    }
    fn arity(&self) -> usize {
        self.stmt.params.len()
    }
}

impl Display for UserFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "UserFun({})", self.stmt.name.lexeme)
    }
}

impl Debug for UserFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "UserFun(<callable>)")
    }
}

#[derive(Clone)]
pub struct Lambda {
    expr: Rc<LambdaExpr>,
    closure: Rc<RefCell<Env>>,
}

impl PartialEq for Lambda {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.expr, &other.expr)
    }
}

impl Eq for Lambda {}

impl Hash for Lambda {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.expr).hash(state)
    }
}

impl Lambda {
    pub fn new(expr: &LambdaExpr, closure: Rc<RefCell<Env>>) -> Lambda {
        Lambda {
            expr: Rc::new(expr.clone()),
            closure,
        }
    }
}

impl Callable for Lambda {
    fn call(&self, interpreter: &Interpreter, arguments: &[Object]) -> Result<Object, JokerError> {
        let mut lambda_env: Env = Env::new_with_enclosing(self.closure.clone());

        for (name, value) in self.expr.params.iter().zip(arguments) {
            lambda_env.define(name.lexeme.clone(), value.clone());
        }

        match &*self.expr.body {
            Stmt::BlockStmt(block) => {
                if let Err(err) = interpreter.execute_block(&block.stmts, lambda_env) {
                    match err {
                        JokerError::Abort(AbortError::ControlFlow(ControlFlowAbort::Return(
                            return_value,
                        ))) => {
                            return Ok(return_value);
                        }
                        _ => return Err(err),
                    }
                }
            }
            Stmt::ExprStmt(ExprStmt { expr }) => {
                return interpreter.evaluate_local(expr, lambda_env)
            }
            _ => {
                return Err(JokerError::Call(CallError::Struct(
                    StructError::report_error(
                        &self.expr.pipe,
                        String::from("lambda structure: | params | expr '1' or block '{}'."),
                    ),
                )))
            }
        }
        Ok(Object::Literal(Literal::Null))
    }
    fn arity(&self) -> usize {
        self.expr.params.len()
    }
}

impl Display for Lambda {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Lambda(<callable>)")
    }
}

impl Debug for Lambda {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Lambda(<callable>)",)
    }
}
