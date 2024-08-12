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
    rc::Rc,
};

use super::{
    abort::{AbortError, ControlFlowAbort, ControlFlowContext},
    ast::{ExprStmt, FunStmt, Lambda as LambdaExpr, Stmt},
    callable::{CallError, Callable, StructError},
    env::Env,
    error::JokerError,
    interpreter::Interpreter,
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

        for pos in 0..arguments.len() {
            fun_env.define(
                &self.stmt.params.get(pos).unwrap().lexeme,
                arguments.get(pos).unwrap().clone(),
            );
        }
        if let Err(err) = interpreter.execute_block(&self.stmt.body, fun_env) {
            match err {
                JokerError::Abort(AbortError::ControlFlow(ControlFlowAbort::Return(
                    return_value,
                ))) => {
                    while interpreter.control_flow_stack.borrow().last()
                        != Some(&ControlFlowContext::Fun)
                    {
                        interpreter.control_flow_stack.borrow_mut().pop();
                    }
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
        write!(f, "UserFun(<callable>)")
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

        for pos in 0..arguments.len() {
            lambda_env.define(
                &self.expr.params.get(pos).unwrap().lexeme,
                arguments.get(pos).unwrap().clone(),
            );
        }

        match &*self.expr.body {
            Stmt::BlockStmt(block) => {
                if let Err(err) = interpreter.execute_block(&block.stmts, lambda_env) {
                    match err {
                        JokerError::Abort(AbortError::ControlFlow(ControlFlowAbort::Return(
                            return_value,
                        ))) => {
                            while interpreter.control_flow_stack.borrow().last()
                                != Some(&ControlFlowContext::Fun)
                            {
                                interpreter.control_flow_stack.borrow_mut().pop();
                            }
                            return Ok(return_value);
                        }
                        _ => return Err(err),
                    }
                }
            }
            Stmt::ExprStmt(ExprStmt { expr }) => {
                let previous = interpreter.local.replace(Rc::new(RefCell::new(lambda_env)));
                let result = interpreter.evaluate(expr);
                interpreter.local.replace(previous);
                return result;
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
