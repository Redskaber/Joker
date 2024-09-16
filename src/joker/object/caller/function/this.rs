//! This file is Function rs
//!
//! - Function
//!     - Native
//!     - User
//!     - Method
//!
//!

use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::joker::{
    abort::{ControlFlowAbort, Error::ControlFlow},
    ast::FnStmt,
    callable::Callable,
    env::Env,
    error::JokerError,
    interpreter::Interpreter,
    object::{Caller, Instance, Object as OEnum, UpCast},
    token::Token,
    types::{DeepClone, Object},
};

use super::{Lambda, MethodFunction};

pub trait Binder {
    fn bind(&self, instance: Instance) -> Function;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Function {
    Native(NativeFunction),
    User(UserFunction),
    Method(MethodFunction),
    Lambda(Lambda),
}

impl DeepClone for Function {
    fn deep_clone(&self) -> Self {
        match self {
            Function::Native(native) => Function::Native(DeepClone::deep_clone(native)),
            Function::User(user) => Function::User(DeepClone::deep_clone(user)),
            Function::Method(method) => Function::Method(DeepClone::deep_clone(method)),
            Function::Lambda(lambda) => Function::Lambda(DeepClone::deep_clone(lambda)),
        }
    }
}

impl UpCast<OEnum> for Function {
    fn upcast(&self) -> OEnum {
        match self {
            Function::Native(native) => UpCast::upcast(native),
            Function::User(user) => UpCast::upcast(user),
            Function::Method(method) => UpCast::upcast(method),
            Function::Lambda(lambda) => UpCast::upcast(lambda),
        }
    }
    fn upcast_into(self) -> OEnum {
        match self {
            Function::Native(native) => UpCast::upcast_into(native),
            Function::User(user) => UpCast::upcast_into(user),
            Function::Method(method) => UpCast::upcast_into(method),
            Function::Lambda(lambda) => UpCast::upcast_into(lambda),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Native(native) => Display::fmt(native, f),
            Function::User(user) => Display::fmt(user, f),
            Function::Method(method) => Display::fmt(method, f),
            Function::Lambda(lambda) => Display::fmt(lambda, f),
        }
    }
}

impl Callable for Function {
    fn call(
        &self,
        interpreter: &Interpreter,
        arguments: &[Object],
    ) -> Result<Option<Object>, JokerError> {
        match self {
            Function::Native(native) => Callable::call(native, interpreter, arguments),
            Function::User(user) => Callable::call(user, interpreter, arguments),
            Function::Method(method) => Callable::call(method, interpreter, arguments),
            Function::Lambda(lambda) => Callable::call(lambda, interpreter, arguments),
        }
    }
    fn arity(&self) -> usize {
        match self {
            Function::Native(native) => Callable::arity(native),
            Function::User(user) => Callable::arity(user),
            Function::Method(method) => Callable::arity(method),
            Function::Lambda(lambda) => Callable::arity(lambda),
        }
    }
}

#[derive(Clone)]
pub struct NativeFunction {
    pub fun: Rc<dyn Callable>,
}

impl DeepClone for NativeFunction {
    fn deep_clone(&self) -> Self {
        self.clone()
    }
}

impl UpCast<OEnum> for NativeFunction {
    fn upcast(&self) -> OEnum {
        OEnum::Caller(Caller::Func(Function::Native(self.clone())))
    }
    fn upcast_into(self) -> OEnum {
        OEnum::Caller(Caller::Func(Function::Native(self)))
    }
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
    fn call(
        &self,
        interpreter: &Interpreter,
        arguments: &[Object],
    ) -> Result<Option<Object>, JokerError> {
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
    pub stmt: Rc<FnStmt>,
    closure: Rc<RefCell<Env>>,
}

impl DeepClone for UserFunction {
    fn deep_clone(&self) -> Self {
        UserFunction {
            stmt: Rc::clone(&self.stmt),
            closure: Rc::new(RefCell::new((*self.closure.borrow()).clone())),
        }
    }
}

impl UpCast<OEnum> for UserFunction {
    fn upcast(&self) -> OEnum {
        OEnum::Caller(Caller::Func(Function::User(self.clone())))
    }
    fn upcast_into(self) -> OEnum {
        OEnum::Caller(Caller::Func(Function::User(self)))
    }
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
    pub fn new(stmt: &FnStmt, closure: Rc<RefCell<Env>>) -> UserFunction {
        UserFunction {
            stmt: Rc::new(stmt.clone()),
            closure,
        }
    }
}

impl Binder for UserFunction {
    // TODO: static function ?
    // class static function inner, not have this.
    fn bind(&self, _instance: Instance) -> Function {
        let instance_env: Rc<RefCell<Env>> = Rc::new(RefCell::new(Env::new_with_enclosing(
            Rc::clone(&self.closure),
        )));
        Function::User(UserFunction::new(&self.stmt, instance_env))
    }
}

impl Callable for UserFunction {
    fn call(
        &self,
        interpreter: &Interpreter,
        arguments: &[Object],
    ) -> Result<Option<Object>, JokerError> {
        let mut fun_env: Env = Env::new_with_enclosing(self.closure.clone());

        if let Some(params) = self.stmt.params.as_ref() {
            for (name, value) in params.iter().zip(arguments) {
                fun_env.define(
                    name.parse_ref::<Token>()?.lexeme.clone(),
                    Some(value.clone()),
                );
            }
        }
        if let Err(err) = interpreter.execute_block(&self.stmt.body, fun_env) {
            match err {
                JokerError::Abort(ControlFlow(ControlFlowAbort::Return(return_value))) => {
                    return Ok(return_value);
                }
                _ => return Err(err),
            }
        }
        Ok(None)
    }
    fn arity(&self) -> usize {
        self.stmt.params.as_ref().map_or(0, |params| params.len())
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinderFunction {
    User(UserFunction),
    Method(MethodFunction),
}

impl UpCast<OEnum> for BinderFunction {
    fn upcast(&self) -> OEnum {
        match self {
            BinderFunction::User(user) => UpCast::upcast(user),
            BinderFunction::Method(method) => UpCast::upcast(method),
        }
    }
    fn upcast_into(self) -> OEnum {
        match self {
            BinderFunction::User(user) => UpCast::upcast_into(user),
            BinderFunction::Method(method) => UpCast::upcast_into(method),
        }
    }
}

impl Callable for BinderFunction {
    fn call(
        &self,
        interpreter: &Interpreter,
        arguments: &[Object],
    ) -> Result<Option<Object>, JokerError> {
        match self {
            BinderFunction::User(user) => Callable::call(user, interpreter, arguments),
            BinderFunction::Method(method) => Callable::call(method, interpreter, arguments),
        }
    }
    fn arity(&self) -> usize {
        match self {
            BinderFunction::User(user) => Callable::arity(user),
            BinderFunction::Method(method) => Callable::arity(method),
        }
    }
}

impl Binder for BinderFunction {
    fn bind(&self, instance: Instance) -> Function {
        match self {
            BinderFunction::User(user) => Binder::bind(user, instance),
            BinderFunction::Method(method) => Binder::bind(method, instance),
        }
    }
}

impl Display for BinderFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinderFunction::User(user) => Display::fmt(user, f),
            BinderFunction::Method(method) => Display::fmt(method, f),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::joker::native_fn;

    use super::*;
    #[test]
    #[should_panic = "left: Caller(Func(Native(NativeFun(<callable>))))\n right: Caller(Func(Native(NativeFun(<callable>)))"]
    fn test_translator_object_from_caller_function() {
        let n_fun = NativeFunction::new(|| NativeFunction {
            fun: Rc::new(native_fn::NativeClock {}),
        });
        // should panic fun pointer neq
        assert_eq!(
            OEnum::Caller(Caller::Func(Function::Native(NativeFunction::new(|| {
                NativeFunction {
                    fun: Rc::new(native_fn::NativeClock {}),
                }
            })))),
            n_fun.upcast()
        );
    }
}
