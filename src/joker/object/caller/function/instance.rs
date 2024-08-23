//! This file is Method rs
//!
//! - Instance
//!
//!

use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::joker::{
    abort::{AbortError, ControlFlowAbort},
    ast::FunStmt,
    callable::Callable,
    env::{Env, EnvError},
    error::JokerError,
    interpreter::Interpreter,
    object::{Caller, Instance, Object as OEnum, UpCast},
    types::Object,
};

use super::{Binder, Function};

#[derive(Clone)]
pub struct InstanceFunction {
    stmt: Rc<FunStmt>,
    closure: Rc<RefCell<Env>>,
}

impl UpCast<OEnum> for InstanceFunction {
    fn upcast(&self) -> OEnum {
        OEnum::Caller(Caller::Func(Function::Instance(self.clone())))
    }
    fn upcast_into(self) -> OEnum {
        OEnum::Caller(Caller::Func(Function::Instance(self)))
    }
}

impl PartialEq for InstanceFunction {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.stmt, &other.stmt)
    }
}
impl Eq for InstanceFunction {}

impl Hash for InstanceFunction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.stmt).hash(state)
    }
}

impl InstanceFunction {
    pub fn new(stmt: &FunStmt, closure: Rc<RefCell<Env>>) -> InstanceFunction {
        InstanceFunction {
            stmt: Rc::new(stmt.clone()),
            closure,
        }
    }
}

impl Binder for InstanceFunction {
    // TODO: instance function ?
    fn bind(&self, instance: Instance) -> Function {
        let instance_env: Rc<RefCell<Env>> = Rc::new(RefCell::new(Env::new_with_enclosing(
            Rc::clone(&self.closure),
        )));
        instance_env.borrow_mut().define(
            String::from("this"),
            Some(Object::new(OEnum::Instance(Box::new(instance)))),
        );
        Function::Instance(InstanceFunction::new(&self.stmt, instance_env))
    }
}

impl Callable for InstanceFunction {
    fn call(
        &self,
        interpreter: &Interpreter,
        arguments: &[Object],
    ) -> Result<Option<Object>, JokerError> {
        let mut instance_env: Env = Env::new_with_enclosing(Rc::clone(&self.closure));

        if let Some(params) = &self.stmt.params {
            // get this before determine execute function bind this(self.closure),
            // so: normal execute determine success unwrap: have value, next unwrap: Object | None
            let value: Option<Object> = self
                .closure
                .borrow()
                .symbol
                .get("this")
                .ok_or(JokerError::Env(EnvError::report_error(
                    &params[0],
                    String::from("failed to retrieve 'this' from closure"),
                )))?
                .clone();
            instance_env.define(params[0].lexeme.clone(), value);
            for (name, value) in params[1..].iter().zip(arguments) {
                instance_env.define(name.lexeme.clone(), Some(value.clone()));
            }
        }
        match interpreter.execute_block(&self.stmt.body, instance_env) {
            Ok(_) => {
                if self.stmt.name.lexeme.eq("init") {
                    if let Some(this) = self.closure.borrow().symbol.get("this") {
                        return Ok(this.clone());
                    } else {
                        eprintln!("class init method return instance error.")
                    }
                }
            }
            Err(err) => match err {
                JokerError::Abort(AbortError::ControlFlow(ControlFlowAbort::Return(
                    return_value,
                ))) => {
                    if self.stmt.name.lexeme.eq("init") {
                        if let Some(this) = self.closure.borrow().symbol.get("this") {
                            return Ok(this.clone());
                        } else {
                            eprintln!("class init method return instance error.")
                        }
                    } else {
                        return Ok(return_value);
                    }
                }
                _ => return Err(err),
            },
        };
        Ok(None)
    }
    fn arity(&self) -> usize {
        self.stmt
            .params
            .as_ref()
            .map_or(0, |params| params.len() - 1) // this
    }
}

impl Display for InstanceFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InstanceFun({})", self.stmt.name.lexeme)
    }
}

impl Debug for InstanceFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InstanceFun(<callable>)")
    }
}
