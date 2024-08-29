//! This file is Method rs
//!
//! - Method
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
    ast::FnStmt,
    callable::Callable,
    env::Env,
    error::JokerError,
    interpreter::Interpreter,
    object::{Caller, Instance, Object as OEnum, UpCast},
    types::{DeepClone, Object},
};

use super::{Binder, Function};

#[derive(Clone)]
pub struct MethodFunction {
    stmt: Rc<FnStmt>,
    closure: Rc<RefCell<Env>>,
}

impl DeepClone for MethodFunction {
    fn deep_clone(&self) -> Self {
        MethodFunction {
            stmt: Rc::clone(&self.stmt),
            closure: Rc::new(RefCell::new((*self.closure.borrow()).clone())),
        }
    }
}

impl UpCast<OEnum> for MethodFunction {
    fn upcast(&self) -> OEnum {
        OEnum::Caller(Caller::Func(Function::Method(self.clone())))
    }
    fn upcast_into(self) -> OEnum {
        OEnum::Caller(Caller::Func(Function::Method(self)))
    }
}

impl PartialEq for MethodFunction {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.stmt, &other.stmt)
    }
}
impl Eq for MethodFunction {}

impl Hash for MethodFunction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.stmt).hash(state)
    }
}

impl MethodFunction {
    pub fn new(stmt: &FnStmt, closure: Rc<RefCell<Env>>) -> MethodFunction {
        MethodFunction {
            stmt: Rc::new(stmt.clone()),
            closure,
        }
    }
}

impl Binder for MethodFunction {
    // TODO: instance function ?
    fn bind(&self, instance: Instance) -> Function {
        let instance_env: Rc<RefCell<Env>> = Rc::new(RefCell::new(Env::new_with_enclosing(
            Rc::clone(&self.closure),
        )));
        instance_env.borrow_mut().define(
            String::from("this"),
            Some(Object::new(OEnum::Instance(Box::new(instance)))),
        );
        Function::Method(MethodFunction::new(&self.stmt, instance_env))
    }
}

impl Callable for MethodFunction {
    fn call(
        &self,
        interpreter: &Interpreter,
        arguments: &[Object],
    ) -> Result<Option<Object>, JokerError> {
        let mut instance_env: Env = Env::new_with_enclosing(Rc::clone(&self.closure));

        if let Some(params) = &self.stmt.params {
            for (name, value) in params[1..].iter().zip(arguments) {
                instance_env.define(name.param().lexeme.clone(), Some(value.clone()));
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

impl Display for MethodFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "MethodFun({})", self.stmt.name.lexeme)
    }
}

impl Debug for MethodFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "MethodFun(<callable>)")
    }
}
