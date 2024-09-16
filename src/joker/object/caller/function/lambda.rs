//! This file is lambda rs
//!
//! - Lambda
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
    ast::{ExprStmt, Lambda as LambdaExpr, Stmt},
    callable::{Callable, Error::Struct, StructError},
    env::Env,
    error::JokerError,
    interpreter::Interpreter,
    object::{Caller, Object as OEnum, UpCast},
    token::Token,
    types::{DeepClone, Object},
};

use super::Function;

#[derive(Clone)]
pub struct Lambda {
    pub expr: Rc<LambdaExpr>,
    closure: Rc<RefCell<Env>>,
}

impl DeepClone for Lambda {
    fn deep_clone(&self) -> Self {
        Lambda {
            expr: Rc::clone(&self.expr),
            closure: Rc::new(RefCell::new((*self.closure.borrow()).clone())),
        }
    }
}

impl UpCast<OEnum> for Lambda {
    fn upcast(&self) -> OEnum {
        OEnum::Caller(Caller::Func(Function::Lambda(self.clone())))
    }
    fn upcast_into(self) -> OEnum {
        OEnum::Caller(Caller::Func(Function::Lambda(self)))
    }
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
    fn call(
        &self,
        interpreter: &Interpreter,
        arguments: &[Object],
    ) -> Result<Option<Object>, JokerError> {
        let mut lambda_env: Env = Env::new_with_enclosing(Rc::clone(&self.closure));

        if let Some(params) = self.expr.params.as_ref() {
            for (name, value) in params.iter().zip(arguments) {
                lambda_env.define(
                    name.parse_ref::<Token>()?.lexeme.clone(),
                    Some(value.clone()),
                );
            }
        }
        match &*self.expr.body {
            Stmt::BlockStmt(block) => {
                if let Err(err) = interpreter.execute_block(&block.stmts, lambda_env) {
                    match err {
                        JokerError::Abort(ControlFlow(ControlFlowAbort::Return(return_value))) => {
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
                return Err(JokerError::Call(Struct(StructError::report_error(
                    &self.expr.pipe,
                    String::from("lambda structure: | params | expr '1' or block '{}'."),
                ))))
            }
        }
        Ok(None)
    }
    fn arity(&self) -> usize {
        self.expr.params.as_ref().map_or(0, |params| params.len())
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
