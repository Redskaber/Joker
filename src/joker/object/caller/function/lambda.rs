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
    abort::{AbortError, ControlFlowAbort},
    ast::{ExprStmt, Lambda as LambdaExpr, Stmt},
    callable::{CallError, Callable, StructError},
    env::Env,
    error::JokerError,
    interpreter::Interpreter,
    object::{Caller, Object as OEnum, UpCast},
    types::Object,
};

#[derive(Clone)]
pub struct Lambda {
    expr: Rc<LambdaExpr>,
    closure: Rc<RefCell<Env>>,
}

impl UpCast<OEnum> for Lambda {
    fn upcast(&self) -> OEnum {
        OEnum::Caller(Caller::Lambda(self.clone()))
    }
    fn upcast_into(self) -> OEnum {
        OEnum::Caller(Caller::Lambda(self))
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
                return Ok(Some(interpreter.evaluate_local(expr, lambda_env)?))
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
        Ok(None)
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
