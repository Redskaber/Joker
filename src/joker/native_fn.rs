//! This file is native function rs
//!
//!
use std::fmt::Display;
use std::time::SystemTime;

use super::{
    callable::Callable,
    error::{JokerError, SystemError, SystemTimeError},
    interpreter::Interpreter,
    object::{Literal, Object as OEnum},
    types::Object,
};

#[derive(Debug)]
pub struct NativeClock;
impl Callable for NativeClock {
    fn call(
        &self,
        _interpreter: &Interpreter,
        _arguments: &[Object],
    ) -> Result<Option<Object>, JokerError> {
        match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
            Ok(duration) => Ok(Some(Object::new(OEnum::Literal(Literal::F64(
                duration.as_millis() as f64,
            ))))),
            Err(err) => Err(JokerError::System(SystemError::Time(
                SystemTimeError::report_error(format!(
                    "Native clock return invalid duration: {:?}.",
                    err
                )),
            ))),
        }
    }
    fn arity(&self) -> usize {
        0
    }
}
impl Display for NativeClock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NativeClock")
    }
}
