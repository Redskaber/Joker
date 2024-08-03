//! This file is error.rs
//!
//!
//!

pub trait ReportError {
    fn report(&self);
}

#[derive(Debug)]
pub struct JokerError {
    line: usize,
    msg: String,
}

impl JokerError {
    pub fn error(line: usize, msg: String) -> JokerError {
        JokerError { line, msg }
    }
}

impl ReportError for JokerError {
    fn report(&self) {
        eprintln!("[line {}] where: '', \n\tmsg: {}\n", self.line, self.msg);
    }
}
