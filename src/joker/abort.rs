//! This file is Flow handle
//!
//!
use super::error::ReportError;
use std::fmt::Display;

#[derive(Debug)]
pub enum AbortError {
    ControlFlow(ControlFlowAbort),
}

impl ReportError for AbortError {
    fn report(&self) {
        match &self {
            AbortError::ControlFlow(control_flow) => ReportError::report(control_flow),
        }
    }
}

#[derive(Debug)]
pub enum ControlFlowContext {
    Loop,
}

#[derive(Debug)]
pub enum ControlFlowAbort {
    Break,
    Continue,
}
impl Display for ControlFlowAbort {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ControlFlowAbort::Break => write!(f, "Break"),
            ControlFlowAbort::Continue => write!(f, "Continue"),
        }
    }
}
impl ReportError for ControlFlowAbort {
    fn report(&self) {
        eprintln!("{self}");
    }
}
