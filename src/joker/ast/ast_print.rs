//! This file is print ast expr!
//!
//!

use super::{
    super::error::JokerError,
    Expr, ExprAccept, ExprVisitor, Literal, Unary, Binary, Grouping,
};


pub struct AstPrinter;


impl AstPrinter {
    pub fn new() -> AstPrinter {
        AstPrinter
    }
    pub fn print(&self, expr: &Expr) -> Result<String, JokerError> {
        expr.accept(self)
    }
    fn parenthesize(&self, label: &String, exprs: &[&Box<Expr>]) -> Result<String, JokerError> {
        let mut result = String::new();

        result.push('(');
        result.push_str(label);
        for expr in exprs {
            result.push(' ');
            match expr.accept(self) {
                Ok(sub_string) => result.push_str(&sub_string),
                Err(err) => return Err(err),
            }
        }
        result.push(')');

        Ok(result)
    }
}

impl ExprVisitor<String> for AstPrinter {
    fn visit_literal(&self,expr: &Literal) -> Result<String,JokerError> {
        match &expr.value {
            Some(value) => Ok(value.to_string()),
            None => Ok("null".to_string()),
        }
    }
    fn visit_unary(&self,expr: &Unary) -> Result<String,JokerError> {
        self.parenthesize(&expr.l_opera.to_string(), &[&expr.r_expr])
    }
    fn visit_binary(&self,expr: &Binary) -> Result<String,JokerError> {
        self.parenthesize(&expr.m_opera.to_string(), &[&expr.l_expr, &expr.r_expr])
    }
    fn visit_grouping(&self,expr: &Grouping) -> Result<String,JokerError> {
        self.parenthesize(&"group".to_string(), &[&expr.expr])
    }
}





