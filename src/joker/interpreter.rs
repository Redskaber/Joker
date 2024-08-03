//! This file is interpreters.rs
//!
//!

use super::{
    ast::{
        Binary, Expr, ExprAcceptor, ExprVisitor, Grouping, Literal, Unary
    }, 
    error::JokerError, 
    object::{Object, Literal as ObL}, 
    token::TokenType,
};


pub struct Interpreter {

}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {

        }
    }

    pub fn evaluate(&self, expr: &Expr) -> Result<Object, JokerError> {
        expr.accept(self)
    }
}

impl ExprVisitor<Object> for Interpreter {
    fn visit_literal(&self,expr: &Literal) -> Result<Object,JokerError> {
        Ok(expr.value.clone())
    }
    fn visit_unary(&self,expr: &Unary) -> Result<Object,JokerError> {
        let r_expr: Object = self.evaluate(&expr.r_expr)?;
        match expr.l_opera.ttype {
            TokenType::Minus => match r_expr {
                Object::Literal(literal) => match literal {
                    ObL::I32(i32_) => Ok(Object::Literal(ObL::I32(-i32_))),
                    ObL::F64(f64_) => Ok(Object::Literal(ObL::F64(-f64_))),
                    _ => Err(JokerError::new(
                        &expr.l_opera, 
                        String::from(format!("The literal cannot take negative values. {} !=> -{}", literal, literal))
                    ))
                },
            },
            TokenType::Bang => match r_expr {
                Object::Literal(ref literal) => match literal {
                    ObL::Bool(bool_) => Ok(Object::Literal(ObL::Bool(!bool_))),
                    _ => Err(JokerError::new(
                        &expr.l_opera, 
                        String::from(format!("The literal cannot take reversed values. {} !=> !{}", literal, literal))
                    ))
                }
            }
            _ => Err(JokerError::new(&expr.l_opera, String::from("Unreachable according to Literal Num!")))
        }
    }
    fn visit_binary(&self,expr: &Binary) -> Result<Object,JokerError> {
        let l_expr: Object = self.evaluate(&expr.l_expr)?;
        let r_expr: Object = self.evaluate(&expr.r_expr)?;
        match expr.m_opera.ttype {
            TokenType::Plus => match (l_expr, r_expr) {
                (Object::Literal(ref l_literal), Object::Literal(ref r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::I32(l_i32 + r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::F64(l_f64 + r_f64))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => {
                        let mut str_: String = String::from(l_str);
                        str_.push_str(r_str);
                        Ok(Object::Literal(ObL::Str(str_)))
                    },
                    _ => Err(JokerError::new(
                        &expr.m_opera, 
                        String::from(format!("The literal cannot take plus values. !({l_literal} + {r_literal})"))
                    ))
                }
            }
            _ => Err(JokerError::new(&expr.m_opera, String::from("Unreachable according to Literal Num!")))
        }
    }
    fn visit_grouping(&self,expr: &Grouping) -> Result<Object,JokerError> {
        self.evaluate(&expr.expr)
    }
}










