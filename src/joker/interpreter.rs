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


pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {}
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
            TokenType::BangEqual => match (l_expr, r_expr) {
                (Object::Literal(ref l_literal), Object::Literal(ref r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::Bool(l_i32 != r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::Bool(l_f64 != r_f64))),
                    (ObL::Bool(l_bool), ObL::Bool(r_bool)) => Ok(Object::Literal(ObL::Bool(l_bool != r_bool))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Object::Literal(ObL::Bool(l_str != r_str))),
                    (ObL::Null, ObL::Null) => Ok(Object::Literal(ObL::Bool(false))),
                    _ => Err(JokerError::new(
                        &expr.m_opera, 
                        String::from(format!("[[BangEqual]] The literal cannot take bang equal values. !({l_literal} != {r_literal})"))
                    ))
                    
                }
            },
            TokenType::EqualEqual => match (l_expr, r_expr) {
                (Object::Literal(ref l_literal), Object::Literal(ref r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::Bool(l_i32 == r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::Bool(l_f64 == r_f64))),
                    (ObL::Bool(l_bool), ObL::Bool(r_bool)) => Ok(Object::Literal(ObL::Bool(l_bool == r_bool))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Object::Literal(ObL::Bool(l_str == r_str))),
                    (ObL::Null, ObL::Null) => Ok(Object::Literal(ObL::Bool(true))),
                    _ => Err(JokerError::new(
                        &expr.m_opera, 
                        String::from(format!("[[BangEqual]] The literal cannot take bang equal values. !({l_literal} != {r_literal})"))
                    ))
                    
                }
            },     
            TokenType::Greater => match (l_expr, r_expr) {
                (Object::Literal(ref l_literal), Object::Literal(ref r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::Bool(l_i32 > r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::Bool(l_f64 > r_f64))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Object::Literal(ObL::Bool(l_str > r_str))),
                    _ => Err(JokerError::new(
                        &expr.m_opera, 
                        String::from(format!("[[BangEqual]] The literal cannot take bang equal values. !({l_literal} != {r_literal})"))
                    ))
                    
                }
            },
            TokenType::GreaterEqual => match (l_expr, r_expr) {
                (Object::Literal(ref l_literal), Object::Literal(ref r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::Bool(l_i32 >= r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::Bool(l_f64 >= r_f64))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Object::Literal(ObL::Bool(l_str >= r_str))),
                    _ => Err(JokerError::new(
                        &expr.m_opera, 
                        String::from(format!("[[BangEqual]] The literal cannot take bang equal values. !({l_literal} != {r_literal})"))
                    ))
                    
                }
            }, 
            TokenType::Less => match (l_expr, r_expr) {
                (Object::Literal(ref l_literal), Object::Literal(ref r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::Bool(l_i32 < r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::Bool(l_f64 < r_f64))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Object::Literal(ObL::Bool(l_str < r_str))),
                    _ => Err(JokerError::new(
                        &expr.m_opera, 
                        String::from(format!("[[BangEqual]] The literal cannot take bang equal values. !({l_literal} != {r_literal})"))
                    ))
                    
                }
            },
            TokenType::LessEqual => match (l_expr, r_expr) {
                (Object::Literal(ref l_literal), Object::Literal(ref r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::Bool(l_i32 <= r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::Bool(l_f64 <= r_f64))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Object::Literal(ObL::Bool(l_str <= r_str))),
                    _ => Err(JokerError::new(
                        &expr.m_opera, 
                        String::from(format!("[[BangEqual]] The literal cannot take bang equal values. !({l_literal} != {r_literal})"))
                    ))
                    
                }
            }, 
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
                        String::from(format!("[[Plus]] The literal cannot take plus values. !({l_literal} + {r_literal})"))
                    ))
                }
            }
            TokenType::Minus => match (l_expr, r_expr) {
                (Object::Literal(ref l_literal), Object::Literal(ref r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::I32(l_i32 - r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::F64(l_f64 - r_f64))),
                    _ => Err(JokerError::new(
                        &expr.m_opera, 
                        String::from(format!("[[Plus]] The literal cannot take plus values. !({l_literal} + {r_literal})"))
                    ))
                }
            }
            TokenType::Slash => match (l_expr, r_expr) {
                (Object::Literal(ref l_literal), Object::Literal(ref r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => {
                        if r_i32 != &0 {
                            Ok(Object::Literal(ObL::I32(l_i32 / r_i32)))
                        } else {
                            Err(JokerError::new(
                                &expr.m_opera, 
                                String::from("[[Slash]] The literal cannot take slash values. !({l_literal} / {r_literal})")
                            ))
                        }
                    },
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => {
                        if r_f64 != &0f64 {
                            Ok(Object::Literal(ObL::F64(l_f64 / r_f64)))
                        } else {
                            Err(JokerError::new(
                                &expr.m_opera, 
                                String::from("[[Slash]] The literal cannot take slash values. !({l_literal} / {r_literal})")
                            ))
                        }
                    },
                    _ => Err(JokerError::new(
                        &expr.m_opera, 
                        String::from(format!("[[Plus]] The literal cannot take plus values. !({l_literal} + {r_literal})"))
                    ))
                }
            }      
            TokenType::Star => match (l_expr, r_expr) {
                (Object::Literal(ref l_literal), Object::Literal(ref r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::I32(l_i32 * r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::F64(l_f64 * r_f64))),
                    (ObL::Str(str_), ObL::I32(i32_))
                    | (ObL::I32(i32_), ObL::Str(str_)) => {
                        let mut r_str: String = String::from(str_);
                        let mut count = *i32_ - 1;
                        while count > 0 {
                            r_str.push_str(str_);
                            count -= 1;
                        }
                        Ok(Object::Literal(ObL::Str(r_str)))
                    },
                    _ => Err(JokerError::new(
                        &expr.m_opera, 
                        String::from(format!("[[Plus]] The literal cannot take plus values. !({l_literal} + {r_literal})"))
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

