//! This file is ast execute expression.
//! 

use super::{
    super::{token::TokenType, env::GlobalDataEnv,error::{RuntimeError, eval_error_new}},
    ast::{Statement, Var, Variable, Expr, Binary, Literal, Unary},
};

impl<'a> Statement<'a> {
    // Explain yourself: get eval
    pub fn execute<'d>(&self, env: &'d mut GlobalDataEnv<'a>) -> Result<(), RuntimeError> {
        match self{
            Statement::Expr(expr) => {
                match  expr.execute(env) {
                    Ok(_) => Ok(()),
                    Err(err) => Err(err),
                }
            },
            Statement::Print(print) => {
                match print.execute(env) {
                    Ok(value) => {
                        println!("stmt: {}", value);
                        Ok(())
                    },
                    Err(err) => Err(err),
                }
            },
            Statement::Var(left_var) => {
                match left_var.execute(env) {
                    Ok(_) => Ok(()),
                    Err(err) => Err(err), 
                }
            },
        }
    }
}


// left var 
impl<'a> Var<'a> {
    // Explain yourself: get eval
    pub fn execute<'d>(&self, env: &'d mut GlobalDataEnv<'a>) -> Result<(), RuntimeError> {
        match self.value.execute(env) {
            Ok(literal) => {   // set global var-val
                env.define_var(self.name.lexeme, literal);
                Ok(())
            },
            Err(err) => return Err(err),
        }
    }
}

impl<'a> Expr<'a> {
    // Explain yourself: get eval
    pub fn execute<'d>(&self, env: &'d mut GlobalDataEnv<'a>) -> Result<Literal<'a>, RuntimeError> {
        match self {
            Expr::Literal(literal) => literal.execute(),
            Expr::Unary(unary) => unary.execute(env),
            Expr::Binary(binary) => binary.execute(env),
            Expr::Variable(right_var) => right_var.execute(env),
            Expr::Grouping(expr) => expr.execute(env),
        }
    }
}

// Explain yourself: get eval
impl<'a> Literal<'a> {
    pub fn execute(&self) -> Result<Literal<'a>, RuntimeError> {
        Ok(*self)
    }
}
impl<'a> Unary<'a> {
    pub fn execute<'d>(&self, env: &'d mut GlobalDataEnv<'a>) -> Result<Literal<'a>, RuntimeError> {
        let value = self.r_expr.execute(env)?;
        match self.l_opera.token_type {
            TokenType::Bang => match value {
                Literal::Bool(bool_) => Ok(Literal::Bool(!bool_)),
                _ => Err(eval_error_new(
                    self.l_opera,
                    String::from(format!("This value '{}' cannot be !{}", value, value))
                )),
            },
            TokenType::Minus => match value {
                Literal::I32(i32_) => Ok(Literal::I32(-i32_)),
                Literal::F64(f64_) => Ok(Literal::F64(-f64_)),
                _ => Err(eval_error_new(
                    self.l_opera,
                    String::from(format!("This value '{}' cannot be -{}", value, value)))),
            },
            _ => Err(eval_error_new(
                self.l_opera,
                String::from(format!(
                    "This operator '{}' cannot operate value {}", 
                    self.l_opera, value
                )),
            )),
        }
    }
}

impl<'a> Binary<'a> {
        // Explain yourself: get eval
    pub fn execute<'d>(&self, env: &'d mut GlobalDataEnv<'a>) -> Result<Literal<'a>, RuntimeError> {
        let l_value: Literal = self.l_expr.execute(env)?;
        let r_value: Literal = self.r_expr.execute(env)?;
        match self.m_opera.token_type {
            TokenType::BangEqual => match (l_value, r_value) {
                (Literal::I32(l_i32), Literal::I32(r_i32)) => Ok(Literal::Bool(l_i32 != r_i32)),
                (Literal::F64(l_f64), Literal::F64(r_f64)) => Ok(Literal::Bool(l_f64 != r_f64)),
                (Literal::Str(l_str), Literal::Str(r_str)) => Ok(Literal::Bool(l_str != r_str)),
                (Literal::Null, Literal::Null) => Ok(Literal::Bool(false)),
                (Literal::Null, _) => Ok(Literal::Bool(true)),
                (_, Literal::Null) => Ok(Literal::Bool(true)),
                _ => Err(eval_error_new(
                    self.m_opera,
                    String::from(format!(
                        "[TypeMismatch] This operator '{}' cannot operate: {} {} {}",
                        self.m_opera, l_value, self.m_opera, r_value
                    ))
                ))
            },
            TokenType::EqualEqual => match (l_value, r_value) {
                (Literal::I32(l_i32), Literal::I32(r_i32)) => Ok(Literal::Bool(l_i32 == r_i32)),
                (Literal::F64(l_f64), Literal::F64(r_f64)) => Ok(Literal::Bool(l_f64 == r_f64)),
                (Literal::Str(l_str), Literal::Str(r_str)) => Ok(Literal::Bool(l_str == r_str)),
                (Literal::Null, Literal::Null) => Ok(Literal::Bool(true)),
                (Literal::Null, _) => Ok(Literal::Bool(false)),
                (_, Literal::Null) => Ok(Literal::Bool(false)),
                _ => Err(eval_error_new(
                    self.m_opera,
                    String::from(format!(
                        "[TypeMismatch] This operator '{}' cannot operate: {} {} {}",
                        self.m_opera, l_value, self.m_opera, r_value
                    ))
                )),
            },
            TokenType::Greater => match (l_value, r_value) {
                (Literal::I32(l_i32), Literal::I32(r_i32)) => Ok(Literal::Bool(l_i32 > r_i32)),
                (Literal::F64(l_f64), Literal::F64(r_f64)) => Ok(Literal::Bool(l_f64 > r_f64)),
                (Literal::Str(l_str), Literal::Str(r_str)) => Ok(Literal::Bool(l_str > r_str)),
                _ => Err(eval_error_new(
                    self.m_opera,
                    String::from(format!(
                        "[TypeMismatch] This operator '{}' cannot operate: {} {} {}",
                        self.m_opera, l_value, self.m_opera, r_value
                    )),
                )),
            },
            TokenType::GreaterEqual => match (l_value, r_value) {
                (Literal::I32(l_i32), Literal::I32(r_i32)) => Ok(Literal::Bool(l_i32 >= r_i32)),
                (Literal::F64(l_f64), Literal::F64(r_f64)) => Ok(Literal::Bool(l_f64 >= r_f64)),
                (Literal::Str(l_str), Literal::Str(r_str)) => Ok(Literal::Bool(l_str >= r_str)),
                _ => Err(eval_error_new(
                    self.m_opera,
                    String::from(format!(
                        "[TypeMismatch] This operator '{}' cannot operate: {} {} {}",
                        self.m_opera, l_value, self.m_opera, r_value
                    )),
                )),
            },
            TokenType::Less => match (l_value, r_value) {
                (Literal::I32(l_i32), Literal::I32(r_i32)) => Ok(Literal::Bool(l_i32 < r_i32)),
                (Literal::F64(l_f64), Literal::F64(r_f64)) => Ok(Literal::Bool(l_f64 < r_f64)),
                (Literal::Str(l_str), Literal::Str(r_str)) => Ok(Literal::Bool(l_str < r_str)),
                _ => Err(eval_error_new(
                    self.m_opera,
                    String::from(format!(
                        "[TypeMismatch] This operator '{}' cannot operate: {} {} {}",
                        self.m_opera, l_value, self.m_opera, r_value
                    )),
                )),
            },
            TokenType::LessEqual => match (l_value, r_value) {
                (Literal::I32(l_i32), Literal::I32(r_i32)) => Ok(Literal::Bool(l_i32 <= r_i32)),
                (Literal::F64(l_f64), Literal::F64(r_f64)) => Ok(Literal::Bool(l_f64 <= r_f64)),
                (Literal::Str(l_str), Literal::Str(r_str)) => Ok(Literal::Bool(l_str <= r_str)),
                _ => Err(eval_error_new(
                    self.m_opera,
                    String::from(format!(
                        "[TypeMismatch] This operator '{}' cannot operate: {} {} {}",
                        self.m_opera, l_value, self.m_opera, r_value
                    )),
                )),
            },
            TokenType::Plus => match (l_value, r_value) {
                (Literal::I32(l_i32), Literal::I32(r_i32)) => Ok(Literal::I32(l_i32 + r_i32)),
                (Literal::F64(l_f64), Literal::F64(r_f64)) => Ok(Literal::F64(l_f64 + r_f64)),
                // pending...
                (Literal::I32(l_i32), Literal::F64(r_f64)) => {
                    Ok(Literal::F64(l_i32 as f64 + r_f64))
                }
                (Literal::F64(l_f64), Literal::I32(r_i32)) => {
                    Ok(Literal::F64(l_f64 + r_i32 as f64))
                }
                // String + => next local language impl, this is &str
                _ => Err(eval_error_new(
                    self.m_opera,
                    String::from(format!(
                        "[TypeMismatch] This operator '{}' cannot operate: {} {} {}",
                        self.m_opera, l_value, self.m_opera, r_value
                    )),
                )),
            },
            TokenType::Minus => match (l_value, r_value) {
                (Literal::I32(l_i32), Literal::I32(r_i32)) => Ok(Literal::I32(l_i32 - r_i32)),
                (Literal::F64(l_f64), Literal::F64(r_f64)) => Ok(Literal::F64(l_f64 - r_f64)),
                _ => Err(eval_error_new(
                    self.m_opera,
                    String::from(format!(
                        "[TypeMismatch] This operator '{}' cannot operate: {} {} {}",
                        self.m_opera, l_value, self.m_opera, r_value
                    )),
                )),
            },
            TokenType::Slash => match (l_value, r_value) {
                (Literal::I32(l_i32), Literal::I32(r_i32)) => {
                    if r_i32 != 0 {
                        Ok(Literal::F64(l_i32 as f64 / r_i32 as f64))
                    } else {
                        Err(eval_error_new(
                            self.m_opera,
                            String::from(format!(
                                "[SlashZeroError] This operator '{}' cannot operate: {} {} {}",
                                self.m_opera, l_value, self.m_opera, r_value
                            )),
                        ))
                    }
                }
                (Literal::F64(l_f64), Literal::F64(r_f64)) => {
                    if r_f64 != 0.0 {
                        Ok(Literal::F64(l_f64 / r_f64))
                    } else {
                        Err(eval_error_new(
                            self.m_opera,
                            String::from(format!(
                                "[SlashZeroError] This operator '{}' cannot operate: {} {} {}",
                                self.m_opera, l_value, self.m_opera, r_value
                            )),
                        ))
                    }
                }
                _ => Err(eval_error_new(
                    self.m_opera,
                    String::from(format!(
                        "[TypeMismatch] This operator '{}' cannot operate: {} {} {}",
                        self.m_opera, l_value, self.m_opera, r_value
                    )),
                )),
            },
            TokenType::Star => match (l_value, r_value) {
                (Literal::I32(l_i32), Literal::I32(r_i32)) => Ok(Literal::I32(l_i32 * r_i32)),
                (Literal::F64(l_f64), Literal::F64(r_f64)) => Ok(Literal::F64(l_f64 * r_f64)),
                (Literal::I32(l_i32), Literal::F64(r_f64)) => {
                    Ok(Literal::F64(l_i32 as f64 * r_f64))
                }
                (Literal::F64(l_f64), Literal::I32(r_i32)) => {
                    Ok(Literal::F64(l_f64 * r_i32 as f64))
                }
                _ => Err(eval_error_new(
                    self.m_opera,
                    String::from(format!(
                        "[TypeMismatch] This operator '{}' cannot operate: {} {} {}",
                        self.m_opera, l_value, self.m_opera, r_value
                    )),
                )),
            },
            _ => Err(eval_error_new(
                self.m_opera,
                String::from(format!(
                    "[InvalidBinaryOperation] This operator '{}' cannot operate: {} {} {}",
                    self.m_opera, l_value, self.m_opera, r_value
                )),
            )),
        }
    }
}


// right var
impl<'a> Variable<'a> {
    pub fn execute<'d>(&self, env: &'d mut GlobalDataEnv<'a>) -> Result<Literal<'a>, RuntimeError> {
        match env.get_var_value(self.name) {
            Ok(value) => Ok(*value),
            Err(err) => Err(RuntimeError::DataEnvError(err)), 
        }
    }
}

