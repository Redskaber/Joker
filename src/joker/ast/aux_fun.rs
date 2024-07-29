//! This file is ast build auxiliary function. 
use std::boxed::Box;
use super::{
    super::token::Token,
    ast::{Binary, Expr, Literal, Statement, Unary, Var, Variable}, Assign,
};


pub fn literal_i32<'a>(value: i32) -> Box<Expr<'a>> {
    Box::new(Expr::Literal(Literal::I32(value)))
}
pub fn literal_f64<'a>(value: f64) -> Box<Expr<'a>> {
    Box::new(Expr::Literal(Literal::F64(value)))
}
pub fn literal_str<'a>(value: &'a str) -> Box<Expr<'a>> {
    Box::new(Expr::Literal(Literal::Str(value)))
}
pub fn literal_bool<'a>(value: bool) -> Box<Expr<'a>> {
    Box::new(Expr::Literal(Literal::Bool(value)))
}
pub fn literal_null<'a>() -> Box<Expr<'a>> {
    Box::new(Expr::Literal(Literal::Null))
}
pub fn literal_variable<'a>(var_name: &'a Token<'a>) -> Box<Expr<'a>> {
    Box::new(Expr::Variable(Variable::new(var_name)))
}
pub fn unary<'a>(l_opera: &'a Token<'a>, r_expr: Box<Expr<'a>>) -> Box<Expr<'a>> {
    Box::new(Expr::Unary(Unary::new(l_opera, r_expr)))
}
pub fn assign<'a>(name: &'a Token<'a>, value: Box<Expr<'a>>) -> Box<Expr<'a>> {
    Box::new(Expr::Assign(Assign::new(name, value)))
}
pub fn binary<'a>(
    l_expr: Box<Expr<'a>>,
    m_opera: &'a Token<'a>,
    r_expr: Box<Expr<'a>>,
) -> Box<Expr<'a>> {
    Box::new(Expr::Binary(Binary::new(l_expr, m_opera, r_expr)))
}
pub fn grouping<'a>(value: Box<Expr<'a>>) -> Box<Expr<'a>> {
    Box::new(Expr::Grouping(value))
}
pub fn expr_stmt<'a>(value: Box<Expr<'a>>) -> Statement<'a> {
    Statement::Expr(*value)
}
pub fn print_stmt<'a>(value: Box<Expr<'a>>) -> Statement<'a> {
    Statement::Print(*value)
}
pub fn var_stmt<'a>(name: &'a Token<'a>, value: Box<Expr<'a>>) -> Statement<'a> {
    Statement::Var(Var::new(name, value))
}
