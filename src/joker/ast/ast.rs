//! This file is abstract syntax tree (AST) impl !
//!
//! Base Syntax (存在歧义):
//!     expression     → literal
//!                    | unary
//!                    | binary
//!                    | grouping ;
//!
//!     literal        → NUMBER | STRING | "true" | "false" | "nil" ;
//!     grouping       → "(" expression ")" ;
//!     unary          → ( "-" | "!" ) expression ;
//!     binary         → expression operator expression ;
//!     operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
//!                    | "+"  | "-"  | "*" | "/" ;
//!
//!
//!
//! Next Syntax RuleSet(使用优先级与结合性, 解决歧义):
//!
//!     program        → statement* EOF ;
//!     
//!     statement      → exprStmt               
//!                     | printStmt             
//!                     | varStmt ;             
//!
//!     exprStmt       → expression ";" ;
//!     printStmt      → "print" expression ";" ;
//!     varStmt        → "var" IDENTIFIER ("=" expression )? ";" ;
//!
//!     expression     → assignment ;
//!     assignment     → IDENTIFIER "=" assignment
//!                     | equality ;
//!
//!     equality       → comparison ( ( "!=" | "==" ) comparison )* ;
//!     comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
//!     term           → factor ( ( "-" | "+" ) factor )* ;
//!     factor         → unary ( ( "/" | "*" ) unary )* ;
//!     unary          → ( "!" | "-" ) unary
//!                     | grouping ;
//!
//!     grouping      -> "(" expression ")" ;
//!                     | primary ;
//!
//!     primary        → I32| F64 | STRING | "true" | "false" | "null"
//!                     | IDENTIFIER ;
//!
//!
//!
use super::super::{
    r#type::Object,
    error::JokerError,
    token::Token,
};


pub enum Expr {
    Literal(Literal),
    Unary(Unary),
    Binary(Binary),
    Grouping(Grouping),
}


pub struct Literal {
    value: Object,
}


pub struct Unary {
    l_opera: Token,
    r_expr: Box<Expr>,
}


pub struct Binary {
    l_expr: Box<Expr>,
    m_opera: Token,
    r_expr: Box<Expr>,
}


pub struct Grouping {
    expr: Box<Expr>,
}



pub trait ExprVisitor<T> {
    fn visit_literal(&self, expr: &Literal) -> Result<T, JokerError>;
    fn visit_unary(&self, expr: &Unary) -> Result<T, JokerError>;
    fn visit_binary(&self, expr: &Binary) -> Result<T, JokerError>;
    fn visit_grouping(&self, expr: &Grouping) -> Result<T, JokerError>;
}
pub trait ExprAccept<T> {
    fn accept(&self, visitor: &dyn ExprVisitor<T>) -> Result<T, JokerError>;
}



impl<T> ExprAccept<T> for Literal {
    fn accept(&self, visitor: &dyn ExprVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_literal(self)
    }
}
impl<T> ExprAccept<T> for Unary {
    fn accept(&self, visitor: &dyn ExprVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_unary(self)
    }
}
impl<T> ExprAccept<T> for Binary {
   fn accept(&self, visitor: &dyn ExprVisitor<T>) -> Result<T, JokerError> {
      visitor.visit_binary(self) 
   } 
} 
impl<T> ExprAccept<T> for Grouping {
    fn accept(&self, visitor: &dyn ExprVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_grouping(self)
    }
}






