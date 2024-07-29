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
use std::fmt::Debug;
use super::super::token::Token;


// used enum and struct impl abstract syntax tree.
#[derive(Debug)]
pub enum Statement<'a> {
    Expr(Expr<'a>),
    Var(Var<'a>),  // left value
    Print(Expr<'a>),  // wait future, debug used
}


#[derive(Debug)]
pub struct Var<'a> {
    pub name: &'a Token<'a>,
    pub value: Box<Expr<'a>>,
}

impl<'a> Var<'a> {
    pub fn new(name: &'a Token<'a>, value: Box<Expr<'a>>) -> Var<'a> {
        Var { name, value }
    }
}

#[derive(Debug)]
pub enum Expr<'a> {
    Literal(Literal<'a>),
    Unary(Unary<'a>),
    Assign(Assign<'a>),
    Binary(Binary<'a>),
    Variable(Variable<'a>),   // right value
    Grouping(Box<Expr<'a>>),
}

/// Literal Operator
/// limiter from token_type
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Literal<'a> {
    I32(i32),
    F64(f64),
    Str(&'a str),
    Bool(bool),
    Null,
}

// Unary Operator
//  Minus,
//  Bang,
#[derive(Debug)]
pub struct Unary<'a> {
    pub l_opera: &'a Token<'a>,   // Cow?
    pub r_expr: Box<Expr<'a>>,
}

impl<'a> Unary<'a> {
    pub fn new(l_opera: &'a Token<'a>, r_expr: Box<Expr<'a>>) -> Unary<'a> {
        Unary { l_opera, r_expr }
    }
}


#[derive(Debug)]
pub struct Assign<'a> {
    pub name: &'a Token<'a>,
    pub value: Box<Expr<'a>>,
}

impl<'a> Assign<'a> {
    pub fn new(name: &'a Token<'a>, value: Box<Expr<'a>>) -> Assign<'a> {
        Assign { name, value }
    }
}


/// Binary Operator
/// 比较运算 && 算术运算
///  wait expand...
#[derive(Debug)]
pub struct Binary<'a> {
    pub l_expr: Box<Expr<'a>>,
    pub m_opera: &'a Token<'a>,
    pub r_expr: Box<Expr<'a>>,
}

impl<'a> Binary<'a> {
    pub fn new(
        l_expr: Box<Expr<'a>>,
        m_opera: &'a Token<'a>,
        r_expr: Box<Expr<'a>>,
    ) -> Binary<'a> {
        Binary {
            l_expr,
            m_opera,
            r_expr,
        }
    }
}

#[derive(Debug)]
pub struct Variable<'a> {
    pub name: &'a Token<'a>,
}
impl<'a> Variable<'a> {
    pub fn new(name: &'a Token<'a>) -> Variable<'a> {
        Variable { name }
    }
}


// Operator Operator
// -  +  / * != == > >= < <=


