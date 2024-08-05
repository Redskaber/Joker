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
//!                     | TrinomialStmt               
//!                     | printStmt             
//!                     | varStmt ;             
//!
//!     exprStmt       → expression ";" ;
//!     printStmt      → "print" expression ";" ;
//!     varStmt        → "var" IDENTIFIER ("=" expression )? ";" ;
//!     TrinomialStmt  → expression "?" expression ":" expression ";" ;  
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
use std::fmt::Display;

use super::{error::JokerError, object::Object, token::Token};

macro_rules! define_ast {
    (
        $ast_name:ident {
            $($struct_name:ident {
                $($field:ident : $field_type:ty),* $(,)?
            }),* $(,)?
        },
        $visitor_name:ident, $param_name:ident, {
            $($visit_name:ident),* $(,)?
        },
        $acceptor_name:ident,
    ) => {
        // abstract tree enum
        #[derive(Debug)]
        pub enum $ast_name {
            $($struct_name($struct_name),)*
        }

        // subtree struct
        $(
        #[derive(Debug)]
        pub struct $struct_name {
            $(pub $field: $field_type),*
        }

        impl $struct_name {
            pub fn new($($field: $field_type),*) -> $struct_name {
                $struct_name { $($field),* }
            }
            pub fn upcast($($field: $field_type),*) -> $ast_name {
                $ast_name::$struct_name($struct_name::new($($field),*))
            }
        }
        )*

        // visitor trait
        pub trait $visitor_name<T> {
            $(fn $visit_name(&self, $param_name: &$struct_name) -> Result<T, JokerError>;)*
        }

        // accept trait
        pub trait $acceptor_name<T> {
            fn accept(&self, visitor: &dyn $visitor_name<T>) -> Result<T, JokerError>;
        }

        impl<T> $acceptor_name<T> for $ast_name {
            fn accept(&self, visitor: &dyn $visitor_name<T>) -> Result<T, JokerError> {
                match self {
                    $($ast_name::$struct_name(expr) => visitor.$visit_name(expr),)*
                }
            }
        }

        $(
        impl<T> $acceptor_name<T> for $struct_name {
            fn accept(&self, visitor: &dyn $visitor_name<T>) -> Result<T, JokerError> {
                visitor.$visit_name(self)
            }
        }
        )*

        impl Display for $ast_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $($ast_name::$struct_name(expr) => Display::fmt(expr, f)),*
                }
            }
        }
        // sub display
        $(define_ast!{@impl_display $struct_name, $($field: $field_type),*})*
    };

    (@impl_display $struct_name:ident, $($field:ident : $field_type:ty),* $(,)?) => {
        impl Display for $struct_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let mut fields = Vec::new();
                $(fields.push(format!("{}: {}", stringify!($field), self.$field).to_string());)*
                let format_args = fields.join(", ").trim_end_matches(", ").to_string();
                write!(f, "{}({})", stringify!($struct_name), format_args)
            }
        }
    };
}

define_ast! {
    Expr {
        Literal     { value: Object },
        Unary       { l_opera: Token, r_expr: Box<Expr> },
        Binary      { l_expr: Box<Expr>, m_opera: Token, r_expr: Box<Expr> },
        Grouping    { expr: Box<Expr> },
    },
    ExprVisitor,    expr, { visit_literal, visit_unary, visit_binary, visit_grouping },
    ExprAcceptor,
}

define_ast! {
    Stmt {
        ExprStmt    { expr: Expr },
        PrintStmt   { expr: Expr },
    },
    StmtVisitor,    stmt, {visit_expr, visit_print },
    StmtAcceptor,
}
