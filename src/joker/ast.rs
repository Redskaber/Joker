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
//!     program        → declaration* EOF ;
//!
//!     declaration    → statement         (语句）            
//!                     | var_declaration  (声明)
//!                     | fun_declaration  (声明)
//!                     | method_declaration  (声明)
//!                     | instance_declaration  (声明)
//!                     | class_declaration(声明)
//!
//!     var_decl       → "var" varStmt ;
//!     fun_decl       → "fun" funStmt ;
//!     class_decl     → "class" classStmt ;
//!     
//!
//!     statement      → exprStmt
//!                     | returnStmt
//!                     | breakStmt
//!                     | continueStmt
//!                     | funStmt
//!                     | forStmt
//!                     | ifStmt               
//!                     | printStmt
//!                     | whileStmt
//!                     | BlockStmt             
//!                     | varStmt ;             
//!
//!     exprStmt       → expression ";" ;
//!     printStmt      → "print" expression ";" ;
//!     BlockStmt      → "{" statement "}"
//!     varStmt        → "var" IDENTIFIER ("=" expression )? ";" ;
//!     ifStmt         → "if" "(" expression ")" statement ( "else" statement )? ;
//!     whileStmt      → "while" "(" expression ")" statement ;          
//!     forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
//!                      expression? ";"
//!                      expression? ")" statement ;
//!     
//!     funStmt        → "fun" IDENTIFIER  "(" parameters? ")" statement ;
//!     methodStmt     → "fun" IDENTIFIER  "(" parameter (, parameters)? ")" statement ;    parameter = cls
//!     instanceStmt   → "fun" IDENTIFIER  "(" parameter (, parameters)? ")" statement ;    parameter = self
//!     parameters     → IDENTIFIER  (, IDENTIFIER)*
//!
//!     classStmt      → "class" IDENTIFIER (":" IDENTIFIER)?  "{"
//!                             var_decl*
//!                             | fun_decl*
//!                             | method_decl*
//!                     "}" ;
//!
//!
//!      breakStmt      → "break" ";"
//!     continueStmt   → "continue" ";"
//!     returnStmt     → "return" expression? ";" ;
//!  
//!     expression     → assignment ;
//!
//!     assignment     → ( call "." )? IDENTIFIER "=" assignment
//!                     | Lambda ;
//!
//!     Lambda         → "|" parameters? "|" statement ( "(" parameters? ")" ";" )?
//!                     | Trinomial ;
//!
//!
//!     Trinomial      → expression "?" Trinomial ":" Trinomial ";" ;    
//!                     | logic_or ;
//!
//!     logic_or       → logic_and ( "or" logic_and )* ;
//!     logic_and      → equality ( "and" equality )* ;
//!
//!     equality       → comparison ( ( "!=" | "==" ) comparison )* ;
//!     comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
//!     term           → factor ( ( "-" | "+" ) factor )* ;
//!     factor         → unary ( ( "/" | "*" ) unary )* ;
//!     unary          → ( "!" | "-" ) unary
//!                     | call ;
//!
//!     call           → grouping ( "(" arguments? ")" | "." IDENTIFIER  )* ;
//!     arguments      → expression ( "," expression )* ;
//!
//!     grouping       → "(" expression ")" ;
//!                     | “super” "." IDENTIFIER
//!                     | primary ;
//!
//!     primary        → I32| F64 | STRING | "true" | "false" | "null"
//!                     | IDENTIFIER ;
//!
//!     getter      -> expr.ident
//!     setter      -> expr.ident = expr
//!
//!
//!
//!
use std::fmt::Display;

use super::{error::JokerError, object::Object as OEnum, token::Token};

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
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum $ast_name {
            $($struct_name($struct_name),)*
        }

        // subtree struct
        $(
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
            pub fn into_upcast(self) -> $ast_name {
                $ast_name::$struct_name(self)
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

    (@impl_display Call, $($field:ident: $field_type: ty),*) => {
        impl Display for Call {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "Call(callee: {}, paren: {}, arguments: {:?})",
                    self.callee,
                    self.paren,
                    self.arguments,
                )
            }
        }
    };
    (@impl_display Lambda, $($field:ident: $field_type: ty),*) => {
        impl Display for Lambda {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "Lambda(params: {:?}, body: {:?})",
                    self.params, self.body)
            }
        }
    };
    (@impl_display BlockStmt, $($field:ident: $field_type: ty),*) => {
        impl Display for BlockStmt {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "BlockStmt({:?})", $(self.$field)*)
            }
        }
    };
    (@impl_display IfStmt, $($field:ident: $field_type: ty),*) => {
        impl Display for IfStmt {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let else_branch = match &self.else_branch {
                    Some(value) => format!("Some({})", value),
                    None => String::from("None"),
                };
                write!(f, "IfStmt(condition: {}, then_branch: {}, else_branch: {})", self.condition, self.then_branch, else_branch)
            }
        }
    };
    (@impl_display ForStmt, $($field:ident: $field_type: ty),*) => {
        impl Display for ForStmt {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "ForStmt(initializer: {}, condition: {}, increment: {} , body: {})",
                    match &self.initializer {
                        Some(initializer) => format!("Some({})", initializer),
                        None => String::from("None"),
                    },
                    self.condition,
                    match &self.increment {
                        Some(increment) => format!("Some({})", increment),
                        None => String::from("None"),
                    },
                    self.body,
                )
            }
        }
    };
    (@impl_display BreakStmt, $($field:ident: $field_type: ty),*) => {
        impl Display for BreakStmt {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "BreakStmt")
            }
        }
    };
    (@impl_display ContinueStmt, $($field:ident: $field_type: ty),*) => {
        impl Display for ContinueStmt {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "ContinueStmt")
            }
        }
    };
    (@impl_display FunStmt, $($field:ident: $field_type: ty),*) => {
        impl Display for FunStmt {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "FunStmt(name: {}, params: {:?}, body: {:?})",
                    self.name, self.params, self.body)
            }
        }
    };
    (@impl_display VarStmt, $($field:ident: $field_type: ty),*) => {
        impl Display for VarStmt {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "VarStmt(name: {}, value: {})",
                    self.name,
                    match &self.value {
                        Some(expr) => format!("Some({})", expr),
                        None => String::from("None"),
                    }
                )
            }
        }
    };
    (@impl_display ReturnStmt, $($field:ident: $field_type: ty),*) => {
        impl Display for ReturnStmt {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "ReturnStmt(keyword: {}, value: {})",
                    self.keyword,
                    match &self.value {
                        Some(expr) => format!("Some({})", expr),
                        None => String::from("None"),
                    }
                )
            }
        }
    };
    (@impl_display ClassStmt, $($field:ident: $field_type: ty),*) => {
        impl Display for ClassStmt {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "ClassStmt(name: {}, super_class: {:?}, fields: {:?}, methods: {:?}, functions: {:?})",
                    self.name,
                    self.super_class,
                    self.fields,
                    self.methods,
                    self.functions,
                )
            }
        }
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
        Literal     { value: OEnum },
        Unary       { l_opera: Token, r_expr: Box<Expr> },
        Binary      { l_expr: Box<Expr>, m_opera: Token, r_expr: Box<Expr> },
        Grouping    { expr: Box<Expr> },
        Variable    { name: Token },                // right value
        Assign      { name: Token, value: Box<Expr>},
        Logical     { l_expr: Box<Expr>, m_opera: Token, r_expr: Box<Expr> },
        Trinomial   { condition: Box<Expr>, l_expr: Box<Expr>, r_expr: Box<Expr> },
        Call        { callee: Box<Expr>, paren: Token, arguments: Vec<Expr> },
        Lambda      { pipe: Token, params: Vec<Token>, body: Box<Stmt> },
        Getter      { expr: Box<Expr>, name: Token },
        Setter      { l_expr: Box<Expr>, name: Token, r_expr: Box<Expr> },
        This        { keyword: Token },
        Super       { keyword: Token, method: Token },
    },
    ExprVisitor,    expr, { visit_literal, visit_unary, visit_binary, visit_grouping ,visit_variable,
                            visit_assign, visit_logical, visit_trinomial, visit_call, visit_lambda,
                            visit_getter, visit_setter, visit_this, visit_super },
    ExprAcceptor,
}

define_ast! {
    Stmt {
        ExprStmt    { expr: Expr },
        PrintStmt   { expr: Expr },
        VarStmt     { name: Token, value: Option<Expr> },   // left value
        BlockStmt   { stmts: Vec<Stmt> },           // space
        IfStmt      { condition: Expr, then_branch: Box<Stmt>, else_branch: Option<Box<Stmt>> },
        WhileStmt   { condition: Expr, body: Box<Stmt>},
        ForStmt     { initializer: Option<Box<Stmt>>, condition: Expr, increment: Option<Expr> , body: Box<Stmt> },
        BreakStmt   { name: Token },
        ContinueStmt{ name: Token },
        FunStmt     { name: Token, params: Option<Vec<Token>>, body: Vec<Stmt> },
        ReturnStmt  { keyword: Token, value: Option<Expr> },
        ClassStmt   { name: Token, super_class: Option<Expr>, fields: Option<Vec<Stmt>>,
                        methods: Option<Vec<Stmt>>, functions: Option<Vec<Stmt>> },
    },
    StmtVisitor,    stmt, {visit_expr, visit_print, visit_var, visit_block, visit_if, visit_while ,
                            visit_for, visit_break, visit_continue, visit_fun, visit_return, visit_class },
    StmtAcceptor,
}
