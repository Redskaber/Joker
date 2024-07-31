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
