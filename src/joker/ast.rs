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
//!     expression     → equality ;
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
//!     primary        → NUMBER | STRING | "true" | "false" | "null"
//!
//!
//!
use std::fmt::{Debug, Display};

pub use self::ast::*;
pub use self::aux_fun::*;
use super::{
    error::ReportError,
    token::{Token, TokenType},
};

// used enum and struct impl abstract syntax tree.
// used inner mod reason is easy to manage
pub mod ast {
    use super::*;

    #[derive(Debug)]
    pub enum Expr<'a> {
        Literal(Literal<'a>),
        Unary(Unary<'a>),
        Binary(Binary<'a>),
        Grouping(Box<Expr<'a>>),
    }
    // 求值表达式
    impl<'a> Expr<'a> {
        // Explain yourself: get eval
        pub fn evaluate(&self) -> Result<Literal<'a>, EvalError> {
            match self {
                Expr::Literal(literal) => literal.evaluate(),
                Expr::Unary(unary) => unary.evaluate(),
                Expr::Binary(binary) => binary.evaluate(),
                Expr::Grouping(expr) => expr.evaluate(),
            }
        }
    }
    // Explain yourself: build ast expr (Recursively distribute calls to your own display)
    impl<'a> Display for Expr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Expr::Literal(literal) => Display::fmt(&literal, f),
                Expr::Unary(unary) => Display::fmt(&unary, f),
                Expr::Binary(binary) => Display::fmt(&binary, f),
                Expr::Grouping(expr) => {
                    let _ = write!(f, "( group ");
                    let _ = Display::fmt(&expr, f);
                    write!(f, ")")
                }
            }
        }
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
    // Explain yourself: get eval
    impl<'a> Literal<'a> {
        pub fn evaluate(&self) -> Result<Literal<'a>, EvalError> {
            Ok(*self)
        }
    }
    // Explain yourself: build ast expr
    impl<'a> Display for Literal<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match *self {
                Literal::I32(value) => write!(f, " {}", value),
                Literal::F64(value) => write!(f, " {}", value),
                Literal::Str(string) => write!(f, " \"{}\"", string),
                Literal::Bool(bool_) => write!(f, " {}", bool_),
                Literal::Null => write!(f, " null"),
            }
        }
    }

    // Explain yourself: transfer base type
    impl<'a> Into<i32> for Literal<'a> {
        fn into(self) -> i32 {
            match self {
                Literal::I32(value) => return value,
                _ => panic!("[{:#?}] Literal is not Number!", self),
            }
        }
    }
    impl<'a> Into<f64> for Literal<'a> {
        fn into(self) -> f64 {
            match self {
                Literal::F64(value) => return value,
                _ => panic!("[{:#?}] Literal is not Number!", self),
            }
        }
    }
    impl<'a> Into<&'a str> for Literal<'a> {
        fn into(self) -> &'a str {
            match self {
                Literal::Str(value) => return value,
                _ => panic!("[{:#?}] Literal is not String!", self),
            }
        }
    }

    // Unary Operator
    //  Minus,
    //  Bang,
    #[derive(Debug)]
    pub struct Unary<'a> {
        pub l_opera: &'a Token<'a>,
        pub r_expr: Box<Expr<'a>>,
    }

    impl<'a> Unary<'a> {
        pub fn new(l_opera: &'a Token<'a>, r_expr: Box<Expr<'a>>) -> Unary<'a> {
            Unary { l_opera, r_expr }
        }
        pub fn evaluate(&self) -> Result<Literal<'a>, EvalError> {
            let value = self.r_expr.evaluate()?;
            match self.l_opera.token_type {
                TokenType::Bang => match value {
                    Literal::Bool(bool_) => Ok(Literal::Bool(!bool_)),
                    _ => Err(EvalError::new(
                        self.l_opera,
                        String::from(format!("This value '{}' cannot be !{}", value, value)),
                    )),
                },
                TokenType::Minus => match value {
                    Literal::I32(i32_) => Ok(Literal::I32(-i32_)),
                    Literal::F64(f64_) => Ok(Literal::F64(-f64_)),
                    _ => Err(EvalError::new(
                        self.l_opera,
                        String::from(format!("This value '{}' cannot be -{}", value, value)),
                    )),
                },
                _ => Err(EvalError::new(
                    self.l_opera,
                    String::from(format!(
                        "This operator '{}' cannot operate value {}",
                        self.l_opera, value
                    )),
                )),
            }
        }
    }
    // Explain yourself: build ast expr
    impl<'a> Display for Unary<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let _ = write!(f, "({} ", self.l_opera);
            let _ = Display::fmt(&self.r_expr, f);
            write!(f, ")")
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
        // Explain yourself: get eval
        pub fn evaluate(&self) -> Result<Literal<'a>, EvalError> {
            let l_value: Literal = self.l_expr.evaluate()?;
            let r_value: Literal = self.r_expr.evaluate()?;
            match self.m_opera.token_type {
                TokenType::BangEqual => match (l_value, r_value) {
                    (Literal::I32(l_i32), Literal::I32(r_i32)) => Ok(Literal::Bool(l_i32 != r_i32)),
                    (Literal::F64(l_f64), Literal::F64(r_f64)) => Ok(Literal::Bool(l_f64 != r_f64)),
                    (Literal::Str(l_str), Literal::Str(r_str)) => Ok(Literal::Bool(l_str != r_str)),
                    (Literal::Null, Literal::Null) => Ok(Literal::Bool(false)),
                    (Literal::Null, _) => Ok(Literal::Bool(true)),
                    (_, Literal::Null) => Ok(Literal::Bool(true)),
                    _ => Err(EvalError::new(
                        self.m_opera,
                        String::from(format!(
                            "[TypeMismatch] This operator '{}' cannot operate: {} {} {}",
                            self.m_opera, l_value, self.m_opera, r_value
                        )),
                    )),
                },
                TokenType::EqualEqual => match (l_value, r_value) {
                    (Literal::I32(l_i32), Literal::I32(r_i32)) => Ok(Literal::Bool(l_i32 == r_i32)),
                    (Literal::F64(l_f64), Literal::F64(r_f64)) => Ok(Literal::Bool(l_f64 == r_f64)),
                    (Literal::Str(l_str), Literal::Str(r_str)) => Ok(Literal::Bool(l_str == r_str)),
                    (Literal::Null, Literal::Null) => Ok(Literal::Bool(true)),
                    (Literal::Null, _) => Ok(Literal::Bool(false)),
                    (_, Literal::Null) => Ok(Literal::Bool(false)),
                    _ => Err(EvalError::new(
                        self.m_opera,
                        String::from(format!(
                            "[TypeMismatch] This operator '{}' cannot operate: {} {} {}",
                            self.m_opera, l_value, self.m_opera, r_value
                        )),
                    )),
                },
                TokenType::Greater => match (l_value, r_value) {
                    (Literal::I32(l_i32), Literal::I32(r_i32)) => Ok(Literal::Bool(l_i32 > r_i32)),
                    (Literal::F64(l_f64), Literal::F64(r_f64)) => Ok(Literal::Bool(l_f64 > r_f64)),
                    (Literal::Str(l_str), Literal::Str(r_str)) => Ok(Literal::Bool(l_str > r_str)),
                    _ => Err(EvalError::new(
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
                    _ => Err(EvalError::new(
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
                    _ => Err(EvalError::new(
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
                    _ => Err(EvalError::new(
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
                    _ => Err(EvalError::new(
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
                    _ => Err(EvalError::new(
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
                            Err(EvalError::new(
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
                            Err(EvalError::new(
                                self.m_opera,
                                String::from(format!(
                                    "[SlashZeroError] This operator '{}' cannot operate: {} {} {}",
                                    self.m_opera, l_value, self.m_opera, r_value
                                )),
                            ))
                        }
                    }
                    _ => Err(EvalError::new(
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
                    _ => Err(EvalError::new(
                        self.m_opera,
                        String::from(format!(
                            "[TypeMismatch] This operator '{}' cannot operate: {} {} {}",
                            self.m_opera, l_value, self.m_opera, r_value
                        )),
                    )),
                },
                _ => Err(EvalError::new(
                    self.m_opera,
                    String::from(format!(
                        "[InvalidBinaryOperation] This operator '{}' cannot operate: {} {} {}",
                        self.m_opera, l_value, self.m_opera, r_value
                    )),
                )),
            }
        }
    }
    // Explain yourself: build ast expr
    impl<'a> Display for Binary<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let _ = write!(f, "(");
            let _ = Display::fmt(&self.m_opera, f);
            let _ = Display::fmt(&self.l_expr, f);
            let _ = Display::fmt(&self.r_expr, f);
            write!(f, ")")
        }
    }
    // Operator Operator
    // -  +  / * != == > >= < <=
}

pub mod aux_fun {
    use super::*;
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
    pub fn unary<'a>(l_opera: &'a Token<'a>, r_expr: Box<Expr<'a>>) -> Box<Expr<'a>> {
        Box::new(Expr::Unary(Unary::new(l_opera, r_expr)))
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
}

pub struct EvalError {
    line: usize,
    where_: String,
    msg: String,
}
impl EvalError {
    pub fn new(token: &Token, msg: String) -> EvalError {
        let line: usize = token.line;
        let where_: String = String::from(format!("at '{}'", token));
        EvalError { line, where_, msg }
    }
}
impl ReportError for EvalError {
    fn report(&self) {
        eprintln!("{self:#?}")
    }
}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "EvalError(\n\tline: {}, \n\twhere: {}, \n\tmsg: {}\n)",
            self.line, self.where_, self.msg
        )
    }
}
impl Debug for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[line {}] where: {}\n\tmsg: {}\n",
            self.line, self.where_, self.msg
        )
    }
}

// ast printer
// (* (- 123) (group 45.67))
// let minus = Token::new(TokenType::Minus, "", Literal::Null, 0);
// let star = Token::new(TokenType::Star, "", Literal::Null, 0);
// let bang = Token::new(TokenType::Bang, "", Literal::Null, 0);
//
// let expr = binary(
//     unary(&minus, literal_number(123.0)),
//     &star,
//     grouping(literal_number(45.67)),
// );
// println!("expr: {:#?}", expr);
// println!("expr: {}", expr);
// println!("unary: {:#?}", unary(&bang, literal_Bool(true)));
// println!("unary: {}", unary(&bang, literal_Bool(true)));

// expr: (*(*( group (* 365 24)) 60)( group (/ 60 3600)))
// expr: (*(* 8670 60)( group (/ 60 3600)))

//
//
// Next Syntax RuleSet(使用优先级与结合性):
//     expression     → equality ;
//     equality       → comparison ( ( "!=" | "==" ) comparison )* ;
//     comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
//     term           → factor ( ( "-" | "+" ) factor )* ;
//     factor         → unary ( ( "/" | "*" ) unary )* ;
//     unary          → ( "!" | "-" ) unary
//                      | grouping ;
//     grouping      -> "(" expression ")"
//                      | primary ;
//     primary        → i32 | f64 | Str | "true" | "false" | "null"
//

// wait handle: idea used enum impl parser rule
// #[allow(dead_code)]
// mod syntax_ruler {
//     use super::TokenType;
//     enum ExprRuler<'a> {
//         EqualRuler(EqualRuler<'a>),
//     }
//     // ( "!=" | "==" )
//     enum EqualRuler<'a> {
//         Opera(TokenType),
//         CompareRuler(CompareRuler<'a>),
//     }
//     // ( ">" | ">=" | "<" | "<=" )
//     enum CompareRuler<'a> {
//         Opera(TokenType),
//         TermRuler(TermRuler<'a>),
//     }
//     //  ( "-" | "+" )
//     enum TermRuler<'a> {
//         Opera(TokenType),
//         FactorRuler(FactorRuler<'a>),
//     }
//     // ( "/" | "*" )
//     enum FactorRuler<'a> {
//         Opera(TokenType),
//         UnaryRuler(UnaryRuler<'a>),
//     }
//     // ( "!" | "-" )
//     enum UnaryRuler<'a> {
//         Opera(TokenType),
//         UnaryRuler(Box<UnaryRuler<'a>>),
//         GroupingRuler(GroupingRuler<'a>),
//     }
//     // "(" expression ")"  | primary
//     enum GroupingRuler<'a> {
//         Grouping(Box<ExprRuler<'a>>),
//         PrimaryRuler(PrimaryRuler<'a>),
//     }
//     enum PrimaryRuler<'a> {
//         Number(f64),
//         String(&'a str),
//         Bool(bool),
//         Null,
//     }
// }

#[cfg(test)]
mod test {}
