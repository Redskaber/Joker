//! This file is joker token.rs.
//!
//!
use std::fmt::{Debug, Display};

use super::object::{literal_null, Object};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    // ()
    LeftParen,
    RightParen,
    // []
    LeftBracket,
    RightBracket,
    // {}
    LeftBrace,
    RightBrace,
    // , .
    Comma,
    Dot,
    // -  +   ;  / *
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // = ==  ! !=
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    // > >=
    Greater,
    GreaterEqual,
    // < <=
    Less,
    LessEqual,
    // :  ? |
    Colon,
    Question,
    Pipeline,
    // id string I32 F64
    Identifier,
    Str,
    I32,
    F64,
    // Keyword
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Null,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Break,
    Continue,
    Match,
    Struct,
    Eof,
}
impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            TokenType::LeftParen => write!(f, "("),
            TokenType::RightParen => write!(f, ")"),
            TokenType::LeftBracket => write!(f, "["),
            TokenType::RightBracket => write!(f, "]"),
            TokenType::LeftBrace => write!(f, "{{"),
            TokenType::RightBrace => write!(f, "}}"),
            TokenType::Comma => write!(f, ","),
            TokenType::Dot => write!(f, "."),
            // -  +   ;  / *
            TokenType::Minus => write!(f, "-"),
            TokenType::Plus => write!(f, "+"),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::Slash => write!(f, "/"),
            TokenType::Star => write!(f, "*"),
            // ! != = ==
            TokenType::Bang => write!(f, "!"),
            TokenType::BangEqual => write!(f, "!="),
            TokenType::Equal => write!(f, "="),
            TokenType::EqualEqual => write!(f, "=="),
            // > >=
            TokenType::Greater => write!(f, ">"),
            TokenType::GreaterEqual => write!(f, ">="),
            // < <=
            TokenType::Less => write!(f, "<"),
            TokenType::LessEqual => write!(f, "<="),
            // : ?
            TokenType::Colon => write!(f, ":"),
            TokenType::Question => write!(f, "?"),
            TokenType::Pipeline => write!(f, "|"),
            // id Str i32 f64
            TokenType::Identifier => write!(f, "Ident"),
            TokenType::Str => write!(f, "Str"),
            TokenType::I32 => write!(f, "I32"),
            TokenType::F64 => write!(f, "F64"),
            // Keyword
            TokenType::And => write!(f, "And"),
            TokenType::Class => write!(f, "Class"),
            TokenType::Else => write!(f, "Else"),
            TokenType::False => write!(f, "False"),
            TokenType::Fun => write!(f, "Fun"),
            TokenType::For => write!(f, "For"),
            TokenType::If => write!(f, "If"),
            TokenType::Null => write!(f, "Null"),
            TokenType::Or => write!(f, "Or"),
            TokenType::Print => write!(f, "Print"),
            TokenType::Return => write!(f, "Return"),
            TokenType::Super => write!(f, "Super"),
            TokenType::This => write!(f, "This"),
            TokenType::True => write!(f, "True"),
            TokenType::Var => write!(f, "Var"),
            TokenType::While => write!(f, "While"),
            TokenType::Break => write!(f, "Break"),
            TokenType::Continue => write!(f, "Continue"),
            TokenType::Match => write!(f, "Match"),
            TokenType::Struct => write!(f, "Struct"),

            TokenType::Eof => write!(f, "Eof"),
        }
    }
}

// 词素和标记（词法单元）
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub ttype: TokenType,
    pub lexeme: String,
    pub literal: Object,
    pub line: usize,
}

impl Token {
    pub fn new(ttype: TokenType, lexeme: String, literal: Object, line: usize) -> Token {
        Token {
            ttype,
            lexeme,
            literal,
            line,
        }
    }
    pub fn eof(line: usize) -> Token {
        Token::new(TokenType::Eof, String::new(), literal_null(), line)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ttype {
            TokenType::Identifier => write!(f, "{}", self.lexeme),
            TokenType::Str => write!(f, "{}", self.lexeme),
            TokenType::I32 => write!(f, "{}", self.lexeme),
            TokenType::F64 => write!(f, "{}", self.lexeme),
            _ => Display::fmt(&self.ttype, f),
        }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Token(ttype: {}, lexeme: {}, literal: {}, line: {})",
            self.ttype, self.lexeme, self.literal, self.line,
        )
    }
}

#[cfg(test)]
mod test {}
