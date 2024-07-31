//! This file is joker token.rs.
use std::collections::HashMap;
use std::fmt::{Debug, Display};

use super::super::r#type::Object;
use lazy_static::lazy_static;


#[derive(Debug, PartialEq, Eq)]
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
            // id Str i32 f64
            TokenType::Identifier => write!(f, "ident"),
            TokenType::Str => write!(f, "str"),
            TokenType::I32 => write!(f, "i32"),
            TokenType::F64 => write!(f, "f64"),
            // Keyword
            TokenType::And => write!(f, "and"),
            TokenType::Class => write!(f, "class"),
            TokenType::Else => write!(f, "else"),
            TokenType::False => write!(f, "false"),
            TokenType::Fun => write!(f, "fun"),
            TokenType::For => write!(f, "for"),
            TokenType::If => write!(f, "if"),
            TokenType::Null => write!(f, "null"),
            TokenType::Or => write!(f, "or"),
            TokenType::Print => write!(f, "print"),
            TokenType::Return => write!(f, "return"),
            TokenType::Super => write!(f, "super"),
            TokenType::This => write!(f, "this"),
            TokenType::True => write!(f, "true"),
            TokenType::Var => write!(f, "var"),
            TokenType::While => write!(f, "while"),
            TokenType::Break => write!(f, "break"),
            TokenType::Match => write!(f, "match"),
            TokenType::Struct => write!(f, "struct"),

            TokenType::Eof => write!(f, "eof"),
        }
    }
}


// lazy_static keyword
lazy_static! {
    pub(crate) static ref KEYWORD: HashMap<&'static str, TokenType> = {
        let mut keyword: HashMap<&str, TokenType> = HashMap::new();
        keyword.insert("and", TokenType::And);
        keyword.insert("class", TokenType::Class);
        keyword.insert("else", TokenType::Else);
        keyword.insert("false", TokenType::False);
        keyword.insert("for", TokenType::For);
        keyword.insert("fun", TokenType::Fun);
        keyword.insert("if", TokenType::If);
        keyword.insert("null", TokenType::Null);
        keyword.insert("or", TokenType::Or);
        keyword.insert("print", TokenType::Print);
        keyword.insert("return", TokenType::Return);
        keyword.insert("super", TokenType::Super);
        keyword.insert("this", TokenType::This);
        keyword.insert("true", TokenType::True);
        keyword.insert("var", TokenType::Var);
        keyword.insert("while", TokenType::While);
        keyword.insert("break", TokenType::Break);
        keyword.insert("match", TokenType::Match);
        keyword.insert("struct", TokenType::Struct);
        keyword
    };
}


// 词素和标记（词法单元）
pub struct Token {
    pub ttype: TokenType,
    pub lexeme: String,
    pub literal: Option<Object>,
    pub line: usize,
}

impl Token {
    pub fn new(
        ttype: TokenType,
        lexeme: String,
        literal: Option<Object>,
        line: usize,
    ) -> Token {
        Token {
            ttype,
            lexeme,
            literal,
            line,
        }
    }

    pub fn eof(line: usize) -> Token {
        Token::new(TokenType::Eof, String::new(), None, line)
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ttype {
            TokenType::LeftParen => write!(f, "("),
            TokenType::RightParen => write!(f, ")"),
            TokenType::LeftBracket => write!(f, "["),
            TokenType::RightBracket => write!(f, "]"),
            TokenType::LeftBrace => write!(f, "{{"), // 转义
            TokenType::RightBrace => write!(f, "}}"),
            TokenType::Comma => write!(f, ","),
            TokenType::Dot => write!(f, "."),
            // -  +   ;  / *
            TokenType::Minus => write!(f, "-"),
            TokenType::Plus => write!(f, "+"),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::Slash => write!(f, "/"),
            TokenType::Star => write!(f, "*"),

            // One or two character tokens
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

            // Literals
            // id string number
            TokenType::Identifier => write!(f, "{}", self.lexeme),
            TokenType::Str => write!(f, "{}", self.lexeme),
            TokenType::I32 => write!(f, "{}", self.lexeme),
            TokenType::F64 => write!(f, "{}", self.lexeme),

            // Keyword Nil -> Null
            TokenType::And => write!(f, "and"),
            TokenType::Class => write!(f, "class"),
            TokenType::Else => write!(f, "else"),
            TokenType::False => write!(f, "false"),
            TokenType::Fun => write!(f, "fun"),
            TokenType::For => write!(f, "for"),
            TokenType::If => write!(f, "if"),
            TokenType::Null => write!(f, "null"),
            TokenType::Or => write!(f, "or"),
            TokenType::Print => write!(f, "print"),
            TokenType::Return => write!(f, "return"),
            TokenType::Super => write!(f, "super"),
            TokenType::This => write!(f, "this"),
            TokenType::True => write!(f, "true"),
            TokenType::Var => write!(f, "var"),
            TokenType::While => write!(f, "while"),
            TokenType::Break => write!(f, "break"),
            TokenType::Match => write!(f, "match"),
            TokenType::Struct => write!(f, "struct"),

            TokenType::Eof => write!(f, "eof"),
        }
    }
}


impl Debug for Token {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let literal: String = match &self.literal {
            Some(literal) => format!("Some({})",literal.to_string()),
            None => String::from("None"),
        };
        write!(f , "Token(ttype: {}, lexeme: {}, literal: {}, line: {})", 
                    self.ttype, self.lexeme, literal, self.line,)
   } 
}


#[cfg(test)]
mod test {}
