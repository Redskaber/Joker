//! This file is joker token.rs.
use super::ast::Literal;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    // single-character tokens
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

    // One or two character tokens
    // ! != = ==
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    // > >=
    Greater,
    GreaterEqual,
    // < <=
    Less,
    LessEqual,

    // Literals
    // id string number
    Identifier,
    Str,
    I32,
    F64,

    // Keyword Nil -> Null
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
            // id Str i32 f64
            TokenType::Identifier => write!(f, "ident"),
            TokenType::Str => write!(f, "str"),
            TokenType::I32 => write!(f, "i32"),
            TokenType::F64 => write!(f, "f64"),

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
#[derive(Debug, PartialEq)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub lexeme: &'a str,
    pub literal: Literal<'a>,
    pub line: usize,
}

impl<'a> Token<'a> {
    pub fn new(
        token_type: TokenType,
        lexeme: &'a str,
        literal: Literal<'a>,
        line: usize,
    ) -> Token<'a> {
        Token {
            token_type,
            lexeme,
            literal,
            line,
        }
    }
}
impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.token_type {
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

#[cfg(test)]
mod test {}
