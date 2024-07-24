//! This file is joker scanner!
//! Token:
//!     - TokenType
//!     - Token
//!   词素
//!   标记
//!     词法单元
//!
//!
//!Scanner:
//! 正则语言与表达式
//!     扫描核心： loop {字符 属于 ？词素.. -> Token}
//!
//! 决定一门语言如何将字符分组为词素的规则 => 词法语法
//!     joker => 正则语言
//!     > Lex , Flex => In(regular) => Out(Scanner)

use super::{
    ast::Literal,
    error::{JokerError, ReportError},
    token::{Token, TokenType, KEYWORD},
};
use std::fmt::{Debug, Display};
use std::result;

#[derive(Debug)]
struct SourceHandler<'a> {
    source: &'a str,
    position: usize,
}

impl<'a> SourceHandler<'a> {
    pub fn new(src: &'a str) -> SourceHandler {
        SourceHandler {
            source: src,
            position: 0,
        }
    }
    /// get source substring &str
    pub fn sub_str(&self, begin: usize, end: usize) -> Option<&'a str> {
        if end > self.source.len() {
            return None;
        }
        Some(&self.source[begin..end])
    }
    // show current position char, position not change.
    pub fn peek(&self) -> Option<char> {
        if self.is_at_end() {
            return Some('\0');
        }
        self.source[self.position..].chars().next()
    }
    // show next position char, position not change.
    pub fn peek_next(&self) -> Option<char> {
        if self.position + 1 >= self.source.len() {
            return Some('\0');
        }
        self.source[self.position + 1..].chars().next()
    }

    // return current char and move to next char position.
    pub fn advance(&mut self) -> Option<char> {
        if let Some(current_char) = self.peek() {
            self.position += 1;
            return Some(current_char);
        }
        None
    }

    // check is at end
    pub fn is_at_end(&self) -> bool {
        self.position >= self.source.len()
    }
}

#[derive(Debug)]
struct ScannerTracker {
    start: usize,
    current: usize,
    line: usize,
}

impl ScannerTracker {
    pub fn new() -> ScannerTracker {
        ScannerTracker {
            start: 0,
            current: 0,
            line: 1,
        }
    }
}

#[derive(Debug)]
pub struct Scanner<'a> {
    source: SourceHandler<'a>,
    tokens: Option<Vec<Token<'a>>>,
    tracker: ScannerTracker,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Scanner<'a> {
        Scanner {
            source: SourceHandler::new(source),
            tokens: Some(Vec::new()),
            tracker: ScannerTracker::new(),
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.source.position = self.tracker.current;
        let ch: Option<char> = self.source.advance();
        if ch.is_some() {
            self.tracker.current += 1;
        }
        ch
    }

    pub fn scan_tokens(&mut self) -> result::Result<Vec<Token<'a>>, ScannerError> {
        while !self.source.is_at_end() {
            self.tracker.start = self.tracker.current;
            if let Err(err) = self.scan_token() {
                return Err(err);
            }
        }

        let mut tokens: Vec<Token<'a>> = self.tokens.take().unwrap();
        tokens.push(Token::new(
            TokenType::Eof,
            "",
            Literal::Null,
            self.tracker.line,
        ));
        Ok(tokens)
    }

    fn scan_token(&mut self) -> result::Result<(), ScannerError> {
        let char_: char = self.advance().ok_or_else(|| ScannerError::new(
                self.tracker.line,
                String::from("The return value of the advance function is None, and the index of the current position overflows.")
            )
        )?;

        match char_ {
            // 单字符词素
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '[' => self.add_token(TokenType::LeftBracket),
            ']' => self.add_token(TokenType::RightBracket),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            // 操作符
            '!' => {
                let is_bang_equal: bool = self.next_matcher('=');
                let token_type: TokenType = if is_bang_equal {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                self.add_token(token_type)
            }
            '=' => {
                let is_equal_equal: bool = self.next_matcher('=');
                let token_type: TokenType = if is_equal_equal {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                self.add_token(token_type)
            }
            '<' => {
                let is_less_equal: bool = self.next_matcher('=');
                let token_type: TokenType = if is_less_equal {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.add_token(token_type)
            }
            '>' => {
                let is_greater_equal: bool = self.next_matcher('=');
                let token_type: TokenType = if is_greater_equal {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.add_token(token_type)
            }
            '/' => {
                let is_annotation: bool = self.next_matcher('/');
                if is_annotation {
                    while self.source.peek() != Some('\n') && !self.source.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash)
                }
            }
            /* Ignore whitespace */
            ' ' | '\r' | '\t' => {}
            '\n' => self.tracker.line += 1,
            // 字符串字面量
            '"' => self.scan_string()?,
            // 数字字面量
            '0'..='9' => self.scan_number()?,
            // 保留字和标识符
            'a'..='z' | 'A'..='Z' | '_' => self.scan_identifier()?,
            _ => {
                return Err(ScannerError::new(
                    self.tracker.line,
                    String::from("Unexpected character."),
                ))
            }
        }

        Ok(())
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.__add_token(token_type, Literal::Null);
    }

    fn __add_token(&mut self, token_type: TokenType, literal: Literal<'a>) {
        let begin: usize = self.tracker.start;
        let end: usize = self.tracker.current;
        let lexeme: &str = self.source.sub_str(begin, end).unwrap();
        let line: usize = self.tracker.line;

        // log
        println!(
            "index: {:<5} line: {:<5} begin: {:<5} end: {:<5} lexeme: {:<5} token_type: {}",
            if !self.tokens.as_ref().unwrap().is_empty() {
                self.tokens.as_ref().unwrap().len()
            } else {
                0
            },
            line,
            begin,
            end,
            lexeme,
            token_type
        );

        match self.tokens {
            Some(ref mut tokens) => {
                tokens.push(Token::new(token_type, lexeme, literal, line));
            }
            None => {
                self.tokens = Some(vec![Token::new(token_type, lexeme, literal, line)]);
            }
        }
    }

    fn next_matcher(&mut self, expected: char) -> bool {
        if self.source.is_at_end() {
            return false;
        }
        self.source.position = self.tracker.current;
        if self.source.advance() == Some(expected) {
            self.tracker.current += 1;
            true
        } else {
            false
        }
    }

    /// scanner string literal.
    fn scan_string(&mut self) -> result::Result<(), ScannerError> {
        while self.source.peek() != Some('"') && !self.source.is_at_end() {
            if self.source.peek() == Some('\n') {
                self.tracker.line += 1;
            }
            self.advance();
        }

        if self.source.is_at_end() {
            return Err(ScannerError::new(
                self.tracker.line,
                String::from("Unterminated string."),
            ));
        }

        self.advance(); // move " next

        // Trim the surrounding quotes.
        let begin: usize = self.tracker.start + 1;
        let end: usize = self.tracker.current - 1;
        let string: &str = self.source.sub_str(begin, end).unwrap();
        let literal: Literal<'a> = Literal::Str(string);
        self.__add_token(TokenType::Str, literal);

        Ok(())
    }

    /// scanner number literal.
    fn scan_number(&mut self) -> result::Result<(), ScannerError> {
        while is_digit(self.source.peek().unwrap()) {
            self.advance();
        }

        //  Look for a fractional part.
        if self.source.peek() == Some('.') && is_digit(self.source.peek_next().unwrap()) {
            // Consume the "."
            self.advance();
            while is_digit(self.source.peek().unwrap()) {
                self.advance();
            }
            let begin: usize = self.tracker.start;
            let end: usize = self.tracker.current;
            let num_str: &str = self.source.sub_str(begin, end).unwrap();
            match num_str.parse::<f64>() {
                Ok(number) => {
                    let literal: Literal<'a> = Literal::F64(number);
                    self.__add_token(TokenType::F64, literal);
                }
                Err(err) => panic!("ParseFloatScannerError: {}", err),
            };
        } else {
            let begin: usize = self.tracker.start;
            let end: usize = self.tracker.current;
            let num_str: &str = self.source.sub_str(begin, end).unwrap();
            match num_str.parse::<i32>() {
                Ok(number) => {
                    let literal: Literal<'a> = Literal::I32(number);
                    self.__add_token(TokenType::I32, literal);
                }
                Err(err) => panic!("ParseFloatScannerError: {}", err),
            };
        }

        Ok(())
    }
    /// scanner identifier
    fn scan_identifier(&mut self) -> result::Result<(), ScannerError> {
        while is_alpha_numeric(self.source.peek().unwrap()) {
            self.advance();
        }

        let begin: usize = self.tracker.start;
        let end: usize = self.tracker.current;
        let text: &str = self.source.sub_str(begin, end).unwrap();
        let token_type = match KEYWORD.get(text) {
            Some(token_type) => token_type.clone(),
            None => TokenType::Identifier,
        };

        self.add_token(token_type);
        Ok(())
    }
}

// tools
fn is_digit(ch: char) -> bool {
    ch >= '0' && ch <= '9'
}

fn is_alpha(ch: char) -> bool {
    ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z' || ch == '_'
}

fn is_alpha_numeric(ch: char) -> bool {
    is_alpha(ch) || is_digit(ch)
}

pub struct ScannerError {
    line: usize,
    where_: String,
    msg: String,
}

impl ScannerError {
    pub fn new<'a>(line: usize, msg: String) -> ScannerError {
        ScannerError {
            line,
            where_: String::from(""),
            msg,
        }
    }
}
impl ReportError for ScannerError {
    fn report<'a>(&self) {
        eprintln!("{self:#?}");
    }
}

impl Display for ScannerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ScannerError(\n\tline: {}, \n\twhere: {}, \n\tmsg: {}\n)",
            self.line, self.where_, self.msg
        )
    }
}
impl Debug for ScannerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[line {}] where: {}:\n\tmsg: {}\n",
            self.line, self.where_, self.msg
        )
    }
}
impl JokerError for ScannerError {} // need impl Display && Debug pretty print.

#[cfg(test)]
mod test {}
