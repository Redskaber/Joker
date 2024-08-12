//! This file is joker scanner.
//!
//!
use super::{
    error::{JokerError, ReportError},
    object::{literal_bool, literal_f64, literal_i32, literal_null, literal_str, Object},
    token::{Token, TokenType},
};

#[derive(Debug)]
pub struct Scanner {
    source: Vec<char>,
    tokens: Option<Vec<Token>>,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: String) -> Scanner {
        Scanner {
            source: source.chars().collect::<Vec<char>>(),
            tokens: Some(Vec::new()),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn is_at_end(&self) -> bool {
        self.current == self.source.len()
    }
    pub fn advance(&mut self) -> char {
        let current_char: char = *self.source.get(self.current).unwrap();
        self.current += 1;
        current_char
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, JokerError> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token()?;
        }

        match self.tokens {
            Some(ref mut tokens) => tokens.push(Token::eof(self.line)),
            None => {
                return Err(JokerError::Scanner(ScannerError::error(
                    self.line,
                    String::from("Scanner tokens is None"),
                )))
            }
        };
        Ok(self.tokens.take().unwrap())
    }

    fn scan_token(&mut self) -> Result<(), JokerError> {
        let char_: char = self.advance();

        match char_ {
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
            '?' => self.add_token(TokenType::Question),
            ':' => self.add_token(TokenType::Colon),
            '|' => self.add_token(TokenType::Pipeline),
            '!' => {
                let t_type = if self.is_match('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                self.add_token(t_type);
            }
            '=' => {
                let t_type = if self.is_match('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                self.add_token(t_type);
            }
            '<' => {
                let t_type = if self.is_match('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.add_token(t_type);
            }
            '>' => {
                let t_type = if self.is_match('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.add_token(t_type);
            }
            '/' => {
                if self.is_match('/') {
                    while let Some(ch) = self.peek() {
                        if *ch == '\n' {
                            break;
                        }
                        self.advance();
                    }
                } else if self.is_match('*') {
                    self.scan_comment()?;
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => self.line += 1,
            '\\' => self.scan_escape()?,
            '"' => self.scan_string()?,
            '0'..='9' => self.scan_number()?,
            'a'..='z' | 'A'..='Z' | '_' => self.scan_identifier()?,
            _ => {
                return Err(JokerError::Scanner(ScannerError::error(
                    self.line,
                    String::from("Unexpected character"),
                )))
            }
        }
        Ok(())
    }

    fn add_token(&mut self, ttype: TokenType) {
        self.add_token_object(ttype, literal_null())
    }

    fn add_token_object(&mut self, ttype: TokenType, literal: Object) {
        let lexeme: String = self.source[self.start..self.current].iter().collect();
        match self.tokens {
            Some(ref mut tokens) => tokens.push(Token::new(ttype, lexeme, literal, self.line)),
            None => self.tokens = Some(vec![Token::new(ttype, lexeme, literal, self.line)]),
        }
    }

    // TODO: scanner escape
    fn scan_escape(&mut self) -> Result<(), JokerError> {
        Ok(())
    }

    fn scan_comment(&mut self) -> Result<(), JokerError> {
        while let Some(ch) = self.peek() {
            match ch {
                '\n' => self.line += 1,
                '*' => match self.next_peek() {
                    Some(next_ch) => {
                        if *next_ch == '/' {
                            self.advance();
                            self.advance();
                            break;
                        }
                    }
                    None => {
                        return Err(JokerError::Scanner(ScannerError::error(
                            self.line,
                            String::from("Unterminated comment."),
                        )))
                    }
                },
                _ => {}
            }
            self.advance();
        }
        Ok(())
    }

    fn scan_string(&mut self) -> Result<(), JokerError> {
        while let Some(ch) = self.peek() {
            match ch {
                '"' => break,
                '\\' if self.next_peek() == Some(&'"') => {
                    self.advance();
                }
                '\n' => self.line += 1,
                _ => {}
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(JokerError::Scanner(ScannerError::error(
                self.line,
                String::from("Unterminated string."),
            )));
        }
        self.advance();

        // TODO: handle escape sequence
        let text: String = self.source[self.start + 1..self.current - 1]
            .iter()
            .collect();
        self.add_token_object(TokenType::Str, literal_str(text));
        Ok(())
    }

    fn is_digit(op_ch: Option<&char>) -> bool {
        match op_ch {
            Some(ch) => ch.is_ascii_digit(),
            None => false,
        }
    }

    fn scan_number(&mut self) -> Result<(), JokerError> {
        while Scanner::is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == Some(&'.') {
            if Scanner::is_digit(self.next_peek()) {
                self.advance();
                while Scanner::is_digit(self.peek()) {
                    self.advance();
                }
                let f64_text: String = self.source[self.start..self.current].iter().collect();
                let f64_: f64 = f64_text.parse::<f64>().unwrap();
                self.add_token_object(TokenType::F64, literal_f64(f64_));
                return Ok(());
            } else {
                return Err(JokerError::Scanner(ScannerError::error(
                    self.line,
                    String::from("floating-point numbers require fractional parts."),
                )));
            }
        }

        let i32_text: String = self.source[self.start..self.current].iter().collect();
        let i32_: i32 = i32_text.parse::<i32>().unwrap();
        self.add_token_object(TokenType::I32, literal_i32(i32_));
        Ok(())
    }

    fn is_alpha_numeric(op_ch: Option<&char>) -> bool {
        match op_ch {
            Some(ch) => ch.is_ascii_alphanumeric() || *ch == '_',
            None => false,
        }
    }

    fn scan_identifier(&mut self) -> Result<(), JokerError> {
        while Scanner::is_alpha_numeric(self.peek()) {
            self.advance();
        }

        let text: String = self.source[self.start..self.current].iter().collect();
        match Scanner::keywords(&text) {
            Some(keyword) => match keyword {
                TokenType::True => self.add_token_object(keyword, literal_bool(true)),
                TokenType::False => self.add_token_object(keyword, literal_bool(false)),
                _ => self.add_token(keyword),
            },
            None => self.add_token(TokenType::Identifier),
        }
        Ok(())
    }

    fn keywords(check: &str) -> Option<TokenType> {
        match check {
            "and" => Some(TokenType::And),
            "class" => Some(TokenType::Class),
            "else" => Some(TokenType::Else),
            "false" => Some(TokenType::False),
            "for" => Some(TokenType::For),
            "fun" => Some(TokenType::Fun),
            "if" => Some(TokenType::If),
            "null" => Some(TokenType::Null),
            "or" => Some(TokenType::Or),
            "print" => Some(TokenType::Print),
            "return" => Some(TokenType::Return),
            "super" => Some(TokenType::Super),
            "this" => Some(TokenType::This),
            "true" => Some(TokenType::True),
            "var" => Some(TokenType::Var),
            "while" => Some(TokenType::While),
            "break" => Some(TokenType::Break),
            "continue" => Some(TokenType::Continue),
            "match" => Some(TokenType::Match),
            "struct" => Some(TokenType::Struct),
            _ => None,
        }
    }

    fn is_match(&mut self, expected: char) -> bool {
        match self.source.get(self.current) {
            Some(ch) if *ch == expected => {
                self.current += 1;
                true
            }
            _ => false,
        }
    }

    fn peek(&self) -> Option<&char> {
        self.source.get(self.current)
    }
    fn next_peek(&self) -> Option<&char> {
        self.source.get(self.current + 1)
    }
}

#[derive(Debug)]
pub struct ScannerError {
    line: usize,
    where_: String,
    msg: String,
}
impl ScannerError {
    pub fn new(token: &Token, msg: String) -> ScannerError {
        let where_: String = if token.ttype == TokenType::Eof {
            String::from(" at end")
        } else {
            format!(" at '{}'", token.lexeme)
        };
        ScannerError {
            line: token.line,
            where_,
            msg,
        }
    }
    pub fn error(line: usize, msg: String) -> ScannerError {
        ScannerError {
            line,
            where_: String::from(""),
            msg,
        }
    }
}
impl ReportError for ScannerError {
    fn report(&self) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, self.where_, self.msg
        );
    }
}
