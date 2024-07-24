//! this file is parse Token.
//! Next Syntax RuleSet(使用优先级与结合性):
//!     expression     → equality ;
//!     equality       → comparison ( ( "!=" | "==" ) comparison )* ;
//!     comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
//!     term           → factor ( ( "-" | "+" ) factor )* ;
//!     factor         → unary ( ( "/" | "*" ) unary )* ;
//!     unary          → ( "!" | "-" ) unary
//!                     | paren ;
//!     paren         -> "(" expression ")"
//!                     | primary ;
//!     primary        → I32| F64 | Str | "true" | "false" | "nil"
//!                    
//!
//! 递归下降分析
//!     现在有一大堆解析技术，它们的名字大多是 “L” 和 “R” 的组合
//!         - LL(k), LR(1), LALR—还有更多的异类，
//!         - 比如解析器组合子、Earley parsers、分流码算法和packrat解析
//!     对于我们的第一个解释器来说，一种技术已经足够了：递归下降。
//!
//!
//! 每个语法规则都成为新结构体中的一个方法。

use std::fmt::{Debug, Display};

use super::{
    ast::{
        binary, grouping, literal_bool, literal_f64, literal_i32, literal_null, literal_str, unary,
        Expr,
    },
    error::{JokerError, ReportError},
    token::{Token, TokenType},
};

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: &'a [Token<'a>],
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> Self {
        Parser { tokens, current: 0 }
    }

    // 解析入口
    pub fn parse(&mut self) -> Result<Box<Expr<'a>>, ParserError> {
        self.parse_expression()
    }

    // 辅助函数，用来获取并检查下一个token
    fn peek(&self) -> Option<&'a Token<'a>> {
        self.tokens.get(self.current)
    }
    fn advance(&mut self) -> Option<&'a Token<'a>> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }
    fn previous(&self) -> Option<&'a Token<'a>> {
        self.tokens.get(self.current - 1)
    }
    fn is_at_end(&self) -> bool {
        self.peek().unwrap().token_type == TokenType::Eof
    }
    fn matcher(&self, token_type: &'a TokenType) -> bool {
        &self.peek().unwrap().token_type == token_type
    }
    fn matchers(&self, token_types: &'a [TokenType]) -> bool {
        for token_type in token_types {
            if self.matcher(token_type) {
                return true;
            }
        }
        return false;
    }
    fn consume(&mut self, expected: TokenType, msg: String) -> Result<&'a Token<'a>, ParserError> {
        if !self.is_at_end() && self.matcher(&expected) {
            return Ok(self.advance().unwrap());
        };

        panic!("{}", ParserError::new(self.peek().unwrap(), msg));
    }
    // 同步递归下降解析器: next handle
    fn _synchronize(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if self.previous().unwrap().token_type == TokenType::Semicolon {
                return;
            }
            match self.peek().unwrap().token_type {
                TokenType::Class
                | TokenType::Struct
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => {}
            }
            self.advance();
        }
    }

    // 解析：语法规则
    fn parse_expression(&mut self) -> Result<Box<Expr<'a>>, ParserError> {
        self.parse_equality()
    }

    // equality    -→ comparison ( ( "!=" | "==" ) comparison )* ;
    fn parse_equality(&mut self) -> Result<Box<Expr<'a>>, ParserError> {
        let mut expr: Box<Expr<'a>> = self.parse_comparison()?;
        while !self.is_at_end() && self.matchers(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let opera: &'a Token<'a> = self.advance().unwrap();
            let r_expr: Box<Expr<'a>> = self.parse_comparison()?;
            expr = binary(expr, opera, r_expr);
        }
        Ok(expr)
    }

    // comparison  -→ term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn parse_comparison(&mut self) -> Result<Box<Expr<'a>>, ParserError> {
        let mut expr: Box<Expr<'a>> = self.parse_term()?;

        while !self.is_at_end()
            && self.matchers(&[
                TokenType::Greater,
                TokenType::GreaterEqual,
                TokenType::Less,
                TokenType::LessEqual,
            ])
        {
            let opera: &'a Token<'a> = self.advance().unwrap();
            let r_expr: Box<Expr<'a>> = self.parse_term()?;
            expr = binary(expr, opera, r_expr);
        }

        Ok(expr)
    }

    // term    -→ factor ( ( "-" | "+" ) factor )* ;
    fn parse_term(&mut self) -> Result<Box<Expr<'a>>, ParserError> {
        let mut expr: Box<Expr<'a>> = self.parse_factor()?;

        while !self.is_at_end() && self.matchers(&[TokenType::Minus, TokenType::Plus]) {
            let opera: &'a Token<'a> = self.advance().unwrap();
            let r_expr: Box<Expr<'a>> = self.parse_factor()?;
            expr = binary(expr, opera, r_expr);
        }

        Ok(expr)
    }

    // factor   -→ unary ( ( "/" | "*" ) unary )* ;
    fn parse_factor(&mut self) -> Result<Box<Expr<'a>>, ParserError> {
        let mut expr: Box<Expr<'a>> = self.parse_unary()?;

        while !self.is_at_end() && self.matchers(&[TokenType::Slash, TokenType::Star]) {
            let opera: &'a Token<'a> = self.advance().unwrap();
            let r_expr: Box<Expr<'a>> = self.parse_unary()?;
            expr = binary(expr, opera, r_expr);
        }

        Ok(expr)
    }

    //unary          → ( "!" | "-" ) unary
    //                | paren ;
    fn parse_unary(&mut self) -> Result<Box<Expr<'a>>, ParserError> {
        if !self.is_at_end() && self.matchers(&[TokenType::Bang, TokenType::Minus]) {
            let l_opera: &'a Token<'a> = self.advance().unwrap();
            let r_expr: Box<Expr<'a>> = self.parse_unary()?;
            return Ok(unary(l_opera, r_expr));
        }

        self.parse_grouping()
    }

    // grouping       -> "(" expression ")"
    //                  | primary
    fn parse_grouping(&mut self) -> Result<Box<Expr<'a>>, ParserError> {
        if !self.is_at_end() && self.matcher(&TokenType::LeftParen) {
            self.advance();
            let m_expr: Box<Expr<'a>> = self.parse_expression()?;
            self.consume(
                TokenType::RightParen,
                format!(
                    "Index: {}, Msg: {}",
                    self.current, "Expect ')' after expression."
                ),
            )?;
            return Ok(grouping(m_expr));
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Box<Expr<'a>>, ParserError> {
        if self.is_at_end() {
            return Err(ParserError::new(
                self.peek().unwrap(),
                String::from("[Null Token] Parsing reaches end!"),
            ));
        };

        // get current value and
        // move to next token
        match self.peek().unwrap().token_type {
            TokenType::False => {
                self.advance();
                return Ok(literal_bool(false));
            }
            TokenType::True => {
                self.advance();
                return Ok(literal_bool(true));
            }
            TokenType::Null => {
                self.advance();
                return Ok(literal_null());
            }
            TokenType::I32 => {
                let current: i32 = self.advance().unwrap().literal.into();
                return Ok(literal_i32(current));
            }
            TokenType::F64 => {
                let current: f64 = self.advance().unwrap().literal.into();
                return Ok(literal_f64(current));
            }
            TokenType::Str => {
                let current: &str = self.advance().unwrap().literal.into();
                return Ok(literal_str(current));
            }
            _ => {
                return Err(ParserError::new(
                    self.peek().unwrap(),
                    String::from("Match arm is Not Impl!"),
                ))
            }
        };
    }
}

pub struct ParserError {
    line: usize,
    where_: String,
    msg: String,
}

impl ParserError {
    pub fn new<'a>(token: &'a Token<'a>, msg: String) -> ParserError {
        let where_: String = if token.token_type == TokenType::Eof {
            String::from(" at end")
        } else {
            String::from(format!(" at '{}'", token.lexeme))
        };
        ParserError {
            line: token.line,
            where_,
            msg,
        }
    }
}
impl ReportError for ParserError {
    fn report(&self) {
        eprintln!("{self:#?}")
    }
}
impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ParserError(\n\tline: {}, \n\twhere: {}, \n\tmsg: {}\n)",
            self.line, self.where_, self.msg
        )
    }
}
impl Debug for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[line {}] where: {}\n\tmsg: {}\n",
            self.line, self.where_, self.msg
        )
    }
}

impl JokerError for ParserError {}

#[cfg(test)]
mod test {}
