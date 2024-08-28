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
        assign, binary, expr_stmt, grouping, literal_bool, literal_f64, literal_i32, literal_null, literal_str, literal_variable, print_stmt, unary, var_stmt, Expr, Literal, Statement 
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
    pub fn parse(&mut self) -> Result<Vec<Statement<'a>>, ParserError> {
        let mut statements: Vec<Statement> = Vec::new();
        while !self.is_at_end() {
            statements.push(self.parse_declaration()?);
        }
        return Ok(statements);
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
    fn synchronize(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if self.previous().unwrap().token_type == TokenType::Semicolon {
                return;
            }
            match self.peek().unwrap().token_type {
                TokenType::Class
                | TokenType::Struct
                | TokenType::Fn
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
    // program        → declaration* EOF ;
    // 
    // declaration    → varDecl
    //                | statement ;
    // 
    // statement      → exprStmt
    //                | printStmt ;
    // 
    // exprStmt       → expression ";" ;
    // printStmt      → "print" expression ";" ; 
    fn parse_declaration(&mut self) -> Result<Statement<'a>, ParserError> {
        if self.matcher(&TokenType::Var) {
            self.advance();
            match self.parse_var_declaration() {
                Ok(stmt) => return Ok(stmt),
                Err(err) => {
                    self.synchronize();
                    return Err(err);
                },
            }
        }
        match self.parse_statement() {
            Ok(stmt) => Ok(stmt),
            Err(err) => {
                self.synchronize();
                Err(err)
            }
        }
    }
    fn parse_var_declaration(&mut self) -> Result<Statement<'a>, ParserError> {
        let var_name: &Token<'a> = self.consume(TokenType::Identifier, String::from("Expect variable name."))?;
        let mut var_value: Box<Expr<'a>> = literal_null();
        if self.matcher(&TokenType::Equal) {
            self.advance();
            var_value = self.parse_expression()?;
        }

        self.consume(TokenType::Semicolon, String::from("Expect ';' after variable declaration."))?;
        Ok(var_stmt(var_name, var_value))

    } 
    fn parse_statement(&mut self) -> Result<Statement<'a>, ParserError> {
        if self.matcher(&TokenType::Print) {
            self.advance();
            return self.parse_print_statement();
        }
        self.parse_expression_statement()
    }
    fn parse_print_statement(&mut self) -> Result<Statement<'a>, ParserError> {
        let expr: Box<Expr<'a>> = self.parse_expression()?;
        self.consume(TokenType::Semicolon, String::from("Expect ';' after value."))?;
        Ok(print_stmt(expr))
    }
    fn parse_expression_statement(&mut self) -> Result<Statement<'a>, ParserError> {
        let expr: Box<Expr<'a>> = self.parse_expression()?;
        self.consume(TokenType::Semicolon, String::from("Expect ';' after expression."))?;
        Ok(expr_stmt(expr))
    }

    // expression     → assignment ;
    // assignment     → IDENTIFIER "=" assignment
    //                | equality ;
    fn parse_expression(&mut self) -> Result<Box<Expr<'a>>, ParserError> {
        self.parse_assignment()
    }
    

    fn parse_assignment(&mut self) -> Result<Box<Expr<'a>>, ParserError> {
        let expr = self.parse_equality()?;

        if self.matcher(&TokenType::Equal) {
            let equal = self.advance().unwrap();
            let value = self.parse_assignment()?;

            match *expr {
                Expr::Variable(variable) => {
                    let name = variable.name;
                    return Ok(assign(name, value))
                },
                _ => return Err(ParserError::new(
                    equal, 
                    String::from("Invalid assignment target.")
                ))
            }
        }
        Ok(expr)
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
                match self.advance().unwrap().literal.try_into() {
                    Ok(current) => return Ok(literal_i32(current)),
                    Err(err) => return Err(ParserError::new(self.peek().unwrap(), String::from(err))),
                };
            }
            TokenType::F64 => {
                match self.advance().unwrap().literal.try_into() {
                    Ok(current) => return Ok(literal_f64(current)),
                    Err(err) => return Err(ParserError::new(self.peek().unwrap(), String::from(err))),
                };
            }
            TokenType::Str => {
                match self.advance().unwrap().literal.try_into() {
                    Ok(current) => return Ok(literal_str(current)),
                    Err(err) => return Err(ParserError::new(self.peek().unwrap(), String::from(err))),
                };
            }
            TokenType::Identifier => {
                match self.advance() {
                    Some(current) => return Ok(literal_variable(current)),
                    None => return Err(ParserError::new(
                        &Token::new(TokenType::Eof, "", Literal::Null, 0), 
                        String::from("The variable assignment statement has an left value, but there is no right value error!")
                    )),
                };
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
