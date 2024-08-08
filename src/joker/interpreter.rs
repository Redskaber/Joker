//! This file is interpreters.rs
//!
//!

use std::{cell::RefCell, rc::Rc};

use super::{
    ast::{
        Assign, Binary, BlockStmt, BreakStmt, ContinueStmt, Expr, ExprAcceptor, ExprStmt,
        ExprVisitor, ForStmt, Grouping, Literal, Logical, PrintStmt, Stmt, StmtAcceptor,
        StmtVisitor, Trinomial, Unary, VarStmt, Variable, WhileStmt,
    },
    // ast_print::AstPrinter,
    env::Env,
    error::{AbortError, ControlFlowAbort, JokerError, ReportError},
    object::{Literal as ObL, Object},
    token::{Token, TokenType},
};

pub struct Interpreter {
    env: RefCell<Rc<RefCell<Env>>>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            env: RefCell::new(Rc::new(RefCell::new(Env::new()))),
        }
    }
    fn is_true(&self, object: &Object) -> bool {
        matches!(object, Object::Literal(ObL::Bool(true)))
    }
    fn execute(&self, stmt: &Stmt) -> Result<(), JokerError> {
        stmt.accept(self)
    }
    fn execute_block(&self, stmts: &[Stmt], block_env: Env) -> Result<(), JokerError> {
        let previous = self.env.replace(Rc::new(RefCell::new(block_env)));
        let result = stmts.iter().try_for_each(|stmt| self.execute(stmt));
        self.env.replace(previous);
        result
    }
    fn evaluate(&self, expr: &Expr) -> Result<Object, JokerError> {
        expr.accept(self)
    }

    pub fn interpreter(&self, stmts: &[Stmt]) -> Result<(), JokerError> {
        let mut is_success: bool = true;
        // let printer: AstPrinter = AstPrinter::new();
        for stmt in stmts {
            // printer.println(stmt);
            if let Err(err) = self.execute(stmt) {
                is_success = false;
                err.report();
            }
        }
        if is_success {
            Ok(())
        } else {
            Err(JokerError::Interpreter(IntoIteratorError::error(
                0,
                String::from("Interpreter Error!"),
            )))
        }
    }
    pub fn println_env(&self) {
        println!("{:?}", self.env);
    }
}

impl StmtVisitor<()> for Interpreter {
    fn visit_expr(&self, stmt: &ExprStmt) -> Result<(), JokerError> {
        self.evaluate(&stmt.expr)?;
        Ok(())
    }
    fn visit_print(&self, stmt: &PrintStmt) -> Result<(), JokerError> {
        match self.evaluate(&stmt.expr) {
            Ok(value) => {
                println!("{value}");
                Ok(())
            }
            Err(err) => Err(err),
        }
    }
    fn visit_var(&self, stmt: &VarStmt) -> Result<(), JokerError> {
        let value = if stmt.value != Expr::Literal(Literal::new(Object::Literal(ObL::Null))) {
            self.evaluate(&stmt.value)?
        } else {
            Object::Literal(ObL::Null)
        };
        self.env
            .borrow()
            .borrow_mut()
            .define(&stmt.name.lexeme, value);
        Ok(())
    }
    fn visit_block(&self, stmt: &BlockStmt) -> Result<(), JokerError> {
        let block_env = Env::new_with_enclosing(Rc::clone(&self.env.borrow()));
        self.execute_block(&stmt.stmts, block_env)
    }
    fn visit_if(&self, stmt: &super::ast::IfStmt) -> Result<(), JokerError> {
        if self.is_true(&self.evaluate(&stmt.condition)?) {
            self.execute(&stmt.then_branch)?;
        } else if let Some(box_stmt) = &stmt.else_branch {
            self.execute(box_stmt)?;
        }
        Ok(())
    }
    fn visit_while(&self, stmt: &WhileStmt) -> Result<(), JokerError> {
        while self.is_true(&self.evaluate(&stmt.condition)?) {
            if let Err(JokerError::Abort(AbortError::ControlFlow(control_flow))) =
                self.execute(&stmt.body)
            {
                match control_flow {
                    ControlFlowAbort::Break => break,
                    ControlFlowAbort::Continue => continue,
                }
            }
        }
        Ok(())
    }
    fn visit_for(&self, stmt: &ForStmt) -> Result<(), JokerError> {
        if let Some(initializer) = &stmt.initializer {
            self.execute(initializer)?
        }
        while self.is_true(&self.evaluate(&stmt.condition)?) {
            if let Err(JokerError::Abort(AbortError::ControlFlow(control_flow))) =
                self.execute(&stmt.body)
            {
                match control_flow {
                    ControlFlowAbort::Break => break,
                    ControlFlowAbort::Continue => {
                        if let Some(increment) = &stmt.increment {
                            self.evaluate(increment)?;
                        }
                        continue;
                    }
                }
            }
            if let Some(increment) = &stmt.increment {
                self.evaluate(increment)?;
            }
        }
        Ok(())
    }
    fn visit_break(&self, _stmt: &BreakStmt) -> Result<(), JokerError> {
        Err(JokerError::Abort(AbortError::ControlFlow(
            ControlFlowAbort::Break,
        )))
    }
    fn visit_continue(&self, _stmt: &ContinueStmt) -> Result<(), JokerError> {
        Err(JokerError::Abort(AbortError::ControlFlow(
            ControlFlowAbort::Continue,
        )))
    }
}

impl ExprVisitor<Object> for Interpreter {
    fn visit_literal(&self, expr: &Literal) -> Result<Object, JokerError> {
        Ok(expr.value.clone())
    }
    fn visit_unary(&self, expr: &Unary) -> Result<Object, JokerError> {
        let r_expr: Object = self.evaluate(&expr.r_expr)?;
        match expr.l_opera.ttype {
            TokenType::Minus => match r_expr {
                Object::Literal(literal) => match literal {
                    ObL::I32(i32_) => Ok(Object::Literal(ObL::I32(-i32_))),
                    ObL::F64(f64_) => Ok(Object::Literal(ObL::F64(-f64_))),
                    _ => Err(JokerError::Interpreter(IntoIteratorError::new(
                        &expr.l_opera,
                        format!(
                            "[[Minus]] The literal cannot take negative values. {} !=> -{}",
                            literal, literal
                        ),
                    ))),
                },
            },
            TokenType::Bang => match r_expr {
                Object::Literal(ref literal) => match literal {
                    ObL::Bool(bool_) => Ok(Object::Literal(ObL::Bool(!bool_))),
                    _ => Err(JokerError::Interpreter(IntoIteratorError::new(
                        &expr.l_opera,
                        format!(
                            "[[Bang]] The literal cannot take reversed values. {} !=> !{}",
                            literal, literal
                        ),
                    ))),
                },
            },
            _ => Err(JokerError::Interpreter(IntoIteratorError::new(
                &expr.l_opera,
                String::from("Unreachable according to Literal Num!"),
            ))),
        }
    }
    fn visit_binary(&self, expr: &Binary) -> Result<Object, JokerError> {
        let l_expr: Object = self.evaluate(&expr.l_expr)?;
        let r_expr: Object = self.evaluate(&expr.r_expr)?;
        match expr.m_opera.ttype {
            TokenType::BangEqual => match (l_expr, r_expr) {
                (Object::Literal(ref l_literal), Object::Literal(ref r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::Bool(l_i32 != r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::Bool(l_f64 != r_f64))),
                    (ObL::Bool(l_bool), ObL::Bool(r_bool)) => Ok(Object::Literal(ObL::Bool(l_bool != r_bool))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Object::Literal(ObL::Bool(l_str != r_str))),
                    (ObL::Null, ObL::Null) => Ok(Object::Literal(ObL::Bool(false))),
                    (ObL::Null, _) | (_, ObL::Null)=> Ok(Object::Literal(ObL::Bool(true))),
                    _ => Err(JokerError::Interpreter(IntoIteratorError::new(
                        &expr.m_opera,
                        format!("[[BangEqual]] The literal cannot take bang equal values. !({l_literal} != {r_literal})")
                    )))
                }
            },
            TokenType::EqualEqual => match (l_expr, r_expr) {
                (Object::Literal(ref l_literal), Object::Literal(ref r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::Bool(l_i32 == r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::Bool(l_f64 == r_f64))),
                    (ObL::Bool(l_bool), ObL::Bool(r_bool)) => Ok(Object::Literal(ObL::Bool(l_bool == r_bool))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Object::Literal(ObL::Bool(l_str == r_str))),
                    (ObL::Null, ObL::Null) => Ok(Object::Literal(ObL::Bool(true))),
                    (ObL::Null, _) | (_, ObL::Null)=> Ok(Object::Literal(ObL::Bool(false))),
                    _ => Err(JokerError::Interpreter(IntoIteratorError::new(
                        &expr.m_opera,
                        format!("[[EqualEqual]] The literal cannot take equal values. !({l_literal} == {r_literal})")
                    )))
                }
            },
            TokenType::Greater => match (l_expr, r_expr) {
                (Object::Literal(ref l_literal), Object::Literal(ref r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::Bool(l_i32 > r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::Bool(l_f64 > r_f64))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Object::Literal(ObL::Bool(l_str > r_str))),
                    _ => Err(JokerError::Interpreter(IntoIteratorError::new(
                        &expr.m_opera,
                        format!("[[Greater]] The literal cannot take greater values. !({l_literal} > {r_literal})")
                    )))
                }
            },
            TokenType::GreaterEqual => match (l_expr, r_expr) {
                (Object::Literal(ref l_literal), Object::Literal(ref r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::Bool(l_i32 >= r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::Bool(l_f64 >= r_f64))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Object::Literal(ObL::Bool(l_str >= r_str))),
                    _ => Err(JokerError::Interpreter(IntoIteratorError::new(
                        &expr.m_opera,
                        format!("[[GreaterEqual]] The literal cannot take greater equal values. !({l_literal} >= {r_literal})")
                    )))
                }
            },
            TokenType::Less => match (l_expr, r_expr) {
                (Object::Literal(ref l_literal), Object::Literal(ref r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::Bool(l_i32 < r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::Bool(l_f64 < r_f64))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Object::Literal(ObL::Bool(l_str < r_str))),
                    _ => Err(JokerError::Interpreter(IntoIteratorError::new(
                        &expr.m_opera,
                        format!("[[Less]] The literal cannot take less values. !({l_literal} < {r_literal})")
                    )))
                }
            },
            TokenType::LessEqual => match (l_expr, r_expr) {
                (Object::Literal(ref l_literal), Object::Literal(ref r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::Bool(l_i32 <= r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::Bool(l_f64 <= r_f64))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Object::Literal(ObL::Bool(l_str <= r_str))),
                    _ => Err(JokerError::Interpreter(IntoIteratorError::new(
                        &expr.m_opera,
                        format!("[[LessEqual]] The literal cannot take less equal values. !({l_literal} <= {r_literal})")
                    )))
                }
            },
            TokenType::Plus => match (l_expr, r_expr) {
                (Object::Literal(ref l_literal), Object::Literal(ref r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::I32(l_i32 + r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::F64(l_f64 + r_f64))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => {
                        Ok(Object::Literal(ObL::Str(format!("{l_str}{r_str}"))))
                    },
                    _ => Err(JokerError::Interpreter(IntoIteratorError::new(
                        &expr.m_opera,
                        format!("[[Plus]] The literal cannot take plus values. !({l_literal} + {r_literal})")
                    )))
                }
            }
            TokenType::Minus => match (l_expr, r_expr) {
                (Object::Literal(ref l_literal), Object::Literal(ref r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::I32(l_i32 - r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::F64(l_f64 - r_f64))),
                    _ => Err(JokerError::Interpreter(IntoIteratorError::new(
                        &expr.m_opera,
                        format!("[[Minus]] The literal cannot take minus values. !({l_literal} - {r_literal})")
                    )))
                }
            }
            TokenType::Slash => match (l_expr, r_expr) {
                (Object::Literal(ref l_literal), Object::Literal(ref r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => {
                        if r_i32 != &0 {
                            Ok(Object::Literal(ObL::I32(l_i32 / r_i32)))
                        } else {
                            Err(JokerError::Interpreter(IntoIteratorError::new(
                                &expr.m_opera,
                                format!("[[Slash::ZeroSlashError]]. !({l_literal} / {r_literal})")
                            )))
                        }
                    },
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => {
                        if r_f64 != &0f64 {
                            Ok(Object::Literal(ObL::F64(l_f64 / r_f64)))
                        } else {
                            Err(JokerError::Interpreter(IntoIteratorError::new(
                                &expr.m_opera,
                                format!("[[Slash::ZeroSlashError]] . !({l_literal} / {r_literal})")
                            )))
                        }
                    },
                    _ => Err(JokerError::Interpreter(IntoIteratorError::new(
                        &expr.m_opera,
                        format!("[[Slash]] The literal cannot take slash values. !({l_literal} /{r_literal})")
                    )))
                }
            }
            TokenType::Star => match (l_expr, r_expr) {
                (Object::Literal(ref l_literal), Object::Literal(ref r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::I32(l_i32 * r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::F64(l_f64 * r_f64))),
                    (ObL::Str(str_), ObL::I32(i32_))
                    | (ObL::I32(i32_), ObL::Str(str_)) => {
                        let mut count = *i32_;
                        let mut r_str: String = String::with_capacity(size_of_val(str_)* count as usize);
                        while count > 0 {
                            r_str.push_str(str_);
                            count -= 1;
                        }
                        Ok(Object::Literal(ObL::Str(r_str)))
                    },
                    _ => Err(JokerError::Interpreter(IntoIteratorError::new(
                        &expr.m_opera,
                        format!("[[Star]] The literal cannot take star values. !({l_literal} * {r_literal})")
                    )))
                }
            }
            _ => Err(JokerError::Interpreter(IntoIteratorError::new(&expr.m_opera, String::from("Unreachable according other type!"))))
        }
    }
    fn visit_grouping(&self, expr: &Grouping) -> Result<Object, JokerError> {
        self.evaluate(&expr.expr)
    }
    fn visit_variable(&self, expr: &Variable) -> Result<Object, JokerError> {
        self.env.borrow().borrow().get(&expr.name)
    }
    fn visit_assign(&self, expr: &Assign) -> Result<Object, JokerError> {
        let value: Object = self.evaluate(&expr.value)?;
        self.env.borrow().borrow_mut().assign(&expr.name, &value)?;
        Ok(value)
    }
    fn visit_logical(&self, expr: &Logical) -> Result<Object, JokerError> {
        let l_object: Object = self.evaluate(&expr.l_expr)?;
        match expr.m_opera.ttype {
            TokenType::Or => {
                if self.is_true(&l_object) {
                    Ok(Object::Literal(ObL::Bool(true)))
                } else {
                    let r_object: Object = self.evaluate(&expr.r_expr)?;
                    if self.is_true(&r_object) {
                        Ok(Object::Literal(ObL::Bool(true)))
                    } else {
                        Ok(Object::Literal(ObL::Bool(false)))
                    }
                }
            }
            TokenType::And => {
                if !self.is_true(&l_object) {
                    Ok(Object::Literal(ObL::Bool(false)))
                } else {
                    let r_object: Object = self.evaluate(&expr.r_expr)?;
                    if self.is_true(&r_object) {
                        Ok(Object::Literal(ObL::Bool(true)))
                    } else {
                        Ok(Object::Literal(ObL::Bool(false)))
                    }
                }
            }
            _ => Err(JokerError::Interpreter(IntoIteratorError::new(
                &expr.m_opera,
                format!("Unsupported logic operator: {:?}", expr.m_opera.ttype),
            ))),
        }
    }
    fn visit_trinomial(&self, expr: &Trinomial) -> Result<Object, JokerError> {
        let condition = expr.condition.accept(self)?;
        if self.is_true(&condition) {
            let l_object = expr.l_expr.accept(self)?;
            Ok(l_object)
        } else {
            let r_object = expr.r_expr.accept(self)?;
            Ok(r_object)
        }
    }
}

#[derive(Debug)]
pub struct IntoIteratorError {
    line: usize,
    where_: String,
    msg: String,
}
impl IntoIteratorError {
    pub fn new(token: &Token, msg: String) -> IntoIteratorError {
        let where_: String = if token.ttype == TokenType::Eof {
            String::from(" at end")
        } else {
            format!(" at '{}'", token.lexeme)
        };
        IntoIteratorError {
            line: token.line,
            where_,
            msg,
        }
    }
    pub fn error(line: usize, msg: String) -> IntoIteratorError {
        IntoIteratorError {
            line,
            where_: String::from(""),
            msg,
        }
    }
}
impl ReportError for IntoIteratorError {
    fn report(&self) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, self.where_, self.msg
        );
    }
}

#[cfg(test)]
mod tests {

    use super::super::{error::ReportError, object::literal_null, token::Token};
    use super::*;

    fn maker_literal_i32_expr(v: i32) -> Box<Expr> {
        Box::new(Expr::Literal(Literal {
            value: Object::Literal(ObL::I32(v)),
        }))
    }
    fn maker_literal_f64_expr(v: f64) -> Box<Expr> {
        Box::new(Expr::Literal(Literal {
            value: Object::Literal(ObL::F64(v)),
        }))
    }
    fn maker_literal_str_expr(v: String) -> Box<Expr> {
        Box::new(Expr::Literal(Literal {
            value: Object::Literal(ObL::Str(v)),
        }))
    }
    fn maker_literal_bool_expr(v: bool) -> Box<Expr> {
        Box::new(Expr::Literal(Literal {
            value: Object::Literal(ObL::Bool(v)),
        }))
    }
    fn maker_literal_null_expr() -> Box<Expr> {
        Box::new(Expr::Literal(Literal {
            value: Object::Literal(ObL::Null),
        }))
    }
    fn maker_token(ttype: TokenType) -> Token {
        let lexeme = ttype.to_string();
        Token {
            ttype,
            lexeme,
            literal: literal_null(),
            line: 0,
        }
    }
    fn maker_unary_expr(ttype: TokenType, v: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::Unary(Unary {
            l_opera: maker_token(ttype),
            r_expr: v,
        }))
    }
    fn maker_grouping_expr(expr: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::Grouping(Grouping { expr }))
    }
    fn maker_binary_expr(l_expr: Box<Expr>, m_opera: Token, r_expr: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::Binary(Binary {
            l_expr,
            m_opera,
            r_expr,
        }))
    }

    #[test]
    fn test_simple_expr() {
        // (-123)*(200/2)
        let expr: Expr = Expr::Binary(Binary {
            l_expr: maker_unary_expr(TokenType::Minus, maker_literal_i32_expr(123)),
            m_opera: maker_token(TokenType::Star),
            r_expr: maker_grouping_expr(maker_binary_expr(
                maker_literal_i32_expr(200),
                maker_token(TokenType::Slash),
                maker_literal_i32_expr(2),
            )),
        });

        let interpreter: Interpreter = Interpreter::new();
        match interpreter.evaluate(&expr) {
            Ok(value) => assert_eq!(value, Object::Literal(ObL::I32(-12300))),
            Err(err) => err.report(),
        };
    }

    #[test]
    fn test_literal_i32_expr() {
        let expr = maker_literal_i32_expr(32);
        let interpreter: Interpreter = Interpreter::new();
        match interpreter.evaluate(&expr) {
            Ok(value) => assert_eq!(value, Object::Literal(ObL::I32(32))),
            Err(err) => err.report(),
        };
    }

    #[test]
    fn test_literal_f64_expr() {
        let expr = maker_literal_f64_expr(320.0);
        let interpreter: Interpreter = Interpreter::new();
        match interpreter.evaluate(&expr) {
            Ok(value) => assert_eq!(value, Object::Literal(ObL::F64(320.0))),
            Err(err) => err.report(),
        };
    }

    #[test]
    fn test_literal_str_expr() {
        let expr = maker_literal_str_expr(String::from("string"));
        let interpreter: Interpreter = Interpreter::new();
        match interpreter.evaluate(&expr) {
            Ok(value) => assert_eq!(value, Object::Literal(ObL::Str(String::from("string")))),
            Err(err) => err.report(),
        };
    }

    #[test]
    fn test_literal_bool_expr() {
        let expr = maker_literal_bool_expr(true);
        let interpreter: Interpreter = Interpreter::new();
        match interpreter.evaluate(&expr) {
            Ok(value) => assert_eq!(value, Object::Literal(ObL::Bool(true))),
            Err(err) => err.report(),
        };
    }

    #[test]
    fn test_literal_null_expr() {
        let expr = maker_literal_null_expr();
        let interpreter: Interpreter = Interpreter::new();
        match interpreter.evaluate(&expr) {
            Ok(value) => assert_eq!(value, Object::Literal(ObL::Null)),
            Err(err) => err.report(),
        };
    }
}
