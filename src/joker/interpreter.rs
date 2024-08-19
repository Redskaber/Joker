//! This file is interpreters.rs
//!
//!

use std::{cell::RefCell, collections::HashMap, error::Error, fmt::Display, hash::Hash, rc::Rc};

use crate::joker::object::Lambda;

use super::{
    abort::{AbortError, ControlFlowAbort},
    ast::{
        Assign, Binary, BlockStmt, BreakStmt, Call, ClassStmt, ContinueStmt, Expr, ExprAcceptor,
        ExprStmt, ExprVisitor, ForStmt, FunStmt, Grouping, IfStmt, Lambda as LambdaExpr, Literal,
        Logical, PrintStmt, ReturnStmt, Stmt, StmtAcceptor, StmtVisitor, Trinomial, Unary, VarStmt,
        Variable, WhileStmt,
    },
    callable::{ArgumentError, CallError, Callable, NonCallError},
    env::Env,
    error::{JokerError, ReportError, SystemError, SystemTimeError},
    object::{
        literal_null, Caller, Class, Function, Literal as ObL, NativeFunction, Object, UserFunction,
    },
    token::{Token, TokenType},
};

#[derive(Debug)]
pub struct Interpreter {
    pub global: Rc<RefCell<Env>>,
    local_resolve: RefCell<HashMap<Expr, usize>>,
    pub run_env: RefCell<Rc<RefCell<Env>>>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let global: Rc<RefCell<Env>> = Rc::new(RefCell::new(Env::new()));
        global.borrow_mut().define(
            String::from("clock"),
            Object::Caller(Caller::Func(Function::Native(NativeFunction::new(|| {
                use std::time::SystemTime;

                #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                pub struct NativeClock;

                impl Callable for NativeClock {
                    fn call(
                        &self,
                        _interpreter: &Interpreter,
                        _arguments: &[Object],
                    ) -> Result<Object, JokerError> {
                        match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
                            Ok(duration) => {
                                Ok(Object::Literal(ObL::F64(duration.as_millis() as f64)))
                            }
                            Err(err) => Err(JokerError::System(SystemError::Time(
                                SystemTimeError::report_error(format!(
                                    "Native clock return invalid duration: {:?}.",
                                    err
                                )),
                            ))),
                        }
                    }
                    fn arity(&self) -> usize {
                        0
                    }
                }
                impl Display for NativeClock {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        write!(f, "clock")
                    }
                }

                NativeFunction {
                    fun: Rc::new(NativeClock {}),
                }
            })))),
        );

        Interpreter {
            global: Rc::clone(&global),
            local_resolve: RefCell::new(HashMap::new()),
            run_env: RefCell::new(Rc::clone(&global)),
        }
    }
    fn is_true(&self, object: &Object) -> bool {
        matches!(object, Object::Literal(ObL::Bool(true)))
    }
    fn execute(&self, stmt: &Stmt) -> Result<(), JokerError> {
        stmt.accept(self)
    }
    pub fn execute_block(&self, stmts: &[Stmt], block_env: Env) -> Result<(), JokerError> {
        let previous = self.run_env.replace(Rc::new(RefCell::new(block_env)));
        let result = stmts.iter().try_for_each(|stmt| self.execute(stmt));
        self.run_env.replace(previous);
        result
    }
    pub fn evaluate(&self, expr: &Expr) -> Result<Object, JokerError> {
        expr.accept(self)
    }
    pub fn evaluate_local(&self, expr: &Expr, env: Env) -> Result<Object, JokerError> {
        let previous = self.run_env.replace(Rc::new(RefCell::new(env)));
        let result = expr.accept(self);
        self.run_env.replace(previous);
        result
    }
    pub fn resolve(&self, expr: Expr, depth: usize) {
        println!(
            "[{:>10}][{:>20}]:\texpr:{:?},\tdepth:{}",
            "inter", "resolve", expr, depth
        );
        self.local_resolve.borrow_mut().insert(expr, depth);
    }
    pub fn resolve_call(&self, name: &Token, expr: &Expr) -> Result<Object, JokerError> {
        println!(
            "[{:>10}][{:>20}]:\texpr: {:?},\tname: {}",
            "inter", "resolve_call", expr, name
        );
        self.look_up_variable(name, expr)
    }
    fn look_up_variable(&self, name: &Token, expr: &Expr) -> Result<Object, JokerError> {
        println!(
            "[{:>10}][{:>20}]:\texpr: {:?},\tname: {}",
            "inter", "look_up_variable", expr, name
        );
        match self.local_resolve.borrow().get(expr) {
            Some(depth) => self
                .run_env
                .borrow()
                .borrow_mut()
                .get_with_depth(*depth, name),
            None => self.global.borrow_mut().get(name),
        }
    }
    pub fn interpreter(&self, stmts: &[Stmt]) -> Result<(), JokerError> {
        // let printer: AstPrinter = AstPrinter::new();
        for stmt in stmts {
            // printer.println(stmt);
            self.execute(stmt)? // not jump
        }
        Ok(())
    }
    pub fn println_local(&self) {
        println!("{:?}", self.run_env);
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
        let value = match &stmt.value {
            Some(expr) => self.evaluate(expr)?,
            None => literal_null(),
        };
        self.run_env
            .borrow()
            .borrow_mut()
            .define(stmt.name.lexeme.clone(), value);
        Ok(())
    }
    fn visit_block(&self, stmt: &BlockStmt) -> Result<(), JokerError> {
        let block_env = Env::new_with_enclosing(Rc::clone(&self.run_env.borrow()));
        self.execute_block(&stmt.stmts, block_env)
    }
    fn visit_if(&self, stmt: &IfStmt) -> Result<(), JokerError> {
        if self.is_true(&self.evaluate(&stmt.condition)?) {
            self.execute(&stmt.then_branch)?;
        } else if let Some(box_stmt) = &stmt.else_branch {
            self.execute(box_stmt)?;
        }
        Ok(())
    }
    fn visit_while(&self, stmt: &WhileStmt) -> Result<(), JokerError> {
        while self.is_true(&self.evaluate(&stmt.condition)?) {
            if let Err(err) = self.execute(&stmt.body) {
                match err {
                    JokerError::Abort(AbortError::ControlFlow(control_flow)) => {
                        match control_flow {
                            ControlFlowAbort::Break => break,
                            ControlFlowAbort::Continue => continue,
                            ControlFlowAbort::Return(return_value) => {
                                return Err(JokerError::Abort(AbortError::ControlFlow(
                                    ControlFlowAbort::Return(return_value),
                                )));
                            }
                        }
                    }
                    _ => return Err(err),
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
            if let Err(err) = self.execute(&stmt.body) {
                match err {
                    JokerError::Abort(AbortError::ControlFlow(control_flow)) => {
                        match control_flow {
                            ControlFlowAbort::Break => break,
                            ControlFlowAbort::Continue => {
                                if let Some(increment) = &stmt.increment {
                                    self.evaluate(increment)?;
                                }
                                continue;
                            }
                            ControlFlowAbort::Return(return_value) => {
                                return Err(JokerError::Abort(AbortError::ControlFlow(
                                    ControlFlowAbort::Return(return_value),
                                )));
                            }
                        }
                    }
                    _ => return Err(err),
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
    fn visit_fun(&self, stmt: &FunStmt) -> Result<(), JokerError> {
        let fun = Object::Caller(Caller::Func(Function::User(UserFunction::new(
            stmt,
            Rc::clone(&self.run_env.borrow()),
        ))));
        self.run_env
            .borrow()
            .borrow_mut()
            .define(stmt.name.lexeme.clone(), fun);
        Ok(())
    }
    fn visit_return(&self, stmt: &ReturnStmt) -> Result<(), JokerError> {
        let value: Object = match &stmt.value {
            Some(expr) => self.evaluate(expr)?,
            None => literal_null(),
        };
        Err(JokerError::Abort(AbortError::ControlFlow(
            ControlFlowAbort::Return(value),
        )))
    }
    fn visit_class(&self, stmt: &ClassStmt) -> Result<(), JokerError> {
        self.run_env
            .borrow()
            .borrow_mut()
            .define(stmt.name.lexeme.clone(), literal_null());
        let class: Object = Object::Caller(Caller::Class(Class::new(stmt.name.lexeme.clone())));
        self.run_env
            .borrow()
            .borrow_mut()
            .assign(&stmt.name, class)?;
        Ok(())
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
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.l_opera,
                        format!(
                            "[[Minus]] The literal cannot take negative values. {} !=> -{}",
                            literal, literal
                        ),
                    ))),
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                    &expr.l_opera,
                    String::from("not impl Minus!"),
                ))),
            },
            TokenType::Bang => match r_expr {
                Object::Literal(ref literal) => match literal {
                    ObL::Bool(bool_) => Ok(Object::Literal(ObL::Bool(!bool_))),
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.l_opera,
                        format!(
                            "[[Bang]] The literal cannot take reversed values. {} !=> !{}",
                            literal, literal
                        ),
                    ))),
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                    &expr.l_opera,
                    String::from("not impl Bang!"),
                ))),
            },
            _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                &expr.l_opera,
                String::from("Unreachable according to Literal Num!"),
            ))),
        }
    }
    fn visit_binary(&self, expr: &Binary) -> Result<Object, JokerError> {
        let l_expr: Object = self.evaluate(&expr.l_expr)?;
        let r_expr: Object = self.evaluate(&expr.r_expr)?;
        match expr.m_opera.ttype {
            TokenType::BangEqual => match (&l_expr, &r_expr) {
                (Object::Literal(l_literal), Object::Literal(r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::Bool(l_i32 != r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::Bool(l_f64 != r_f64))),
                    (ObL::Bool(l_bool), ObL::Bool(r_bool)) => Ok(Object::Literal(ObL::Bool(l_bool != r_bool))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Object::Literal(ObL::Bool(l_str != r_str))),
                    (ObL::Null, ObL::Null) => Ok(Object::Literal(ObL::Bool(false))),
                    (ObL::Null, _) | (_, ObL::Null)=> Ok(Object::Literal(ObL::Bool(true))),
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("[[BangEqual]] The literal cannot take bang equal values. !({l_literal} != {r_literal})")
                    )))
                }
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("not impl BangEqual! (l_expr: {}, r_expr: {})", l_expr, r_expr),
                    ))),
            },
            TokenType::EqualEqual => match (&l_expr, &r_expr) {
                (Object::Literal(l_literal), Object::Literal(r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::Bool(l_i32 == r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::Bool(l_f64 == r_f64))),
                    (ObL::Bool(l_bool), ObL::Bool(r_bool)) => Ok(Object::Literal(ObL::Bool(l_bool == r_bool))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Object::Literal(ObL::Bool(l_str == r_str))),
                    (ObL::Null, ObL::Null) => Ok(Object::Literal(ObL::Bool(true))),
                    (ObL::Null, _) | (_, ObL::Null)=> Ok(Object::Literal(ObL::Bool(false))),
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("[[EqualEqual]] The literal cannot take equal values. !({l_literal} == {r_literal})")
                    )))
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("not impl EqualEqual! (l_expr: {}, r_expr: {})", l_expr, r_expr),
                    ))),
            },
            TokenType::Greater => match (&l_expr, &r_expr) {
                (Object::Literal(l_literal), Object::Literal(r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::Bool(l_i32 > r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::Bool(l_f64 > r_f64))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Object::Literal(ObL::Bool(l_str > r_str))),
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("[[Greater]] The literal cannot take greater values. !({l_literal} > {r_literal})")
                    )))
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("not impl Greater! (l_expr: {}, r_expr: {})", l_expr, r_expr),
                    ))),
            },
            TokenType::GreaterEqual => match (&l_expr, &r_expr) {
                (Object::Literal(l_literal), Object::Literal(r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::Bool(l_i32 >= r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::Bool(l_f64 >= r_f64))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Object::Literal(ObL::Bool(l_str >= r_str))),
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("[[GreaterEqual]] The literal cannot take greater equal values. !({l_literal} >= {r_literal})")
                    )))
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("not impl GreaterEqual! (l_expr: {}, r_expr: {})", l_expr, r_expr),
                    ))),
            },
            TokenType::Less => match (&l_expr, &r_expr) {
                (Object::Literal(l_literal), Object::Literal(r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::Bool(l_i32 < r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::Bool(l_f64 < r_f64))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Object::Literal(ObL::Bool(l_str < r_str))),
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("[[Less]] The literal cannot take less values. !({l_literal} < {r_literal})")
                    )))
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("not impl Less! (l_expr: {}, r_expr: {})", l_expr, r_expr),
                    ))),
            },
            TokenType::LessEqual => match (&l_expr, &r_expr) {
                (Object::Literal(l_literal), Object::Literal(r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::Bool(l_i32 <= r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::Bool(l_f64 <= r_f64))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Object::Literal(ObL::Bool(l_str <= r_str))),
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("[[LessEqual]] The literal cannot take less equal values. !({l_literal} <= {r_literal})")
                    )))
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("not impl LessEqual! (l_expr: {}, r_expr: {})", l_expr, r_expr),
                    ))),
            },
            TokenType::Plus => match (&l_expr, &r_expr) {
                (Object::Literal(l_literal), Object::Literal(r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::I32(l_i32 + r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::F64(l_f64 + r_f64))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => {
                        Ok(Object::Literal(ObL::Str(format!("{l_str}{r_str}"))))
                    },
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("[[Plus]] The literal cannot take plus values. !({l_literal} + {r_literal})")
                    )))
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("not impl Plus! (l_expr: {}, r_expr: {})", l_expr, r_expr),
                    ))),
            }
            TokenType::Minus => match (&l_expr, &r_expr) {
                (Object::Literal(l_literal), Object::Literal(r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Object::Literal(ObL::I32(l_i32 - r_i32))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Object::Literal(ObL::F64(l_f64 - r_f64))),
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("[[Minus]] The literal cannot take minus values. !({l_literal} - {r_literal})")
                    )))
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("not impl Minus! (l_expr: {}, r_expr: {})", l_expr, r_expr),
                    ))),
            }
            TokenType::Slash => match (&l_expr, &r_expr) {
                (Object::Literal(l_literal), Object::Literal(r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => {
                        if r_i32 != &0 {
                            Ok(Object::Literal(ObL::I32(l_i32 / r_i32)))
                        } else {
                            Err(JokerError::Interpreter(InterpreterError::report_error(
                                &expr.m_opera,
                                format!("[[Slash::ZeroSlashError]]. !({l_literal} / {r_literal})")
                            )))
                        }
                    },
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => {
                        if r_f64 != &0f64 {
                            Ok(Object::Literal(ObL::F64(l_f64 / r_f64)))
                        } else {
                            Err(JokerError::Interpreter(InterpreterError::report_error(
                                &expr.m_opera,
                                format!("[[Slash::ZeroSlashError]] . !({l_literal} / {r_literal})")
                            )))
                        }
                    },
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("[[Slash]] The literal cannot take slash values. !({l_literal} / {r_literal})")
                    )))
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("not impl Slash! (l_expr: {}, r_expr: {})", l_expr, r_expr),
                    ))),
            }
            TokenType::Star => match (&l_expr, &r_expr) {
                (Object::Literal(l_literal), Object::Literal(r_literal)) => match (l_literal, r_literal) {
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
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("[[Star]] The literal cannot take star values. !({l_literal} * {r_literal})")
                    )))
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("not impl Star! (l_expr: {}, r_expr: {})", l_expr, r_expr),
                    ))),
            }
            _ => Err(JokerError::Interpreter(InterpreterError::report_error(&expr.m_opera, String::from("Unreachable according other type!"))))
        }
    }
    fn visit_grouping(&self, expr: &Grouping) -> Result<Object, JokerError> {
        self.evaluate(&expr.expr)
    }
    fn visit_variable(&self, expr: &Variable) -> Result<Object, JokerError> {
        self.look_up_variable(&expr.name, &Expr::Variable(expr.clone()))
    }
    fn visit_assign(&self, expr: &Assign) -> Result<Object, JokerError> {
        let value: Object = self.evaluate(&expr.value)?;
        match self.local_resolve.borrow().get(&Expr::Assign(expr.clone())) {
            Some(depth) => self.run_env.borrow().borrow_mut().assign_with_depth(
                *depth,
                &expr.name,
                value.clone(),
            )?,
            None => self.global.borrow_mut().assign(&expr.name, value.clone())?,
        }
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
            _ => Err(JokerError::Interpreter(InterpreterError::report_error(
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
    fn visit_call(&self, expr: &Call) -> Result<Object, JokerError> {
        let callee: Object = self.evaluate(&expr.callee)?;

        let mut arguments: Vec<Object> = Vec::new();
        for arg in &expr.arguments {
            arguments.push(self.evaluate(arg)?);
        }

        if let Object::Caller(caller) = callee {
            if arguments.len() != caller.arity() {
                return Err(JokerError::Call(CallError::Argument(
                    ArgumentError::report_error(
                        &expr.paren,
                        format!(
                            "call expected {} arguments but got {}.",
                            caller.arity(),
                            arguments.len()
                        ),
                    ),
                )));
            }
            Ok(caller.call(self, &arguments)?)
        } else {
            Err(JokerError::Call(CallError::NonCallable(
                NonCallError::report_error(
                    &expr.paren,
                    String::from("Can only call functions and classes."),
                ),
            )))
        }
    }
    fn visit_lambda(&self, expr: &LambdaExpr) -> Result<Object, JokerError> {
        let lambda: Object = Object::Caller(Caller::Lambda(Lambda::new(
            expr,
            Rc::clone(&self.run_env.borrow()),
        )));
        Ok(lambda)
    }
}

#[derive(Debug)]
pub struct InterpreterError {
    line: usize,
    where_: String,
    msg: String,
}

impl InterpreterError {
    pub fn new(token: &Token, msg: String) -> InterpreterError {
        let where_: String = if token.ttype == TokenType::Eof {
            String::from(" at end")
        } else {
            format!(" at '{}'", token.lexeme)
        };
        InterpreterError {
            line: token.line,
            where_,
            msg,
        }
    }
    pub fn report_error(token: &Token, msg: String) -> InterpreterError {
        let inter_err = InterpreterError::new(token, msg);
        inter_err.report();
        inter_err
    }
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "InterpreterError(line: {}, where: {}, msg: {})",
            self.line, self.where_, self.msg
        )
    }
}

impl Error for InterpreterError {}

impl ReportError for InterpreterError {
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
