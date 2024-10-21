/*
* Abstract Syntax Tree(AST):
*	An abstract syntax tree (AST) is a tree-like representation of the source code.
*	It is a way to represent the code in a way that is easy to manipulate and process.
*
*   Declaration    -> classDecl
*					| funcDecl		-> "fn" ident "(" ident? ("," ident)* ")" ("->" ident)? blockStmt
*					| varDecl		-> parse_var -> ident_const -> make_const
*                   | Statement
*
*   Statement      -> exprStmt -> expr -> precedence -> dispath { }
*					| forStmt		-> "for" "(" varDecl | expr ; expr; expr ")" Statement
*					| ifStmt		-> "if" "(" expr ")" Statement ("else" Statement)?
* 					| printStmt		-> "print" expr ;
*					| returnStmt	-> "return" expr? ;
*					| whileStmt     -> "while" "(" expr ")" Statement
* 					| matchStmt		-> "match" "(" expr ")" "{" 
*											expr "=>" Statement ("," expr "=>" Statement)* 
*										"}"
*				    | blockStmt		->  "{" Declaration* "}"
*
* 
*	switchStmt     → "switch" "(" expression ")"
*						"{" switchCase* defaultCase? "}" ;
*	switchCase     → "case" expression ":" statement* ;
*	defaultCase    → "default" ":" statement* ;
* 
* 
* 
* 
* stack effect
* 
* TODO: handler codes: uint8_t && index_t relations;
* TODO: global variables store and find optimization.
* 
* TODO: static check: e.g.
*   ```joker
*       fn use_var() {
*           print oops; // error: variable 'oops' is not defined.
*       }
* 
*      var ooops = "too many o's";
*   ``` 
*   should in compile time error.?
* 
* TODO: add keyword 'const' to declare constant variable.
* TODO: could have separate instructions for conditional jumps that implicitly pop and those that don’t.
* TODO: add match keyword to match pattern in switch statement. 
* TODO: add ?: operator to handle ternary operator.
* TODO：add 'continue' and 'break' statement to loop statement.
* TODO: 'goto'?
* TODO: jump table used ?
* TODO: lambda function? anonymous function?
*/




#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "error.h"
#include "hashmap.h"
#include "string.h"
#include "fn.h"
#include "compiler.h"

#include "parser.h"

#ifdef debug_print_code
#include "debug.h"
#endif



/* Parser */
static index_t make_constent(Parser* self, Chunk* chunk, Value value);
static index_t identifier_constant(Parser* self, VirtualMachine* vm, Token* token);
static void emit_constant(Parser* self, Chunk* chunk, Value value);
static void emit_return(Parser* self, Chunk* chunk);
static void emit_byte(Parser* self, Chunk* chunk, uint8_t byte);
static void emit_bytes(Parser* self, Chunk* chunk, uint8_t opcode, uint8_t operand);
static int emit_jump(Parser* self, Chunk* chunk, uint8_t opcode);
static void patch_jump(Parser* self, Chunk* chunk, int offset);

static void synchronize(Parser* self);
static TokenNode* parse_advance(Parser* self);
static bool parse_check(Parser* self, TokenType type);
static bool parse_match(Parser* self, TokenType type);
static bool parse_is_at_end(Parser* self);
static void parse_consume(Parser* self, TokenType type, const char* message);
static void parse_error_at(Parser* self, Token* error, const char* message);
void parse_error_at_current(Parser* self, const char* message);
void parse_error_at_previous(Parser* self, const char* message);


/* Abstract Syntax Tree(AST)*/
static void parse_declaration(Parser* self, VirtualMachine* vm);
static void parse_fn_declaration(Parser* self, VirtualMachine* vm);
static void parse_function(Parser* self, VirtualMachine* vm, FnType type);
static void parse_var_declaraton(Parser* self, VirtualMachine* vm);
static void parse_named_variable(Parser* self, VirtualMachine* vm, Token* variable, bool can_assign);
static index_t parse_variable(Parser* self, VirtualMachine* vm, const char* message);
static void parse_statement(Parser* self, VirtualMachine* vm);
static void parse_if_statement(Parser* self, VirtualMachine* vm);
static void parse_while_statement(Parser* self, VirtualMachine* vm);
static void parse_for_statement(Parser* self, VirtualMachine* vm);
static void parse_block_statement(Parser* self, VirtualMachine* vm);
static void parse_print_stmtement(Parser* self, VirtualMachine* vm);
static void parse_expr_statement(Parser* self, VirtualMachine* vm);
static void parse_return_statement(Parser* self, VirtualMachine* vm);
static void parse_precedence(Parser* self, VirtualMachine* vm, Precedence outer_precedence);
static void parse_expression(Parser* self, VirtualMachine* vm);


/* Compiler */
static Fn* end_compiler(Parser* self, VirtualMachine* vm);
static void begin_scope(Compiler* compiler);
static void end_scope(Parser* self, Compiler* compiler, Chunk* chunk);
static void current_into_sub_compiler(VirtualMachine* vm, Compiler* sub, FnType type, Parser* parser);
static void current_from_sub_compiler(VirtualMachine* vm);
static bool identifiers_equal(Token* a, Token* b);
static void declare_variable(Parser* self, Compiler* compiler, Token* name);
static void define_variable(Parser* self, Compiler* compiler, Chunk* chunk, uint8_t index);
static void add_local(Parser* self, Compiler* compiler, Token* name);
static void mark_initialized(Compiler* compiler);
static int resolve_local(Parser* self, Compiler* compiler, Token* name);



/* get handle token's rule(prefix, infix, ...)[function pointer] */
ParseRule* get_rule(TokenType type) {
	return &static_syntax_rules[type];
}


void init_parser(Parser* self, TokenList* tokens) {
	/* initialize parser with tokens, reset current and previous token pointer */
	reset_foreach_pointer(tokens);
	self->tokens = tokens;
	self->current = tokens->head;
	self->previous = NULL;
	self->had_error = false;
	self->panic_mode = false;
}

void free_parser(Parser* self) {
	// TODO: free(parser);  // error: Parser is not malloc'ed memory, used loacal variable instead.
	self->tokens = NULL;
	self->current = NULL;
	self->previous = NULL;
	self->had_error = false;
	self->panic_mode = false;
}

void print_parser(Parser* self) {
	printf("Parser:\n");
	printf("tokens: ");print_tokens(self->tokens);
	printf("current: ");print_token_node(self->current);printf("\n");
	printf("previous: ");print_token_node(self->previous);printf("\n");
	printf("had_error: %d\n", self->had_error);
	printf("panic_mode: %d\n", self->panic_mode);
}

static Fn* end_compiler(Parser* self, VirtualMachine* vm) {
	emit_return(self, current_chunk(vm->compiler));

	// builded func return && return parent compiler's func
	Fn* fn = vm->compiler->function;
#ifdef debug_print_code
	if (!self->had_error) {
		disassemble_chunk(current_chunk(vm->compiler), 
			is_anonymous_fn(fn)? "<script>" : fn->name->chars);
	}
#endif
	current_from_sub_compiler(vm);
	return fn;
}

/* check if is at end of file, return true if is at end of file, false otherwise */
static bool parse_is_at_end(Parser* self) {
	if (self->current == NULL) {
		parse_error_at_previous(self, "[Parser::parse_is_at_end] Expected match end of file(eof token), Found Unexpected end of file(not a token).");
		return true;
	}
	return self->current->token.type == token_eof;
}

/* advance to next token, return previous token, if is at end of file, return NULL */
static TokenNode* parse_advance(Parser* self) {
	if (parse_is_at_end(self)) return NULL;
	self->previous = self->current;
	self->current = self->current->next;
	return self->previous;
}

static bool parse_check(Parser* self, TokenType type) {
	if (self->current == NULL) return false;
	return self->current->token.type == type;
}

static bool parse_match(Parser* self, TokenType type) {
	if (!parse_check(self, type)) return false;
	parse_advance(self);
	return true;
}

static void parse_consume(Parser* self, TokenType type, const char* message) {
	if (self->current->token.type == type) {
		parse_advance(self);
		return;
	}
	parse_error_at_current(self, message);
}


static void emit_byte(Parser* self, Chunk* chunk, uint8_t byte) {
	write_chunk(chunk, byte, self->previous->token.line);
}

static void emit_constant(Parser* self, Chunk* chunk, Value value) {
	write_constant(chunk, value, self->previous->token.line);
}

static void emit_bytes(Parser* self, Chunk* chunk, uint8_t opcode, uint8_t operand) {
	emit_byte(self, chunk, opcode);
	emit_byte(self, chunk, operand);
}

static void emit_return(Parser* self, Chunk* chunk) {
	emit_byte(self, chunk, op_return);
}

/*
* chunk->count - 2: return index of jump offset
*						|----------< -2 >---------|
*						V						  V
* [code, code , {code, jump_offset, jump_offset}, ...]
*/
static int emit_jump(Parser* self, Chunk* chunk, uint8_t opcode) {
	emit_byte(self, chunk, opcode);
	emit_byte(self, chunk, 0xff);		// placeholder for jump offset
	emit_byte(self, chunk, 0xff);		// placeholder for jump offset 2^(8+8) = 65535
	return chunk->count - 2;			// return index of jump offset
}
/*
* patch_jump:
*	- calculate jump offset: chunk->count - offset - 2
*	- write high 8 bits of jump offset to chunk->code[offset]
* 	- write low 8 bits of jump offset to chunk->code[offset+1]
* 
*   | --< offset >-- |									  |(chunk->count)
*	V				 V |<---------(2)--------->|<-jump->| V
* [code, code , {jump, jump_offset, jump_offset}, ......, code,code, ...]
*/
static void patch_jump(Parser* self, Chunk* chunk, int offset) {
	int jump = chunk->count - offset - 2;
	if (jump > UINT16_MAX) {
		parse_error_at_current(self, "[Parser::patch_jump] Expected jump offset less than 2^16, Found jump offset greater than 2^16.");
	}

	chunk->code[offset] = (jump >> 8) & 0xff;   // high 8 bits of jump offset
	chunk->code[offset + 1] = jump & 0xff;		// low 8 bits of jump offset
}

/*
* emit_loop:
*	- emit op_loop
*	- calculate loop offset: chunk->count - loop_start + 2
* 	- emit high 8 bits of loop offset
* 	- emit low 8 bits of loop offset
*
*	while
*    |     <- loop_start  <-——--- loop_start
*    |     (				↑
*    |     condition		|
*    |     )				|
*    |     <- exit_jump		|
*    |     {				|
*    |         statement	|
*    |     }				|
*    |     <--- op_loop --->|
*    |        | loop_offset |
*    |        V loop_offset | <- chunck->count
*	=> offset = chunk->count - loop_start + 2(loop_offset)
*/
static void emit_loop(Parser* self, Chunk* chunk, int loop_start) {
	emit_byte(self, chunk, op_loop);

	int offset = chunk->count - loop_start + 2;
	if (offset > UINT16_MAX) {
		parse_error_at_current(self, "[Parser::emit_loop] Expected loop offset less than 2^16, Found loop offset greater than 2^16.");
	}

	emit_byte(self, chunk, (offset >> 8) & 0xff);	// high 8 bits of loop offset
	emit_byte(self, chunk, offset & 0xff);			// low 8 bits of loop offset
}


static index_t make_constent(Parser* self, Chunk* chunk, Value value) {
	index_t index = add_constant(chunk, value);
	if (index > UINT16_MAX) {
		parse_error_at_current(self, "[Parser::make_constent] Expected constant index less than 2^16, Found constant index greater than 2^16.");
	}
	return index;
}

/*  “Declaring” is when the variable is added to the scope, and “defining” is when it becomes available for use */
static void declare_variable(Parser* self, Compiler* compiler, Token* name) {
	if (compiler->scope_depth == 0) return;
	
	for (int i = compiler->local_count - 1; i >= 0; i--) {
		LocalVariable* local = &compiler->locals[i];
		if (local->depth != -1 && local->depth < compiler->scope_depth) {
			break;
		}

		if (identifiers_equal(local->name, name)) {
			parse_error_at_current(self, "[Parser::declare_variable] Variable with this name already declared in this scope.");
		}
	}

	add_local(self, compiler, name);
}

static void define_variable(Parser* self, Compiler* compiler, Chunk* chunk, uint8_t index) {
	if (compiler->scope_depth > 0) {
		mark_initialized(compiler);
		return;
	}
	emit_bytes(self, chunk, op_define_global, index);
}

static bool identifiers_equal(Token* left, Token* right) {
	return left->length == right->length && 
		memcmp(left->start, right->start, left->length) == 0;
}

/* identifier is string constant so big, so we add to constants table, through index get value
* TODO: string -> constent table { (more reference same value)↓ -> memory optimization }
*/
static index_t identifier_constant(Parser* self, VirtualMachine* vm, Token* token) {
	return make_constent(self, current_chunk(vm->compiler), macro_obj_from_val(new_string(&vm->strings, token->start, token->length)));
}

static void add_local(Parser* self, Compiler* compiler, Token* name) {
	if (compiler->local_count == uint8_count) {
		parse_error_at_current(self, "[Parser::add_local] Expected local variable count less than 256, Found local variable count greater than 256.");
		return;
	}
	LocalVariable* local = &compiler->locals[compiler->local_count++];
	local->name = name;
	local->depth = -1;		// default depth is -1, means not defined.
}

static void mark_initialized(Compiler* compiler) {
	if (compiler->scope_depth == 0) return; // global scope, no need to mark initialized.
	compiler->locals[compiler->local_count - 1].depth = compiler->scope_depth;
}

// TODO: optimize: use hashmap to store local variables.[hashmap? or array? or linked list? or other?]
static int resolve_local(Parser* self, Compiler* compiler, Token* name) {
	for (int i = compiler->local_count - 1; i >= 0; i--) {
		LocalVariable* local = &compiler->locals[i];
		if (identifiers_equal(local->name, name)) {
			if (local->depth == -1) {
				parse_error_at_current(self, "[Parser::resolve_local] Local variable with this name is not initialized in this scope.");
			}
			return i;	// found: return index
		}
	}
	return -1;	// not found
}

static void begin_scope(Compiler* compiler) {
	compile_begin_scope(compiler);
}

static void end_scope(Parser* self, Compiler* compiler, Chunk* chunk) {
	compile_end_scope(compiler);

	// free local variables
	while (compiler->local_count > 0 &&
		compiler->locals[compiler->local_count - 1].depth > compiler->scope_depth) {
		emit_byte(self, chunk, op_pop);   // TODO: op_popn?
		compiler->local_count--;
	}
}


static void current_into_sub_compiler(VirtualMachine* vm, Compiler* sub, FnType type, Parser* parser) {
	// set sub compiler
	init_compiler(sub, type);
	sub->enclosing = vm->compiler;
	// set vm compiler to sub compiler
	vm->compiler = sub;

	if (type != type_script) {
		// set function name
		Token* name = &parser->previous->token;
		sub->function->name = new_string(&vm->strings, name->start, name->length);
	}

}

static void current_from_sub_compiler(VirtualMachine* vm) {
/*
	if (vm->compiler->enclosing == NULL) {
		panic("Compiler: [Parser::current_from_sub_compiler] Compiler stack underflow");
	}
	vm -> main->compiler					--- ↑  => NULL
	|		|								|
	|		|   sub_compiler            --- ↑
	|			|						|
	|			|   sub_compiler	--- ↑

*/
	vm->compiler = vm->compiler->enclosing;
}


static void synchronize(Parser* self) {
	self->panic_mode = false;

	while (!parse_is_at_end(self)) {
		if (self->previous->token.type == token_semicolon) return;

		switch (self->current->token.type)
		{
		case token_class:
		case token_fn:
		case token_var:
		case token_for:
		case token_if:
		case token_while:
		case token_print:
		case token_return: return;
		default: break;
		}
		parse_advance(self);
	}
}

Fn* parse_tokens(Parser* self, VirtualMachine* vm) {
	while (!parse_is_at_end(self)) {
		parse_declaration(self, vm);
	}
	parse_consume(self, token_eof, "Expected end of expression.");
	return end_compiler(self, vm);
}

static void parse_declaration(Parser* self, VirtualMachine* vm) {
	if (parse_match(self, token_fn)) {
		parse_fn_declaration(self, vm);
	}
	else if (parse_match(self, token_var)) {
		parse_var_declaraton(self, vm);
	}
	else {
		parse_statement(self, vm);
	}
	if (self->panic_mode) synchronize(self);
}

static void parse_fn_declaration(Parser* self, VirtualMachine* vm) {
	index_t func_index = parse_variable(self, vm, "[Parser::parse_fn_declaration] Expected function name.");
	mark_initialized(vm->compiler);

	parse_function(self, vm, type_fn);
	define_variable(self, vm->compiler, current_chunk(vm->compiler), func_index);
}

static void parse_function(Parser* self, VirtualMachine* vm, FnType type) {
	Compiler func_compiler;
	current_into_sub_compiler(vm, &func_compiler, type, self);
	begin_scope(vm->compiler);	// this compiler is a sub-compiler(func_compiler), so it has its own scope. 

	parse_consume(self, token_left_paren, "[Parser::parse_function] Expected '(' after function name.");
	// parameter list
	if (!parse_check(self, token_right_paren)) {
		do {
			vm->compiler->function->arity++;
			if (vm->compiler->function->arity > argument_count_max) {
				parse_error_at_current(self, "[Parser::parse_function] Expected function parameter count less than 256, Found function parameter count greater than 256.");
				return; // error?
			}
			uint8_t var_index = parse_variable(self, vm, "[Parser::parse_function] Expected parameter name.");
			define_variable(self, vm->compiler, current_chunk(vm->compiler), var_index);
		} while (parse_match(self, token_comma));
	}
	parse_consume(self, token_right_paren, "[Parser::parse_function] Expected ')' after function parameters.");
	parse_consume(self, token_left_brace, "[Parser::parse_function] Expected '{' before function body.");
	
	parse_block_statement(self, vm);
	
	// get func object && emit define instruction
	Fn* fn = end_compiler(self, vm);
	emit_bytes(
		self, 
		current_chunk(vm->compiler), 
		op_constant, 
		make_constent(self, current_chunk(vm->compiler), macro_obj_from_val(fn))
	);
}

/*
* parse variable declaration:
*	- parse_variable(identifier -> constant table && return index): parse variable name, add to constants table, return index.
*   - parse_expression(value -> constant table): parse expression after equal sign, if is not exist, emit null.
*   - define_variable(variable type -> constant table): emit define_global instruction to define variable.
*/
static void parse_var_declaraton(Parser* self, VirtualMachine* vm) {
	uint8_t index = parse_variable(self, vm, "[Parser::parse_var_declaraton] Expected variable name.");

	if (parse_match(self, token_equal)) {
		parse_expression(self, vm);				// variable value
	}
	else {
		emit_byte(self, current_chunk(vm->compiler), op_null);	// variable value default is null
	}

	parse_consume(self, token_semicolon, "[Parser::parse_var_declaraton] Expected ';' after variable declaration.");
	
	define_variable(self, vm->compiler, current_chunk(vm->compiler), index);
}

static index_t parse_variable(Parser* self, VirtualMachine* vm, const char* message) {
	parse_consume(self, token_identifier, message);

	declare_variable(self, vm->compiler, &self->previous->token);
	if (vm->compiler->scope_depth > 0) return 0;
	
	return identifier_constant(self, vm, &self->previous->token);
}

void parse_identifier(Parser* self, VirtualMachine* vm, bool can_assign) {
	parse_named_variable(self, vm, &self->previous->token, can_assign);
}

static void parse_named_variable(Parser* self, VirtualMachine* vm, Token* var_name, bool can_assign) {
	
	index_t index = resolve_local(self, vm->compiler, var_name);	
	uint8_t get_op, set_op;

	if (index != -1) {
		get_op = op_get_local;
		set_op = op_set_local;
	}
	else {
		index = identifier_constant(self, vm, var_name);
		get_op = op_get_global;
		set_op = op_set_global;
	}

	if (can_assign && parse_match(self, token_equal)) {
		parse_expression(self, vm);
		emit_bytes(self, current_chunk(vm->compiler), set_op, (uint8_t)index);
	}
	else {
		emit_bytes(self, current_chunk(vm->compiler), get_op, (uint8_t)index);
	}
}

/* Statement: 
*   - printStmt: print expression;
*   - exprStmt: expression;
*   - blockStmt: { Declaration* }
*/
static void parse_statement(Parser* self, VirtualMachine* vm) {
	if (parse_match(self, token_print)) {
		parse_print_stmtement(self, vm);
	}
	else if (parse_match(self, token_for)) {
		parse_for_statement(self, vm);
	}
	else if (parse_match(self, token_if)) {
		parse_if_statement(self, vm);
	}
	else if (parse_match(self, token_return)) {
		parse_return_statement(self, vm);
	}
	else if (parse_match(self, token_while)) {
		parse_while_statement(self, vm);
	}
	else if (parse_match(self, token_left_brace)) {
		begin_scope(vm->compiler);
		parse_block_statement(self, vm);
		end_scope(self, vm->compiler, current_chunk(vm->compiler));
	}
	else {
		parse_expr_statement(self, vm);
	}
}


// returnStmt: "return" expression? ";"
static void parse_return_statement(Parser* self, VirtualMachine* vm) {
	// check return keywold, don't use in top-level code.
	if (vm->compiler->function_type== type_script) {
		parse_error_at_current(self, "[Parser::parse_return_statement] Expected return in function, Found return in top-level code.");
		return;
	}
	if (!parse_match(self, token_semicolon)) {
		parse_expression(self, vm);
		parse_consume(self, token_semicolon, "[Parser::parse_return_statement] Expected ';' after return value.");
	}
	emit_return(self, current_chunk(vm->compiler));
}


// forStmt: for (varDecl | exprStmt | ; condition ; increment) statement;
/*
*	for
*    |     (
*    |     varDecl | exprStmt
*    |     ;
*    |     condition           <- loop_start (exit_jump)            <-           <-
*    |     ;					|                                    |            |
*    |     increment            |							<- increment_start    |
*    |     )					|							 ↑                    |
*    |     statement;			|— body_jump --------------> -                    |
*	 |     			            |- exit_jump -----------------------------------> -
* 
*/
static void parse_for_statement(Parser* self, VirtualMachine* vm) {
	begin_scope(vm->compiler);

	parse_consume(self, token_left_paren, "[Parser::parse_for_statement] Expected '(' after 'for'.");
	if (parse_match(self, token_semicolon)) {
		// no initialization.
	}
	else if (parse_match(self, token_var)) {
		parse_var_declaraton(self, vm);
	}
	else {
		parse_expr_statement(self, vm);
	}

	// condition: expression
	int loop_start = current_chunk(vm->compiler)->count;
	int exit_jump = -1;
	if (!parse_match(self, token_semicolon)) {
		parse_expression(self, vm);
		parse_consume(self, token_semicolon, "[Parser::parse_for_statement] Expected ';' after loop condition.");

		// jump to the end of the loop if the condition is false.
		exit_jump = emit_jump(self, current_chunk(vm->compiler), op_jump_if_false);
		emit_byte(self, current_chunk(vm->compiler), op_pop); // condition
	}

	// increment: expression; unconditional jump that hops over the increment clause’s code to the body of the loop. 
	if (!parse_match(self, token_right_paren)) {
		int body_jump = emit_jump(self, current_chunk(vm->compiler), op_jump);
		int increment_start = current_chunk(vm->compiler)->count; // label for the increment clause.

		parse_expression(self, vm);
		emit_byte(self, current_chunk(vm->compiler), op_pop); // increment
		parse_consume(self, token_right_paren, "[Parser::parse_for_statement] Expected ')' after for clauses.");
		
		emit_loop(self, current_chunk(vm->compiler), loop_start);  
		loop_start = increment_start;
		patch_jump(self, current_chunk(vm->compiler), body_jump);
	}

	// statement: statement;
	parse_statement(self, vm);

	// emit loop op_loop
	emit_loop(self, current_chunk(vm->compiler), loop_start);
	// patch exit_jump to here: update loop chunk [opcode, jump_offset, jump_offset]
	if (exit_jump != -1) {
		patch_jump(self, current_chunk(vm->compiler), exit_jump);
		emit_byte(self, current_chunk(vm->compiler), op_pop); // condition
	}

	end_scope(self, vm->compiler, current_chunk(vm->compiler));
}

// whileStmt: while (expression) statement;
/*
*	while
*    |     <- loop_start  <-——--- loop_start
*    |     (				↑
*    |     condition		|
*    |     )				|
*    |     <- exit_jump		|
*    |     {				|
*    |         statement	|
*    |     }				|
*    |     <--- op_loop --->|
*    |        | loop_offset |
*    |        V loop_offset | <- chunck->count
*	=> offset = chunk->count - loop_start + 2(loop_offset)
*/
static void parse_while_statement(Parser* self, VirtualMachine* vm) {
	int loop_start = current_chunk(vm->compiler)->count;

	parse_consume(self, token_left_paren, "[Parser::parse_while_statement] Expected '(' after 'while'.");
	parse_expression(self, vm);
	parse_consume(self, token_right_paren, "[Parser::parse_while_statement] Expected ')' after condition.");

	int exit_jump = emit_jump(self, current_chunk(vm->compiler), op_jump_if_false);
	emit_byte(self, current_chunk(vm->compiler), op_pop);
	parse_statement(self, vm);
	emit_loop(self, current_chunk(vm->compiler), loop_start);

	patch_jump(self, current_chunk(vm->compiler), exit_jump);
	emit_byte(self, current_chunk(vm->compiler), op_pop);
}

// ifStmt: if (expression) statement else statement;
static void parse_if_statement(Parser* self, VirtualMachine* vm) {
	parse_consume(self, token_left_paren, "[Parser::parse_if_statement] Expected '(' after 'if'.");
	parse_expression(self, vm);
	parse_consume(self, token_right_paren, "[Parser::parse_if_statement] Expected ')' after condition.");

	// backpatching later: chunk [opcode, tempslot, tempslot]
	int then_jump = emit_jump(self, current_chunk(vm->compiler), op_jump_if_false);
	emit_byte(self, current_chunk(vm->compiler), op_pop);
	parse_statement(self, vm);
	
	// backpatching later: chunk [opcode, tempslot, tempslot]
	int else_jump = emit_jump(self, current_chunk(vm->compiler), op_jump);  
	// patch then_jump to here: update then chunk [opcode, jump_offset, jump_offset]
	patch_jump(self, current_chunk(vm->compiler), then_jump);
	
	emit_byte(self, current_chunk(vm->compiler), op_pop);
	if (parse_match(self, token_else)) parse_statement(self, vm);

	// patch else_jump to here: update else chunk [opcode, jump_offset, jump_offset]
	patch_jump(self, current_chunk(vm->compiler), else_jump);
}

static void parse_block_statement(Parser* self, VirtualMachine* vm) {
	while (!parse_check(self, token_right_brace) && !parse_check(self, token_eof)) {
		parse_declaration(self, vm);
	}
	parse_consume(self, token_right_brace, "[Parser::parse_block_statement] Expected '}' after block statement.");
}

static void parse_print_stmtement(Parser* self, VirtualMachine* vm) {
	parse_expression(self, vm);
	parse_consume(self, token_semicolon, "[Parser::parse_print_stmtement] Expected ';' after expression.");
	emit_byte(self, current_chunk(vm->compiler), op_print);
}

static void parse_expr_statement(Parser* self, VirtualMachine* vm) {
	parse_expression(self, vm);
	parse_consume(self, token_semicolon, "[Parser::parse_expr_statement] Expected ';' after expression.");
	emit_byte(self, current_chunk(vm->compiler), op_pop);
}

static void parse_expression(Parser* self, VirtualMachine* vm) {
	parse_precedence(self, vm, prec_assignment);
}


static void parse_precedence(Parser* self, VirtualMachine* vm, Precedence outer_precedence) {
	/* 
	* execute advance() after: 
	*	'Scanner->current' will move to 'Scanner->previous'
	*   Scanner:
	*       -----(move) ----->
	*	type: curr         next
	*		|            |
	*		V		     V
	*		previous(1), current(2), next(3), ...
	*/
	if (parse_advance(self) == NULL) {
		parse_error_at_previous(self, "[Parser::parse_precedence] Expected expression, Found end of file.");
		return;
	}

	/* get current handle token's prefix rule */
	ParseFnPtr previous_prefix_rule = get_rule(self->previous->token.type)->prefix;
	if (previous_prefix_rule == NULL) {
		parse_error_at_previous(self, "[Parser::parse_precedence] Expected prefix rule for this token, Found NULL.");
		return;
	}

	/* dispatch to prefix rule */
	bool can_assign = outer_precedence <= prec_assignment;
	previous_prefix_rule(self, vm, can_assign);

	/* get current next token's infix rule:
	*    1. if next token's precedence is greater than current token's precedence,
	*       then execute infix rule of next token.
	*    2. if next token's precedence is less than or equal to current token's precedence,
	*       then execute infix rule of current token.
	* 
	* precedence:
	*     1. parameter: outsiders' precedence.
	*/

	/* check if there is a next token, if is not the end of file */
	if (parse_is_at_end(self)) return;

	while (outer_precedence <= get_rule(self->current->token.type)->precedence) {
		/*
		*   Scanner:
		*   -------------------- (move) -------------------->
		*   type: prev	              curr         next
		*	    |			          |            |
		*	    V			          V		       V
		*	    previous_previous(1), previous(2), current(3), next(4), ...
		*/
		if (parse_advance(self) == NULL) {
			parse_error_at_previous(self, "[Parser::parse_precedence] Expected expression, Found end of file.");
			return;
		}

		/* get current handle token's infix rule */
		ParseFnPtr previous_infix_rule = get_rule(self->previous->token.type)->infix;
		previous_infix_rule(self, vm, can_assign);
	}

	/* check if is assignment operator, if is, then error */
	if (can_assign && parse_match(self, token_equal)) {
		parse_error_at_current(self, "[Parser::parse_precedence] Expected expression, Found assignment operator.");
	}
}


/* parse number: {i32, f64,...}, need detailed dispatching */
/* TODO: add more types */
void parse_i32(Parser* self, VirtualMachine* vm, bool _can_assign) {
	int32_t value = atoi(self->previous->token.start);
	emit_constant(self, current_chunk(vm->compiler), macro_i32_from_val(value));
}
void parse_i64(Parser* self, VirtualMachine* vm, bool _can_assign) {
	int64_t value = atoll(self->previous->token.start);
	emit_constant(self, current_chunk(vm->compiler), macro_i64_from_val(value));
}
void parse_f32(Parser* self, VirtualMachine* vm, bool _can_assign) {
    float value = strtof(self->previous->token.start, NULL);
	emit_constant(self, current_chunk(vm->compiler), macro_f32_from_val(value));
}
void parse_f64(Parser* self, VirtualMachine* vm, bool _can_assign) {
    double value = strtod(self->previous->token.start, NULL);
	emit_constant(self, current_chunk(vm->compiler), macro_f64_from_val(value));
}


void parse_grouping(Parser* self, VirtualMachine* vm, bool can_assign) {
	parse_expression(self, vm);
	parse_consume(self, token_right_paren, "[Parser::parse_grouping] Expected ')' after expression.");
}

static uint8_t parse_argument_list(Parser* self, VirtualMachine* vm) {
	uint8_t arg_count = 0;
	if (!parse_check(self, token_right_paren)) {
		do {
			parse_expression(self, vm);
			if (arg_count == argument_count_max) {
				parse_error_at_current(self, "[Parser::parse_argument_list] Too many arguments, limit is 255.");
			}
			arg_count++;
		} while (parse_match(self, token_comma));
	}
	parse_consume(self, token_right_paren, "[Parser::parse_argument_list] Expected ')' after arguments.");
	return arg_count;
}

/*
* parse function call:
* {..., [op_call, arg_count], ... } 
*/
void parse_call(Parser* self, VirtualMachine* vm, bool can_assign) {
	uint8_t arg_count = parse_argument_list(self, vm);
	emit_bytes(self, current_chunk(vm->compiler), op_call, arg_count);
}

/* pervious suffix expression: -a.b + c; => parse_precedence ; 嵌套一元表达式 */
void parse_unary(Parser* self, VirtualMachine* vm, bool can_assign) {
	TokenType operator_type = self->previous->token.type;

	// compile the operand
	parse_precedence(self, vm, prec_unary);

	// emit the operator instruction
	switch (operator_type) {
	case token_bang: emit_byte(self, current_chunk(vm->compiler), op_not); break;
	case token_minus: emit_byte(self, current_chunk(vm->compiler), op_negate); break;
	default:
		return; // Unreachable.
	}
}


/* middle suffix expression: a + b */
void parse_binary(Parser* self, VirtualMachine* vm, bool can_assign) {
	TokenType operator_type = self->previous->token.type;
	ParseRule* rule = get_rule(operator_type);

	// compile the right operand
	parse_precedence(self, vm, (Precedence)(rule->precedence + 1));

	// emit the operator instruction
	switch (operator_type) {
	case token_bang_equal:		emit_byte(self, current_chunk(vm->compiler), op_not_equal); break;
	case token_equal_equal:		emit_byte(self, current_chunk(vm->compiler), op_equal); break;
	case token_greater:			emit_byte(self, current_chunk(vm->compiler), op_greater); break;
	case token_greater_equal:	emit_byte(self, current_chunk(vm->compiler), op_greater_equal); break;
	case token_less:			emit_byte(self, current_chunk(vm->compiler), op_less); break;
	case token_less_equal:		emit_byte(self, current_chunk(vm->compiler), op_less_equal); break;
	case token_plus:			emit_byte(self, current_chunk(vm->compiler), op_add); break;
	case token_minus:			emit_byte(self, current_chunk(vm->compiler), op_subtract); break;
	case token_star:			emit_byte(self, current_chunk(vm->compiler), op_multiply); break;
	case token_slash:			emit_byte(self, current_chunk(vm->compiler), op_divide); break;
	default:
		return; // Unreachable.
	}
}

void parse_literal(Parser* self, VirtualMachine* vm, bool _can_assign) {
	switch (self->previous->token.type) {
	case token_false: emit_byte(self, current_chunk(vm->compiler), op_false); break;
	case token_true:  emit_byte(self, current_chunk(vm->compiler), op_true);  break;
	case token_null:  emit_byte(self, current_chunk(vm->compiler), op_null);  break;
	default:
		return; // Unreachable.
	}
}
/* parse string literal */
void parse_string(Parser* self, VirtualMachine* vm, bool _can_assign) {
	emit_constant(self, current_chunk(vm->compiler), macro_obj_from_val(new_string(
		&vm->strings,
		self->previous->token.start + 1,
		self->previous->token.length - 2
	)));
}

void parse_and(Parser* self, VirtualMachine* vm, bool _can_assign) {
	int end_jump = emit_jump(self, current_chunk(vm->compiler), op_jump_if_false);
	emit_byte(self, current_chunk(vm->compiler), op_pop);
	parse_precedence(self, vm, prec_and);
	patch_jump(self, current_chunk(vm->compiler), end_jump);
}

void parse_or(Parser* self, VirtualMachine* vm, bool _can_assign) {
	int else_jump = emit_jump(self, current_chunk(vm->compiler), op_jump_if_false);
	int end_jump = emit_jump(self, current_chunk(vm->compiler), op_jump);
	patch_jump(self, current_chunk(vm->compiler), else_jump);
	emit_byte(self, current_chunk(vm->compiler), op_pop);
	parse_precedence(self, vm, prec_or);
	patch_jump(self, current_chunk(vm->compiler), end_jump);
}


void parse_error_at_current(Parser* self, const char* message) {
	if (self->current == NULL) {
		parse_error_at_previous(self, "[Parser::parse_error_at_current] Expected expression, Found end of file.");
		return;
	}
	parse_error_at(self, &self->current->token, message);
}

void parse_error_at_previous(Parser* self, const char* message) {
	if (self->previous == NULL) {
		parse_error_at(self, &self->tokens->head->token, "[Parser::parse_error_at_previous] Expected expression, Found end of file.");
		return;
	}
	parse_error_at(self, &self->previous->token, message);
}


/*
* PS:
*	[line 1] where: {'at end'" | at 'lexeme'} .
*		msg: 'parse error message'.
* 
*/
static void parse_error_at(Parser* self, Token* error, const char* message) {
	/* if had error, then return */
	if (self->panic_mode) return;
	/* set panic mode */
	self->panic_mode = true;

	fprintf(stderr, "[line %d] ", error->line);
	switch (error->type)
	{
	case token_eof:
		fprintf(stderr, "where: at end.\n");
		break;
	case token_error:
		fprintf(stderr, "where: at error.\n");
		break;
	default:
		fprintf(stderr, "where: at '%.*s'.\n", error->length, error->start);
		break;
	}
	fprintf(stderr, "\tmsg: %s\n", message);

    // TODO: hander error
	self->had_error = true;
}
