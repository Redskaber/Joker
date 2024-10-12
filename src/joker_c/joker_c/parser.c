/*
* Abstract Syntax Tree(AST):
*	An abstract syntax tree (AST) is a tree-like representation of the source code.
*	It is a way to represent the code in a way that is easy to manipulate and process.
*
*   Declaration    -> classDecl
*					| funcDecl
*					| varDecl -> parse_var -> ident_const -> make_const
*                   | Statement
*
*   Statement      -> exprStmt -> expr -> precedence -> dispath { }
*					| forStmt
*					| ifStmt
* 					| printStmt
*					| returnStmt
*					| whileStmt
*				    | blockStmt		->  "{" Declaration* "}"
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
*/




#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "hashmap.h"
#include "string.h"
#include "compiler.h"

#include "parser.h"




static void parse_error_at(Parser* self, Token* error, const char* message);
void parse_error_at_current(Parser* self, const char* message);
void parse_error_at_previous(Parser* self, const char* message);
static TokenNode* parse_advance(Parser* self);
static bool parse_check(Parser* self, TokenType type);
static bool parse_match(Parser* self, TokenType type);
static bool parse_is_at_end(Parser* self);
static void parse_consume(Parser* self, TokenType type, const char* message);


/* Abstract Syntax Tree(AST)*/
static void parse_declaration(Parser* self, VirtualMachine* vm);
static void parse_var_declaraton(Parser* self, VirtualMachine* vm);
static void parse_named_variable(Parser* self, VirtualMachine* vm, Token* variable, bool can_assign);
static index_t parse_variable(Parser* self, VirtualMachine* vm, const char* message);
static void parse_statement(Parser* self, VirtualMachine* vm);
static void parse_block_statement(Parser* self, VirtualMachine* vm);
static void parse_print_stmtement(Parser* self, VirtualMachine* vm);
static void parse_expr_statement(Parser* self, VirtualMachine* vm);
static void parse_precedence(Parser* self, VirtualMachine* vm, Precedence outer_precedence);
static void parse_expression(Parser* self, VirtualMachine* vm);
static index_t identifier_constant(Parser* self, VirtualMachine* vm, Token* token);


/* Compiler */
static void begin_scope(Compiler* compiler);
static void end_scope(Parser* self, Compiler* compiler, Chunk* chunk);
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


void emit_byte(Parser* self, Chunk* chunk, uint8_t byte) {
	write_chunk(chunk, byte, self->previous->token.line);
}

void emit_constant(Parser* self, Chunk* chunk, Value value) {
	write_constant(chunk, value, self->previous->token.line);
}

void emit_bytes(Parser* self, Chunk* chunk, uint8_t opcode, uint8_t operand) {
	emit_byte(self, chunk, opcode);
	emit_byte(self, chunk, operand);
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
	return make_constent(self, vm->chunk, macro_obj_from_val(new_string(&vm->strings, token->start, token->length)));
}

static void add_local(Parser* self, Compiler* compiler, Token* name) {
	if (compiler->local_count == max_local_variable_count) {
		parse_error_at_current(self, "[Parser::add_local] Expected local variable count less than 256, Found local variable count greater than 256.");
		return;
	}
	LocalVariable* local = &compiler->locals[compiler->local_count++];
	local->name = name;
	local->depth = -1;		// default depth is -1, means not defined.
}

static void mark_initialized(Compiler* compiler) {
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

void parse_tokens(Parser* self, VirtualMachine* vm) {
	while (!parse_is_at_end(self)) {
		parse_declaration(self, vm);
	}
	parse_consume(self, token_eof, "Expected end of expression.");
}

static void parse_declaration(Parser* self, VirtualMachine* vm) {
	if (parse_match(self, token_var)) {
		parse_var_declaraton(self, vm);
	}
	else {
		parse_statement(self, vm);
	}
	if (self->panic_mode) synchronize(self);
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
		emit_byte(self, vm->chunk, op_null);	// variable value default is null
	}

	parse_consume(self, token_semicolon, "[Parser::parse_var_declaraton] Expected ';' after variable declaration.");
	
	define_variable(self, vm->compiler, vm->chunk, index);
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
		emit_bytes(self, vm->chunk, set_op, (uint8_t)index);
	}
	else {
		emit_bytes(self, vm->chunk, get_op, (uint8_t)index);
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
	else if (parse_match(self, token_left_brace)) {
		begin_scope(vm->compiler);
		parse_block_statement(self, vm);
		end_scope(self, vm->compiler, vm->chunk);
	}
	else {
		parse_expr_statement(self, vm);
	}
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
	emit_byte(self, vm->chunk, op_print);
}

static void parse_expr_statement(Parser* self, VirtualMachine* vm) {
	parse_expression(self, vm);
	parse_consume(self, token_semicolon, "[Parser::parse_expr_statement] Expected ';' after expression.");
	emit_byte(self, vm->chunk, op_pop);
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
	// atoi() converts a string to an integer
	int value = atoi(self->previous->token.start);
	emit_constant(self, vm->chunk, macro_i32_from_val(value));
}
void parse_f64(Parser* self, VirtualMachine* vm, bool _can_assign) {
	// strtod() converts a string to a double
    double value = strtod(self->previous->token.start, NULL);
	emit_constant(self, vm->chunk, macro_f64_from_val(value));
}


void parse_grouping(Parser* self, VirtualMachine* vm, bool can_assign) {
	parse_expression(self, vm);
	parse_consume(self, token_right_paren, "[Parser::parse_grouping] Expected ')' after expression.");
}


/* pervious suffix expression: -a.b + c; => parse_precedence ; 嵌套一元表达式 */
void parse_unary(Parser* self, VirtualMachine* vm, bool can_assign) {
	TokenType operator_type = self->previous->token.type;

	// compile the operand
	parse_precedence(self, vm, prec_unary);

	// emit the operator instruction
	switch (operator_type) {
	case token_bang: emit_byte(self, vm->chunk, op_not); break;
	case token_minus: emit_byte(self, vm->chunk, op_negate); break;
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
	case token_bang_equal:		emit_byte(self, vm->chunk, op_not_equal); break;
	case token_equal_equal:		emit_byte(self, vm->chunk, op_equal); break;
	case token_greater:			emit_byte(self, vm->chunk, op_greater); break;
	case token_greater_equal:	emit_byte(self, vm->chunk, op_greater_equal); break;
	case token_less:			emit_byte(self, vm->chunk, op_less); break;
	case token_less_equal:		emit_byte(self, vm->chunk, op_less_equal); break;
	case token_plus:			emit_byte(self, vm->chunk, op_add); break;
	case token_minus:			emit_byte(self, vm->chunk, op_subtract); break;
	case token_star:			emit_byte(self, vm->chunk, op_multiply); break;
	case token_slash:			emit_byte(self, vm->chunk, op_divide); break;
	default:
		return; // Unreachable.
	}
}

void parse_literal(Parser* self, VirtualMachine* vm, bool _can_assign) {
	switch (self->previous->token.type) {
	case token_false: emit_byte(self, vm->chunk, op_false); break;
	case token_true:  emit_byte(self, vm->chunk, op_true);  break;
	case token_null:  emit_byte(self, vm->chunk, op_null);  break;
	default:
		return; // Unreachable.
	}
}
/* parse string literal */
void parse_string(Parser* self, VirtualMachine* vm, bool _can_assign) {
	emit_constant(self, vm->chunk, macro_obj_from_val(new_string(
		&vm->strings,
		self->previous->token.start + 1,
		self->previous->token.length - 2
	)));
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
