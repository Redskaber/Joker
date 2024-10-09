#include <stdio.h>
#include <stdlib.h>

#include "hashmap.h"
#include "string.h"
#include "parser.h"




static void parse_error_at(Parser* self, Token* error, const char* message);
void parse_error_at_current(Parser* self, const char* message);
void parse_error_at_previous(Parser* self, const char* message);



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
bool static parse_is_at_end(Parser* self) {
	if (self->current == NULL) {
		parse_error_at_previous(self, "[Parser::parse_is_at_end] Expected match end of file(eof token), Found Unexpected end of file(not a token).");
		return true;
	}
	return self->current->token.type == token_eof;
}

/* advance to next token, return previous token, if is at end of file, return NULL */
TokenNode* parse_advance(Parser* self) {
	if (parse_is_at_end(self)) return NULL;
	self->previous = self->current;
	self->current = self->current->next;
	return self->previous;
}

void parse_consume(Parser* self, TokenType type, const char* message) {
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


static void parse_precedence(Parser* self, VirtualMachine* vm, Chunk* chunk, Precedence outer_precedence) {
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
	previous_prefix_rule(self, vm, chunk);

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
		ParseFnPtr infix_rule = get_rule(self->previous->token.type)->infix;
		infix_rule(self, vm, chunk);
	}
}


void parse_expression(Parser* self, VirtualMachine* vm, Chunk* chunk) {
	parse_precedence(self, vm, chunk, prec_assignment);
}


/* parse number: {i32, f64,...}, need detailed dispatching */
/* TODO: add more types */
void parse_i32(Parser* self, VirtualMachine* _vm, Chunk* chunk) {
	// atoi() converts a string to an integer
	int value = atoi(self->previous->token.start);
	emit_constant(self, chunk, macro_i32_from_val(value));
}
void parse_f64(Parser* self, VirtualMachine* _vm, Chunk* chunk) {
	// strtod() converts a string to a double
    double value = strtod(self->previous->token.start, NULL);
	emit_constant(self, chunk, macro_f64_from_val(value));
}


void parse_grouping(Parser* self, VirtualMachine* vm, Chunk* chunk) {
	parse_expression(self, vm, chunk);
	parse_consume(self, token_right_paren, "[Parser::parse_grouping] Expected ')' after expression.");
}


/* pervious suffix expression: -a.b + c; => parse_precedence ; 嵌套一元表达式 */
void parse_unary(Parser* self, VirtualMachine* vm, Chunk* chunk) {
	TokenType operator_type = self->previous->token.type;

	// compile the operand
	parse_precedence(self, vm, chunk, prec_unary);

	// emit the operator instruction
	switch (operator_type) {
	case token_bang: emit_byte(self, chunk, op_not); break;
	case token_minus: emit_byte(self, chunk, op_negate); break;
	default:
		return; // Unreachable.
	}
}


/* middle suffix expression: a + b */
void parse_binary(Parser* self, VirtualMachine* vm, Chunk* chunk) {
	TokenType operator_type = self->previous->token.type;
	ParseRule* rule = get_rule(operator_type);

	// compile the right operand
	parse_precedence(self, vm, chunk, (Precedence)(rule->precedence + 1));

	// emit the operator instruction
	switch (operator_type) {
	case token_bang_equal:		emit_byte(self, chunk, op_not_equal); break;
	case token_equal_equal:		emit_byte(self, chunk, op_equal); break;
	case token_greater:			emit_byte(self, chunk, op_greater); break;
	case token_greater_equal:	emit_byte(self, chunk, op_greater_equal); break;
	case token_less:			emit_byte(self, chunk, op_less); break;
	case token_less_equal:		emit_byte(self, chunk, op_less_equal); break;
	case token_plus:			emit_byte(self, chunk, op_add); break;
	case token_minus:			emit_byte(self, chunk, op_subtract); break;
	case token_star:			emit_byte(self, chunk, op_multiply); break;
	case token_slash:			emit_byte(self, chunk, op_divide); break;
	default:
		return; // Unreachable.
	}
}

void parse_literal(Parser* self, VirtualMachine* _vm, Chunk* chunk) {
	switch (self->previous->token.type) {
	case token_false: emit_byte(self, chunk, op_false); break;
	case token_true:  emit_byte(self, chunk, op_true);  break;
	case token_null:  emit_byte(self, chunk, op_null);  break;
	default:
		return; // Unreachable.
	}
}
/* parse string literal */
void parse_string(Parser* self, VirtualMachine* vm, Chunk* chunk) {
	emit_constant(self, chunk, macro_obj_from_val(new_string(
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
