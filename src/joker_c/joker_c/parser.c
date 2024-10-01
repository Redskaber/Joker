#include <stdio.h>
#include <stdlib.h>

#include "parser.h"




static void parse_error_at(Parser* parser, Token* error, const char* message);
void parse_error_at_current(Parser* parser, const char* message);
void parse_error_at_previous(Parser* parser, const char* message);



/* get handle token's rule(prefix, infix, ...)[function pointer] */
ParseRule* get_rule(TokenType type) {
	return &static_syntax_rules[type];
}


void init_parser(Parser* parser, TokenList* tokens) {
	/* initialize parser with tokens, reset current and previous token pointer */
	reset_foreach_pointer(tokens);
	parser->tokens = tokens;
	parser->current = tokens->head;
	parser->previous = NULL;
	parser->had_error = false;
	parser->panic_mode = false;
}

void free_parser(Parser* parser) {
	// TODO: free(parser);  // error: Parser is not malloc'ed memory, used loacal variable instead.
	parser -> tokens = NULL;
	parser -> current = NULL;
	parser -> previous = NULL;
	parser -> had_error = false;
	parser -> panic_mode = false;
}

void print_parser(Parser* parser) {
	printf("Parser:\n");
	printf("tokens: ");print_tokens(parser->tokens);
	printf("current: ");print_token_node(parser->current);printf("\n");
	printf("previous: ");print_token_node(parser->previous);printf("\n");
	printf("had_error: %d\n", parser->had_error);
	printf("panic_mode: %d\n", parser->panic_mode);
}

/* check if is at end of file, return true if is at end of file, false otherwise */
bool static parse_is_at_end(Parser* parser) {
	if (parser->current == NULL) {
		parse_error_at_previous(parser, "[Parser::parse_is_at_end] Expected match end of file(eof token), Found Unexpected end of file(not a token).");
		return true;
	}
	return parser->current->token.type == token_eof;
}

/* advance to next token, return previous token, if is at end of file, return NULL */
TokenNode* advance(Parser* parser) {
	if (parse_is_at_end(parser)) return NULL;
	parser->previous = parser->current;
	parser->current = parser->current->next;
	return parser->previous;
}

void parse_consume(Parser* parser, TokenType type, const char* message) {
	if (parser->current->token.type == type) {
		advance(parser);
		return;
	}
	parse_error_at_current(parser, message);
}


void emit_byte(Parser* parser, Chunk* chunk, uint8_t byte) {
	write_chunk(chunk, byte, parser->previous->token.line);
}


void emit_constant(Parser* parser, Chunk* chunk, Value value) {
	write_constant(chunk, value, parser->previous->token.line);
}


void emit_bytes(Parser* parser, Chunk* chunk, uint8_t opcode, uint8_t operand) {
	emit_byte(parser, chunk, opcode);
	emit_byte(parser, chunk, operand);
}


static void parse_precedence(Parser* parser, Chunk* chunk, Precedence outer_precedence) {
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
	if (advance(parser) == NULL) {
		parse_error_at_previous(parser, "[Parser::parse_precedence] Expected expression, Found end of file.");
		return;
	}

	/* get current handle token's prefix rule */
	ParseFnPtr previous_prefix_rule = get_rule(parser->previous->token.type)->prefix;
	if (previous_prefix_rule == NULL) {
		parse_error_at_previous(parser, "[Parser::parse_precedence] Expected prefix rule for this token, Found NULL.");
		return;
	}

	/* dispatch to prefix rule */
	previous_prefix_rule(parser, chunk);

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
	if (parse_is_at_end(parser)) return;

	while (outer_precedence <= get_rule(parser->current->token.type)->precedence) {
		/*
		*   Scanner:
		*   -------------------- (move) -------------------->
		*   type: prev	              curr         next
		*	    |			          |            |
		*	    V			          V		       V
		*	    previous_previous(1), previous(2), current(3), next(4), ...
		*/
		if (advance(parser) == NULL) {
			parse_error_at_previous(parser, "[Parser::parse_precedence] Expected expression, Found end of file.");
			return;
		}

		/* get current handle token's infix rule */
		ParseFnPtr infix_rule = get_rule(parser->previous->token.type)->infix;
		infix_rule(parser, chunk);
	}
}


void parse_expression(Parser* parser, Chunk* chunk) {
	parse_precedence(parser, chunk, prec_assignment);
}


/* parse number: {i32, f64,...}, need detailed dispatching */
/* TODO: add more types */
void parse_i32(Parser* parser, Chunk* chunk) {
	// atoi() converts a string to an integer
	Value value = atoi(parser->previous->token.start, NULL);
	emit_constant(parser, chunk, value);
}
void parse_f64(Parser* parser, Chunk* chunk) {
	// strtod() converts a string to a double
	Value value = strtod(parser->previous->token.start, NULL);
	emit_constant(parser, chunk, value);
}


void parse_grouping(Parser* parser, Chunk* chunk) {
	parse_expression(parser, chunk);
	parse_consume(parser, token_right_paren, "[Parser::parse_grouping] Expected ')' after expression.");
}


/* pervious suffix expression: -a.b + c; => parse_precedence ; 嵌套一元表达式 */
void parse_unary(Parser* parser, Chunk* chunk) {
	TokenType operator_type = parser->previous->token.type;

	// compile the operand
	parse_precedence(parser, chunk, prec_unary);

	// emit the operator instruction
	switch (operator_type) {
	case token_minus: emit_byte(parser, chunk, op_negate); break;
	default:
		return; // Unreachable.
	}
}


/* middle suffix expression: a + b */
void parse_binary(Parser* parser, Chunk* chunk) {
	TokenType operator_type = parser->previous->token.type;
	ParseRule* rule = get_rule(operator_type);

	// compile the right operand
	parse_precedence(parser, chunk, (Precedence)(rule->precedence + 1));

	// emit the operator instruction
	switch (operator_type) {
	case token_plus:  emit_byte(parser, chunk, op_add);			break;
	case token_minus: emit_byte(parser, chunk, op_subtract);	break;
	case token_star:  emit_byte(parser, chunk, op_multiply);	break;
	case token_slash: emit_byte(parser, chunk, op_divide);		break;
	default:
		return; // Unreachable.
	}
}



void parse_error_at_current(Parser* parser, const char* message) {
	if (parser->current == NULL) {
		parse_error_at_previous(parser, "[Parser::parse_error_at_current] Expected expression, Found end of file.");
		return;
	}
	parse_error_at(parser, &parser->current->token, message);
}

void parse_error_at_previous(Parser* parser, const char* message) {
	if (parser->previous == NULL) {
		parse_error_at(parser, &parser->tokens->head->token, "[Parser::parse_error_at_previous] Expected expression, Found end of file.");
		return;
	}
	parse_error_at(parser, &parser->previous->token, message);
}


/*
* PS:
*	[line 1] where: {'at end'" | at 'lexeme'} .
*		msg: 'parse error message'.
* 
*/
static void parse_error_at(Parser* parser, Token* error, const char* message) {
	/* if had error, then return */
	if (parser->panic_mode) return;
	/* set panic mode */
	parser->panic_mode = true;

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
	parser->had_error = true;
}
