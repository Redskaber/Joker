#pragma once

#ifndef joker_parser_h
#define joker_parser_h
#include "token.h"
#include "chunk.h"
#include "object.h"


typedef struct {
	TokenList* tokens;
	TokenNode* current;
	TokenNode* previous;
	bool had_error;
	bool panic_mode;
} Parser;

void init_parser(Parser* parser, TokenList* tokens);
void free_parser(Parser* parser);
void print_parser(Parser* parser);
void parse_error_at_current(Parser* parser, const char* message);
void parse_error_at_previous(Parser* parser, const char* message);
TokenNode* advance(Parser* parser);
void parse_consume(Parser* parser, TokenType type, const char* message);
void emit_byte(Parser* parser, Chunk* chunk, uint8_t byte);
void emit_constant(Parser* parser, Chunk* chunk, Value value);
void emit_bytes(Parser* parser, Chunk* chunk, uint8_t opcode, uint8_t operand);


/* Precedence levels : lowest to highest */
typedef enum {
	prec_none,			// No precedence
	prec_assignment,	// =
	prec_or,			// or
	prec_and,			// and
	prec_equality,		// ==!=
	prec_comparison,	// < > <= >=
	prec_term,			// + -
	prec_factor,		// * / %
	prec_unary,			// unary - !
	prec_call,			// ()
	prec_primary		// primary
} Precedence;

typedef void (*ParseFnPtr)(Parser* parser, Chunk* chunk);			// ParseFn -point-> void function()

/* parser table of rules row */
typedef struct {
	ParseFnPtr prefix;
	ParseFnPtr infix;
	Precedence precedence;
} ParseRule;

/* parser function prototypes */
void parse_expression(Parser* parser, Chunk* chunk);
void parse_grouping(Parser* parser, Chunk* chunk);
void parse_i32(Parser* parser, Chunk* chunk);
void parse_f64(Parser* parser, Chunk* chunk);
void parse_unary(Parser* parser, Chunk* chunk);
void parse_binary(Parser* parser, Chunk* chunk);
void parse_literal(Parser* parser, Chunk* chunk);
void parse_string(Parser* parser, Chunk* chunk);


/* parser rules table: Patttern-Action table */
ParseRule static static_syntax_rules[] = {
	/* entry: [index]	= {prefix,			infix,			infix_precedence} */
	[token_left_paren]	= {parse_grouping,	NULL,			prec_none},
	[token_right_paren]	= {NULL,			NULL,			prec_none},
	[token_left_brace]	= {NULL,			NULL,			prec_none},
	[token_right_brace]	= {NULL,			NULL,			prec_none},
	[token_comma]		= {NULL,			NULL,			prec_none},
	[token_dot]			= {NULL,			NULL,			prec_none},
	[token_minus]		= {parse_unary,		parse_binary,	prec_term},
	[token_plus]		= {NULL,			parse_binary,	prec_term},
	[token_semicolon]	= {NULL,			NULL,			prec_none},
	[token_slash]		= {NULL,			parse_binary,	prec_factor},
	[token_star]		= {NULL,			parse_binary,	prec_factor},
	[token_bang]		= {parse_unary,	    NULL,			prec_none},
	[token_equal]		= {NULL,			NULL,			prec_none},
	[token_bang_equal]	= {NULL,			parse_binary,   prec_equality},
	[token_equal_equal]	= {NULL,			parse_binary,   prec_equality},
	[token_greater]		= {NULL,			parse_binary,   prec_comparison},
	[token_greater_equal]= {NULL,			parse_binary,   prec_comparison},
	[token_less]		= {NULL,			parse_binary,   prec_comparison},
	[token_less_equal]	= {NULL,			parse_binary,   prec_comparison},
	[token_identifier]	= {NULL,			NULL,			prec_none},
	[token_string]		= {parse_string,	NULL,			prec_none},
	[token_i32]			= {parse_i32,		NULL,			prec_none},
	[token_f64]			= {parse_f64,		NULL,			prec_none},
	[token_and]			= {NULL,			NULL,			prec_none},
	[token_class]		= {NULL,			NULL,			prec_none},
	[token_else]		= {NULL,			NULL,			prec_none},
	[token_false]		= {parse_literal,   NULL,			prec_none},
	[token_for]			= {NULL,			NULL,			prec_none},
	[token_fn]			= {NULL,			NULL,			prec_none},
	[token_if]			= {NULL,			NULL,			prec_none},
	[token_null]		= {parse_literal,   NULL,			prec_none},
	[token_or]			= {NULL,			NULL,			prec_none},
	[token_print]		= {NULL,			NULL,			prec_none},
	[token_return]		= {NULL,			NULL,			prec_none},
	[token_super]		= {NULL,			NULL,			prec_none},
	[token_self]		= {NULL,			NULL,			prec_none},
	[token_true]		= {parse_literal,   NULL,			prec_none},
	[token_var]			= {NULL,			NULL,			prec_none},
	[token_while]		= {NULL,			NULL,			prec_none},
	[token_error]		= {NULL,			NULL,			prec_none},
	[token_eof]			= {NULL,			NULL,			prec_none}
};

/* get parser rule for token type */
ParseRule* get_rule(TokenType type);


#endif /* joker_parser_h */

