#pragma once

#ifndef joker_parser_h
#define joker_parser_h
#include "token.h"
#include "vm.h"
#include "chunk.h"
#include "object.h"


typedef struct {
	TokenList* tokens;
	TokenNode* current;
	TokenNode* previous;
	bool had_error;
	bool panic_mode;
} Parser;

void init_parser(Parser* self, TokenList* tokens);
void free_parser(Parser* self);
void print_parser(Parser* self);


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

typedef void (*ParseFnPtr)(Parser* self, VirtualMachine* vm, bool can_assign);			// ParseFn -point-> void function()

/* parser table of rules row */
typedef struct {
	ParseFnPtr prefix;
	ParseFnPtr infix;
	Precedence precedence;
} ParseRule;

/* parser function prototypes */
Function* parse_tokens(Parser* self, VirtualMachine* vm);
void parse_grouping(Parser* self, VirtualMachine* vm, bool can_assign);
void parse_call(Parser* self, VirtualMachine* vm, bool can_assign);
void parse_i32(Parser* self, VirtualMachine* vm, bool _can_assign);
void parse_f64(Parser* self, VirtualMachine* vm, bool _can_assign);
void parse_unary(Parser* self, VirtualMachine* vm, bool can_assign);
void parse_binary(Parser* self, VirtualMachine* vm, bool can_assign);
void parse_literal(Parser* self, VirtualMachine* vm, bool _can_assign);
void parse_string(Parser* self, VirtualMachine* vm, bool _can_assign);
void parse_identifier(Parser* self, VirtualMachine* vm, bool _can_assign);
void parse_and(Parser* self, VirtualMachine* vm, bool _can_assign);
void parse_or(Parser* self, VirtualMachine* vm, bool _can_assign);



/* parser rules table: Patttern-Action table */
ParseRule static static_syntax_rules[] = {
	/* entry: [index]	= {prefix,			infix,			infix_precedence} */
	[token_left_paren]	= {parse_grouping,	parse_call,		prec_call},
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
	[token_identifier]	= {parse_identifier,NULL,			prec_none},
	[token_string]		= {parse_string,	NULL,			prec_none},
	[token_i32]			= {parse_i32,		NULL,			prec_none},
	[token_f64]			= {parse_f64,		NULL,			prec_none},
	[token_and]			= {NULL,			parse_and,		prec_and},
	[token_class]		= {NULL,			NULL,			prec_none},
	[token_else]		= {NULL,			NULL,			prec_none},
	[token_false]		= {parse_literal,   NULL,			prec_none},
	[token_for]			= {NULL,			NULL,			prec_none},
	[token_fn]			= {NULL,			NULL,			prec_none},
	[token_if]			= {NULL,			NULL,			prec_none},
	[token_null]		= {parse_literal,   NULL,			prec_none},
	[token_or]			= {NULL,			parse_or,		prec_or},
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

