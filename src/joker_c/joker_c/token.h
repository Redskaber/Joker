#pragma once

#ifndef joker_token_h
#define joker_token_h

#include "common.h"
typedef uint32_t line_t;


/* display Token type to string macro */
#define macro_token_type_to_string(type)			\
	(type == token_left_paren) ? "left_paren" :		\
	(type == token_right_paren)? "right_paren" :	\
	(type == token_left_bracket) ? "left_bracket" :	\
	(type == token_right_bracket)? "right_bracket" :\
	(type == token_left_brace) ? "left_brace" :		\
	(type == token_right_brace)? "right_brace" :	\
	(type == token_right_brace)? "right_brace" :	\
	(type == token_comma)     ? "comma" :			\
	(type == token_dot)       ? "dot" :				\
	(type == token_minus)     ? "minus" :			\
	(type == token_plus)      ? "plus" :			\
	(type == token_semicolon) ? "semicolon" :		\
	(type == token_slash)     ? "slash" :			\
	(type == token_star)      ? "star" :			\
	(type == token_bang)      ? "bang" :			\
	(type == token_bang_equal) ? "bang_equal" :		\
	(type == token_equal)     ? "equal" :			\
	(type == token_equal_equal)? "equal_equal" :	\
	(type == token_greater)   ? "greater" :			\
	(type == token_greater_equal)? "greater_equal" :\
	(type == token_less)      ? "less" :			\
	(type == token_less_equal) ? "less_equal" :		\
	(type == token_identifier) ? "identifier" :		\
	(type == token_string)    ? "string" :			\
	(type == token_i32)       ? "i32" :				\
	(type == token_f64)       ? "f64" :				\
	(type == token_and)       ? "and" :				\
	(type == token_class)     ? "class" :			\
	(type == token_else)      ? "else" :			\
	(type == token_false)     ? "false" :			\
	(type == token_for)       ? "for" :				\
	(type == token_fn)        ? "fn" :				\
	(type == token_if)        ? "if" :				\
	(type == token_null)      ? "null" :			\
	(type == token_or)        ? "or" :				\
	(type == token_print)     ? "print" :			\
	(type == token_return)    ? "return" :			\
	(type == token_super)     ? "super" :			\
	(type == token_self)      ? "self" :			\
	(type == token_true)      ? "true" :			\
	(type == token_var)       ? "var" :				\
	(type == token_while)     ? "while" :			\
	(type == token_break)     ? "break" :			\
	(type == token_continue)  ? "continue" :		\
	(type == token_error)     ? "error" :			\
	(type == token_eof)       ? "eof" :				\
	"unknown"

/* display Token start paremeter pointer offset length substring macro: used %.*s */
#define macro_token_get_lexeme(pointer, length)		\
	(int)length, pointer

/* Token type enum */
typedef enum {
	// Single-character tokens.
	token_left_paren, token_right_paren,			// ()
	token_left_bracket, token_right_bracket,		// []
	token_left_brace, token_right_brace,			// {}
	token_comma, token_dot, token_minus, token_plus,// , . - +
	token_semicolon, token_slash, token_star,		// ; / *
	// One or two character tokens.
	token_bang, token_bang_equal,					// ! !=
	token_equal, token_equal_equal,					// = ==
	token_greater, token_greater_equal,				// > >=
	token_less, token_less_equal,					// < <=
	// Literals.
	token_identifier, token_string, token_i32, token_f64,
	// string interpolation: {}
	// Keywords.
	token_and, token_class, token_else, token_false,
	token_for, token_fn, token_if, token_null, token_or,
	token_print, token_return, token_super, token_self,
	token_true, token_var, token_while, token_break, token_continue,

	token_error, token_eof
} TokenType;


/* Token struct */
typedef struct {
	const char* start;
	uint32_t length;
	line_t line;
	TokenType type;
} Token;


Token eof_token(line_t line);
void reset_token(Token* token);
void print_token(Token* token);


typedef struct {
	Token token;
	struct TokenNode* next;
} TokenNode;

TokenNode* create_token_node(Token token);
void free_token_node(TokenNode* node);
void print_token_node(TokenNode* node);
void reset_token_node(TokenNode* node);



typedef struct {
	TokenNode* head;
	TokenNode* tail;
	TokenNode* current;
	TokenNode* previous;
} TokenList;

TokenList* create_token_list();
void free_token_list(TokenList* list);
void init_token_list(TokenList* list);
void reset_foreach_pointer(TokenList* list);
void reset_token_list(TokenList* list);
void list_add_token(TokenList* list, Token token);
void print_tokens(TokenList* list);
void free_tokens(TokenList* list);
bool is_empty_tokens(TokenList* list);
Token* advance_token(TokenList* list);
Token* previous_token(TokenList* list);
Token* current_token(TokenList* list);
Token* first_token(TokenList* list);
Token* last_token(TokenList* list);
Token* nth_token(TokenList* list, size_t n);

void tests();

#endif /* joker_token_h */
