/*
* 
* 字典树(trie) && 状态机(state machine)[DFA|NFA]
* Lex => 给它一个关于语法的描述, 堆正则表达式(Heap of Regular Expression)，它会自动生成词法分析器的代码(DFA or NFA)。
* 
* 正则表达式(Regular Expression) -> 正则表达式引擎(Regular Expression Engine) -> 词法分析器(Lexical Analyzer) -> 语法分析器(Syntax Analyzer) -> 语义分析器(Semantic Analyzer)[DFA|NFA] -> 代码生成器(Code Generator)[编译器|解释器]
*
* 正则表达式转换为DFA的算法 => 龙书(Dragon Book)
* 正则表达式转换为NFA的算法 => Thompson算法(Thompson's Construction Algorithm)
* 
* TODO: 使用正则表达式引擎生成词法分析器的代码（DFA or NFA）
* 
* 1.string interpolation(字符串插值) => "xxx
	{variable}
	dddd
	{literal}
	ddd
	{expression}
	..."
*
* 2.泛型编程(Generic Programming) => 泛型数据类型(Generic Data Type) <T>s
* 
* 3.上下文式关键字(Contextual Keywords) => 关键字(Keyword)
* 
*/


#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "error.h"
#include "scanner.h"



static bool is_at_end(Scanner* scanner);
Token scan_token(Scanner* scanner);
void scan_tokens(Scanner* scanner);
static Token scan_string(Scanner* scanner);
static Token scan_number(Scanner* scanner);
static Token scan_identifier(Scanner* scanner);
static TokenType identifier_type(Scanner* scanner);



static bool is_digit(char ch) {
	return ch >= '0' && ch <= '9';
}

static bool is_alpha(char ch) {
	return (ch >= 'a' && ch <= 'z') 
		|| (ch >= 'A' && ch <= 'Z') 
		|| ch == '_';
}

static Token make_token(Scanner* self, TokenType token_type) {
	Token token;

	token.type = token_type;
	token.start = self->start;
	token.length = (uint32_t)(self->current - self->start);
	token.line = self->line;

	return token;
}

static Token error_token(Scanner* self, const char* msg) {
	Token token;

	token.type = token_error;
	token.start = msg;
	token.length = (uint32_t)strlen(msg);
	token.line = self->line;

	return token;
}

static char advance(Scanner* self) {
	if (is_at_end(self)) return '\0';
	self->current++;
	return self->current[-1];
}

static bool match(Scanner* self, const char expected) {
	if (is_at_end(self)) return false;
	if (*self->current != expected) return false;
	self->current++;
	return true;
}

static char peek(Scanner* self) {
	return *self->current;
}

static char peek_next(Scanner* self) {
	if (is_at_end(self)) return '\0';
	return self->current[1];
}

static bool is_at_end(Scanner* self) {
	return peek(self) == '\0';
}


static void skip_with_espace(Scanner* self) {
	while (true) {
		char ch = peek(self);
		switch (ch) 
		{
		case ' ':
		case '\r':
		case '\t':
			advance(self);
			break;
		case '\n':
			self->line++;
			advance(self);
			break;
		case '/':
			switch (peek_next(self)) 
			{
			case '/':
				while (peek(self) != '\n' && !is_at_end(self)) advance(self);
				break;
			case '*':
				advance(self);
				advance(self);

				bool is_success = false;
				while (!is_at_end(self)) {
					if (peek(self) == '\n') self->line++;
					if (peek(self) == '*' && peek_next(self) == '/') {
						advance(self);
						advance(self);
						is_success = true;
						break;
					}
					advance(self);
				}
				if (!is_success) {
					fprintf(stderr, "Unterminated comment.");
					exit(enum_scanner_error);
				}
				break;
			default:
				return;
			}
			break;
		default:
			return;
		}
	}
}

void init_scanner(Scanner* self, const char* source) {
	self->start = source;
	self->current = source;
	self->line = 1;
	self->tokens = create_token_list();
}

void free_scanner(Scanner* self) {
	self->start = NULL;
	self->current = NULL;
	self->line = 0;
	free_token_list(self->tokens);
	self->tokens = NULL;
}

void scan_tokens(Scanner* self) {
	while (!is_at_end(self)) {
		Token token = scan_token(self);
		list_add_token(self->tokens, token);
	}
}


/* scan token */
Token scan_token(Scanner* self) {
	skip_with_espace(self);

	self->start = self->current;
	if (is_at_end(self)) return make_token(self, token_eof);

	char ch = advance(self);
	
	if (is_alpha(ch)) return scan_identifier(self);
	if (is_digit(ch)) return scan_number(self);

	switch (ch) {
		case '(': return make_token(self, token_left_paren);
		case ')': return make_token(self, token_right_paren);
		case '[': return make_token(self, token_left_bracket);
		case ']': return make_token(self, token_right_bracket);
		case '{': return make_token(self, token_left_brace);
		case '}': return make_token(self, token_right_brace);
		case ';': return make_token(self, token_semicolon);
		case ',': return make_token(self, token_comma);
		case '.': return make_token(self, token_dot);
		case '-': return make_token(self, 
			match(self, '>') ? token_arrow : token_minus);
		case '+': return make_token(self, token_plus);
		case '/': return make_token(self, token_slash);
		case '*': return make_token(self, token_star);
		case '!': return make_token(self, 
			match(self, '=') ? token_bang_equal : token_bang);
		case '=': return make_token(self, 
			match(self, '=') ? token_equal_equal : 
			match(self, '>') ? token_fat_arrow : token_equal);
		case '<': return make_token(self, 
			match(self, '=') ? token_less_equal : token_less);
		case '>': return make_token(self, 
			match(self, '=') ? token_greater_equal : token_greater);
		case '"': return scan_string(self);
	}

	return error_token(self, "Unexpected character.");
}

/* scan string token, handler string interpolation: "literal{expression}{variable}" */
static Token scan_string(Scanner* self) {
	while (peek(self) != '"' && !is_at_end(self)) {
		// handle escape sequence
		if (peek(self) == '\n') self->line++;
		advance(self);
	}

	if (is_at_end(self)) return error_token(self, "Unterminated string.");
	
	// The closing quote. '"'
	advance(self);
	return make_token(self, token_string);
}


static char* strndup(const char* src, size_t n) {
	if (src == NULL) return NULL;
	if (n == 0) return NULL;

	char* result = (char*)malloc(n + 1);
	if (result == NULL) return panic("Expected malloc() to succeed, Found memory allocation failure.");
	
	memset(result, 0, n + 1);
	memcpy_s(result, n + 1, src, n);
	result[n] = '\0';
	return result;
}

/* scan number token, handler f64 number 
* 
* TODO: 
* 1. i32 number support
* 2. i64 number support
* 3. f32 number support
* 4. f64 number support
* 
* number(maby big big number)：
* 1. xxxxxx
* 2. xxx.xx
*/
static TokenType check_number(Scanner* self, bool is_float) {
	char* number_str = strndup(self->start, self->current - self->start);
	if (number_str == NULL) return token_error;

	// auto check number type
	TokenType result = token_error;
	if (is_float) {
		result = token_f64;
	}
	else {
		// default i32, if not type label, it will be i32
		// if number > i32max value, auto big number
		size_t valie = strtoul(number_str, NULL, 10);
		if (valie > INT32_MAX) {
			result = token_i64;
		}
		else {
			result = token_i32;
		}
	}
	printf("number_str: %s, result: %s\n", number_str, macro_token_type_to_string(result));
	free(number_str);
	
	return result;
}


static Token scan_number(Scanner* self) {
	bool is_float = false;

	while (is_digit(peek(self))) advance(self);

	// Look for a fractional part.
	if (peek(self) == '.') {
		// error
		if (!is_digit(peek_next(self))) {
			fprintf(stderr, "F64 number, int part after '.' after need value.");
			exit(enum_scanner_error);
		}
		is_float = true;
		// Consume the ".".
		advance(self);
		while (is_digit(peek(self))) advance(self);
		// return make_token(scanner, token_f64);
	}
	return make_token(self, check_number(self, is_float));
}

static Token scan_identifier(Scanner* self) {

	while (is_alpha(peek(self)) || is_digit(peek(self))) advance(self);
	return make_token(self, identifier_type(self));
}

/* used to check keyword */
static TokenType check_keyword(Scanner* self, int start, int length, const char* rest, TokenType type) {
	if (self->current - self->start == start + length &&
		memcmp(self->start + start, rest, length) == 0) {
		return type;
		}
	return token_identifier;
}

/* used trie to match identifier type */
static TokenType identifier_type(Scanner* self) {
	switch (self->start[0]) {
		case 'a': return check_keyword(self, 1, 2, "nd", token_and);
		case 'b': return check_keyword(self, 1, 4, "reak", token_break);
		case 'c': 
			/* exclude one letter identifier */
			if (self->current - self->start > 1) {
				switch (self->start[1]) {
					case 'l': return check_keyword(self, 2, 3, "ass", token_class);
					case 'o': return check_keyword(self, 2, 6, "ntinue", token_continue);
				}
			};
			break;
		case 'e': return check_keyword(self, 1, 3, "lse", token_else);
		case 'f':
			/* exclude one letter identifier */
			if (self->current - self->start > 1) {
				switch (self->start[1]) {
					case 'a': return check_keyword(self, 2, 3, "lse", token_false);
					case 'o': return check_keyword(self, 2, 1, "r", token_for);
					case 'n': return token_fn;
				}
			};
			break;
		case 'i': return check_keyword(self, 1, 1, "f", token_if);
		case 'm': return check_keyword(self, 1, 4, "atch", token_match);
		case 'n': return check_keyword(self, 1, 3, "ull", token_null);
		case 'o': return check_keyword(self, 1, 1, "r", token_or);
		case 'p': return check_keyword(self, 1, 4, "rint", token_print);
		case 'r': return check_keyword(self, 1, 5, "eturn", token_return);
		case 't': return check_keyword(self, 1, 3, "rue", token_true);
		case 'v': return check_keyword(self, 1, 2, "ar", token_var);
		case 'w': return check_keyword(self, 1, 4, "hile", token_while);
		case 's': 
			if (self->current - self->start > 1) {
				switch (self->start[1]) {
					case 'e': return check_keyword(self, 2, 2, "lf", token_self);
					case 'u': return check_keyword(self, 2, 3, "per", token_super);
				}
			}
			break;
	}

	return token_identifier;
}


