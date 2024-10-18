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

static Token make_token(Scanner* scanner, TokenType token_type) {
	Token token;

	token.type = token_type;
	token.start = scanner->start;
	token.length = (uint32_t)(scanner->current - scanner->start);
	token.line = scanner->line;

	return token;
}

static Token error_token(Scanner* scanner, const char* msg) {
	Token token;

	token.type = token_error;
	token.start = msg;
	token.length = (uint32_t)strlen(msg);
	token.line = scanner->line;

	return token;
}

static char advance(Scanner* scanner) {
	if (is_at_end(scanner)) return '\0';
	scanner->current++;
	return scanner->current[-1];
}

static bool match(Scanner* scanner, const char expected) {
	if (is_at_end(scanner)) return false;
	if (*scanner->current != expected) return false;
	scanner->current++;
	return true;
}

static char peek(Scanner* scanner) {
	return *scanner->current;
}

static char peek_next(Scanner* scanner) {
	if (is_at_end(scanner)) return '\0';
	return scanner->current[1];
}

static bool is_at_end(Scanner* scanner) {
	return peek(scanner) == '\0';
}


static void skip_with_espace(Scanner* scanner) {
	while (true) {
		char ch = peek(scanner);
		switch (ch) 
		{
		case ' ':
		case '\r':
		case '\t':
			advance(scanner);
			break;
		case '\n':
			scanner->line++;
			advance(scanner);
			break;
		case '/':
			switch (peek_next(scanner)) 
			{
			case '/':
				while (peek(scanner) != '\n' && !is_at_end(scanner)) advance(scanner);
				break;
			case '*':
				advance(scanner);
				advance(scanner);

				bool is_success = false;
				while (!is_at_end(scanner)) {
					if (peek(scanner) == '\n') scanner->line++;
					if (peek(scanner) == '*' && peek_next(scanner) == '/') {
						advance(scanner);
						advance(scanner);
						is_success = true;
						break;
					}
					advance(scanner);
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

void init_scanner(Scanner* scanner, const char* source) {
	scanner->start = source;
	scanner->current = source;
	scanner->line = 1;
	scanner->tokens = create_token_list();
}

void free_scanner(Scanner* scanner) {
	scanner->start = NULL;
	scanner->current = NULL;
	scanner->line = 0;
	free_token_list(scanner->tokens);
	scanner->tokens = NULL;
}

void scan_tokens(Scanner* scanner) {
	while (!is_at_end(scanner)) {
		Token token = scan_token(scanner);
		list_add_token(scanner->tokens, token);
	}
}


/* scan token */
Token scan_token(Scanner* scanner) {
	skip_with_espace(scanner);

	scanner->start = scanner->current;
	if (is_at_end(scanner)) return make_token(scanner, token_eof);

	char ch = advance(scanner);
	
	if (is_alpha(ch)) return scan_identifier(scanner);
	if (is_digit(ch)) return scan_number(scanner);

	switch (ch) {
		case '(': return make_token(scanner, token_left_paren);
		case ')': return make_token(scanner, token_right_paren);
		case '[': return make_token(scanner, token_left_bracket);
		case ']': return make_token(scanner, token_right_bracket);
		case '{': return make_token(scanner, token_left_brace);
		case '}': return make_token(scanner, token_right_brace);
		case ';': return make_token(scanner, token_semicolon);
		case ',': return make_token(scanner, token_comma);
		case '.': return make_token(scanner, token_dot);
		case '-': return make_token(scanner, 
			match(scanner, '>') ? token_arrow : token_minus);
		case '+': return make_token(scanner, token_plus);
		case '/': return make_token(scanner, token_slash);
		case '*': return make_token(scanner, token_star);
		case '!': return make_token(scanner, 
			match(scanner, '=') ? token_bang_equal : token_bang);
		case '=': return make_token(scanner, 
			match(scanner, '=') ? token_equal_equal : 
			match(scanner, '>') ? token_fat_arrow : token_equal);
		case '<': return make_token(scanner, 
			match(scanner, '=') ? token_less_equal : token_less);
		case '>': return make_token(scanner, 
			match(scanner, '=') ? token_greater_equal : token_greater);
		case '"': return scan_string(scanner);
	}

	return error_token(scanner, "Unexpected character.");
}

/* scan string token, handler string interpolation: "literal{expression}{variable}" */
static Token scan_string(Scanner* scanner) {
	while (peek(scanner) != '"' && !is_at_end(scanner)) {
		// handle escape sequence
		if (peek(scanner) == '\n') scanner->line++;
		advance(scanner);
	}

	if (is_at_end(scanner)) return error_token(scanner, "Unterminated string.");
	
	// The closing quote. '"'
	advance(scanner);
	return make_token(scanner, token_string);
}


static Token scan_number(Scanner* scanner) {
	while (is_digit(peek(scanner))) advance(scanner);

	// Look for a fractional part.
	if (peek(scanner) == '.') {
		// error
		if (!is_digit(peek_next(scanner))) {
			fprintf(stderr, "F64 number, int part after '.' after need value.");
			exit(enum_scanner_error);
		}

		// Consume the ".".
		advance(scanner);
		while (is_digit(peek(scanner))) advance(scanner);
		return make_token(scanner, token_f64);
	}
	return make_token(scanner, token_i32);
}

static Token scan_identifier(Scanner* scanner) {

	while (is_alpha(peek(scanner)) || is_digit(peek(scanner))) advance(scanner);
	return make_token(scanner, identifier_type(scanner));
}

/* used to check keyword */
static TokenType check_keyword(Scanner* scanner, int start, int length, const char* rest, TokenType type) {
	if (scanner->current - scanner->start == start + length &&
		memcmp(scanner->start + start, rest, length) == 0) {
		return type;
		}
	return token_identifier;
}

/* used trie to match identifier type */
static TokenType identifier_type(Scanner* scanner) {
	switch (scanner->start[0]) {
		case 'a': return check_keyword(scanner, 1, 2, "nd", token_and);
		case 'b': return check_keyword(scanner, 1, 4, "reak", token_break);
		case 'c': 
			/* exclude one letter identifier */
			if (scanner->current - scanner->start > 1) {
				switch (scanner->start[1]) {
					case 'l': return check_keyword(scanner, 2, 3, "ass", token_class);
					case 'o': return check_keyword(scanner, 2, 6, "ntinue", token_continue);
				}
			};
			break;
		case 'e': return check_keyword(scanner, 1, 3, "lse", token_else);
		case 'f':
			/* exclude one letter identifier */
			if (scanner->current - scanner->start > 1) {
				switch (scanner->start[1]) {
					case 'a': return check_keyword(scanner, 2, 3, "lse", token_false);
					case 'o': return check_keyword(scanner, 2, 1, "r", token_for);
					case 'n': return token_fn;
				}
			};
			break;
		case 'i': return check_keyword(scanner, 1, 1, "f", token_if);
		case 'm': return check_keyword(scanner, 1, 4, "atch", token_match);
		case 'n': return check_keyword(scanner, 1, 3, "ull", token_null);
		case 'o': return check_keyword(scanner, 1, 1, "r", token_or);
		case 'p': return check_keyword(scanner, 1, 4, "rint", token_print);
		case 'r': return check_keyword(scanner, 1, 5, "eturn", token_return);
		case 't': return check_keyword(scanner, 1, 3, "rue", token_true);
		case 'v': return check_keyword(scanner, 1, 2, "ar", token_var);
		case 'w': return check_keyword(scanner, 1, 4, "hile", token_while);
		case 's': 
			if (scanner->current - scanner->start > 1) {
				switch (scanner->start[1]) {
					case 'e': return check_keyword(scanner, 2, 2, "lf", token_self);
					case 'u': return check_keyword(scanner, 2, 3, "per", token_super);
				}
			}
			break;
	}

	return token_identifier;
}


