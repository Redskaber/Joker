
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#include "token.h"


inline static void panic_error(const char* msg) {
	fprintf(stderr, "Panic: %s\n", msg);
	exit(1);
}

/* Token */
inline void reset_token(Token* token) {
	token->type = token_eof;
	token->start = NULL;
	token->length = 0;
	token->line = 0;	
}

void print_token(Token* token) {
	if (token != NULL) {
		printf("Token(type: %s, lexeme: \"%.*s\", length: %d, line: %d)",
			macro_token_type_to_string(token->type),
			macro_token_get_lexeme(token->start, token->length),
			token->length,
			token->line
		);
	}
	else {
		printf("Token(NULL)");
	}
}


Token eof_token(line_t line) {
	Token token = {
		.type = token_eof,
		.start = NULL,
		.length = 0,
		.line = line
	};
	return token;
}



/* TokenNode */
TokenNode* create_token_node(Token token) {
	TokenNode* node = (TokenNode*)malloc(sizeof(TokenNode));
	if (node == NULL) {
		panic_error("Failed to allocate memory for token node");
		return NULL;
	}
	node->token = token;
	node->next = NULL;
	return node;
}
void reset_token_node(TokenNode* node) {
	reset_token(&node->token);
}
void free_token_node(TokenNode* node) {
	free(node);
}
void print_token_node(TokenNode* node) {
	print_token(&node->token);
}



/* TokenList */
TokenList* create_token_list() {
	TokenList* list = (TokenList*)malloc(sizeof(TokenList));
	if (list == NULL) {
		panic_error("Failed to allocate memory for token list");
		return NULL;
	}
    init_token_list(list);
	return list;
}

void init_token_list(TokenList* list) {
	list->head = NULL;
	list->tail = NULL;
	list->current = NULL;
	list->previous = NULL;
}

void reset_foreach_pointer(TokenList* list) {
	list->current = list->head;
	list->previous = NULL;
}

void reset_token_list(TokenList* list) {
	init_token_list(list);
}

void free_tokens(TokenList* list) {
	TokenNode* node = list->head;
	while (node != NULL) {
		TokenNode* next = node->next;
		free_token_node(node);
		node = next;
	}
	list->head = NULL;
	list->tail = NULL;
	list->current = NULL;
	list->previous = NULL;
}

void free_token_list(TokenList* list) {
	free_tokens(list);
	free(list);
}


void list_add_token(TokenList* list, Token token) {
	/* list->head is NULL => list is empty */
	if (list->head == NULL) {
		list->head = create_token_node(token);
		list->tail = list->head;
		list->current = list->head;
		list->previous = NULL;
	}
	/* list->head is not NULL => list is not empty */
	else {
		list->tail->next = create_token_node(token);
		list->tail = list->tail->next;
		list->current = list->tail;
		list->previous = list->current->next;
	}
}

void print_tokens(TokenList* list) {
	printf("Tokens:\n");
	TokenNode* node = list->head;
	while (node != NULL) {
		print_token(&node->token);
		printf("\n");
		node = node->next;
	}
}


bool is_empty_tokens(TokenList* list) {
	return list->head == NULL;
}

Token* advance_token(TokenList* list) {
	if (list->current == NULL) {
		return NULL;
	}
	else {
		list->previous = list->current;
		list->current = list->current->next;
		return &list->previous->token;
	}
}

Token* current_token(TokenList* list) {
	if (list->current == NULL) {
		return NULL;
	}
	else {
		return &list->current->token;
	}
}

Token* previous_token(TokenList* list) {
	if (list->previous == NULL) {
		return NULL;
	}
	else {
		return &list->previous->token;
	}
}

Token* first_token(TokenList* list) {
	if (list->head == NULL) {
		return NULL;
	}
	else {
		return &list->head->token;
	}
}

Token* last_token(TokenList* list) {
	if (list->tail == NULL) {
		return NULL;
	}
	else {
		return &list->tail->token;
	}
}

Token* nth_token(TokenList* list, size_t n) {
	TokenNode* node = list->head;
	size_t i = 0;
	while (node != NULL && i < n) {
		node = node->next;
		i++;
	}
	if (node == NULL) {
		return NULL;
	}
	else {
		return &node->token;
	}
}



/* Tests */
static void test_token() {
	Token token = {
		.type = token_eof,
		.start = "EOF",
		.length = 3,
		.line = 10
	};
	print_token(&token);
	printf("\n");
}

static void test_token_node() {
	Token token = {
		.type = token_eof,
		.start = "EOF",
		.length = 3,
		.line = 10
	};
	TokenNode* node = create_token_node(token);
	print_token_node(node);
	printf("\n");
	free_token_node(node);
}

static void test_token_list() {
	char* source = "eof hello 123";
	TokenList* list = create_token_list();
	list_add_token(list, (Token) { .type = token_eof, .start = source, .length = 3, .line = 10 });
	list_add_token(list, (Token) { .type = token_identifier, .start = source + 4, .length = 5, .line = 11 });
	list_add_token(list, (Token) { .type = token_i32, .start = source + 10, .length = 3, .line = 12 });
	print_tokens(list);
	printf("\n");
	free_token_list(list);
}

void tests() {
	test_token();
	test_token_node();
	test_token_list();
}
