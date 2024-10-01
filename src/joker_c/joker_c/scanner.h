#pragma once

#ifndef joker_scanner_h
#define joker_scanner_h

#include "common.h"
#include "token.h"


typedef struct {
	TokenList* tokens;
	const char* start;
	const char* current;
	line_t line;
} Scanner;



void init_scanner(Scanner* scanner, const char* source);
void free_scanner(Scanner* scanner);
Token scan_token(Scanner* scanner);
void scan_tokens(Scanner* scanner);



#endif /* joker_scanner_h*/

