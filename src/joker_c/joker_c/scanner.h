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



void init_scanner(Scanner* self, const char* source);
void free_scanner(Scanner* self);
Token scan_token(Scanner* self);
void scan_tokens(Scanner* self);



#endif /* joker_scanner_h*/

