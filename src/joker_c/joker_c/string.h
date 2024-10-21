#pragma once


#ifndef __joker_string_h__
#define __joker_string_h__
#include "common.h"
#include "object.h"
#include "hashmap.h"

#define macro_is_string(value) (is_obj_type(value, obj_string))

#define macro_as_string(value) ((String*)macro_as_obj(value))
#define macro_as_cstring(value) (macro_as_string(value)->chars)

#define macro_as_string_from_obj(obj) ((String*)obj)
#define macro_as_cstring_from_obj(obj) (macro_as_string_from_obj(obj)->chars)

/* flexible array members( 灵活数组成员):
*     1. flexible array member is the last member of a struct.
*     2. flexible array member is declared with the keyword "flexible array member" (e.g. char chars[];).
*     3. flexible array member can be accessed using the pointer to the struct (e.g. chars).
*     4. flexible array member can be resized dynamically.
*/

typedef struct String {
	Object base;
	uint32_t hash;   // hash value of string (string hash)
	int32_t length;
	char chars[];	// flexible array member: from char* need double pointer to char[].
} String;



String* new_string_uninterned(const char* chars, int32_t length);
String* new_string(HashMap* interned_pool, const char* chars, int32_t length);
void free_string(String* string);
String* concat_string_uninterned(String* left, String* right);
String* concat_string(HashMap* interned_pool, String* left, String* right);
bool string_equal(String* left, String* right);
void print_string(String* string);


#endif // !__joker_string_h__