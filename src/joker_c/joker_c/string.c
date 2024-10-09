#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "string.h"


// macro for allocate fixed array
#define macro_allocate_fixed_array(type, elem_type, arr_size, object_type) \
    (type*)allocate_object((sizeof(type) + arr_size * sizeof(elem_type)), object_type)
// macro for free fixed array
#define macro_free_fixed_array(type, pointer, elem_type, arr_size) \
    reallocate(pointer, (sizeof(type) + arr_size * sizeof(elem_type)), 0)



static uint32_t hash_string(const char* key, uint32_t length);



static void* panic_error(const char* message) {
	fprintf(stderr, "Panic: %s\n", message);
	exit(1);
	return NULL;
}


/* flexible array member */
String* new_string_uninterned(const char* chars, uint32_t length) {
	if (chars == NULL && length != 0) {
		return panic_error("Expected non-null string chars, Found null string chars.");
	}
	if (length < 0 || length > UINT32_MAX - 1) {
		return panic_error("Expected string length between 0 and 4294967295, Found invalid string length.");
	}

	/* fixed array member*/
	String* string = macro_allocate_fixed_array(String, char, length + 1, obj_string);
	string->length = length;
	memset(string->chars, 0, length + 1);
	if (chars != NULL) {
		memcpy_s(string->chars, length, chars, length);
	}
	string->chars[length] = '\0';

	/* Hash string */
	string->hash = hash_string(string->chars, length);

	return string;
}

String* new_string(HashMap* interned_pool, const char* chars, uint32_t length) {
	if (chars == NULL && length != 0) {
		return panic_error("Expected non-null string chars, Found null string chars.");
	}
	if (length < 0 || length > UINT32_MAX - 1) {
		return panic_error("Expected string length between 0 and 4294967295, Found invalid string length.");
	}
	/* Check if the string is already interned */
	uint32_t hash = hash_string(chars, length);
	String* interned_string = hashmap_find_key(interned_pool, chars, length, hash);
	if (interned_string != NULL) {
		return interned_string;
	}
	/* Create a new string */
	String* string = macro_allocate_fixed_array(String, char, length + 1, obj_string);
	string->length = length;
	memset(string->chars, 0, length + 1);
	if (chars != NULL) {
		memcpy_s(string->chars, length, chars, length);
	}
	string->chars[length] = '\0';
	string->hash = hash;
	/* Add the new string to the interned pool */
	hashmap_set(interned_pool, string, macro_null_var);
	return string;
}

void free_string(String* string) {
	if (string == NULL) {
		panic_error("Expected non-null string, Found null string.");
		return;
	}
	if (string->chars == NULL) {
		panic_error("Expected non-null string chars, Found null string chars.");
		return;
	}
	if (string->length < 0 || string->length > UINT32_MAX - 1) {
		panic_error("Expected string length between 0 and 4294967295, Found invalid string length.");
		return;
	}
	macro_free_fixed_array(String, string, char, string->length + 1);
}

String* concat_string_uninterned(String* left, String* right) {
	if (left == NULL || right == NULL) {
		return panic_error("Expected non-null string, Found null string.");
	}
	if (left->chars == NULL || right->chars == NULL) {
		return panic_error("Expected non-null string chars, Found null string chars.");
	}
	if (left->length < 0 || left->length > UINT32_MAX - 1) {
		return panic_error("Expected left string length between 0 and 4294967295, Found invalid string length.");
	}
	if (right->length < 0 || right->length > UINT32_MAX - 1) {
		return panic_error("Expected right string length between 0 and 4294967295, Found invalid string length.");
	}
	if (left->length + right->length > UINT32_MAX - 1) {
		return panic_error("Expected result string length between 0 and 4294967295, Found invalid string length.");
	}

	uint32_t new_length = left->length + right->length;
	String* result = macro_allocate_fixed_array(String, char, new_length + 1, obj_string);
	if (result == NULL) {
		return panic_error("Expected non-null memory, Found null memory.");
	}

	result->length = new_length;
	memset(result->chars, 0, new_length + 1);
	memcpy_s(result->chars, new_length, left->chars, left->length);
	memcpy_s(result->chars + left->length, new_length - left->length, right->chars, right->length);
	result->chars[new_length] = '\0';

	/* Hash string */
	result->hash = hash_string(result->chars, new_length);

	return result;
}

String* concat_string(HashMap* interned_pool, String* left, String* right) {
	String* result = concat_string_uninterned(left, right);
	if (result == NULL) {
		return panic_error("Expected non-null string, Found null string.");
	}
	String* interned_string = hashmap_find_key(interned_pool, result->chars, result->length, result->hash);
	if (interned_string != NULL) {
		free_string(result);
		return interned_string;
	}
	hashmap_set(interned_pool, result, macro_null_var);
	return result;
}

/*
* base: return (left->length == right->length && memcmp(left->chars, right->chars, left->length) == 0)
* bucause used interned pool, so same string, it pointes to the same memory address.
*/
bool string_equal(String* left, String* right) {
	return left == right;
}

/* Hash string
*  - FNV-1a:
*  hash need: 1.determistic 2.uniform 3.fast
*/
static uint32_t hash_string(const char* key, uint32_t length) {
	uint32_t hash = 2166136261u;
	for (size_t i = 0; i < length; i++) {
		hash ^= (uint8_t)key[i];  // xor with the next byte, 1 char = 1 byte
		hash *= 16777619u;
	}
	return hash;
}


void print_string(String* string) {
	printf("%s", string->chars);
}
