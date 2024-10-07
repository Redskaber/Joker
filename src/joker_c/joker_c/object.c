/*
* object.c
*  Object(struct) <-----> Operation(trait[interface])
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "value.h"


// macro for allocate object
#define macro_allocate_object(type, object_type) \
	(type*)allocate_object(sizeof(type), object_type)

// macro for allocate fixed array
#define macro_allocate_fixed_array(type, elem_type, arr_size, object_type) \
    (type*)allocate_object((sizeof(type) + arr_size * sizeof(elem_type)), object_type)
// macro for free fixed array
#define macro_free_fixed_array(type, pointer, elem_type, arr_size) \
    reallocate(pointer, (sizeof(type) + arr_size * sizeof(elem_type)), 0)


// static functions
static Object* allocate_object(size_t size, ObjectType type);
static bool type_check(Object* object, ObjectType expected_type);




void* panic_error(const char* message) {
	fprintf(stderr, "Panic: %s\n", message);
	exit(1);
	return NULL;
}



/* flexible array member */
StringObject* new_string(const char* chars, size_t length) {
	if (chars == NULL && length != 0) {
		return panic_error("Invalid string chars");
	}
	if (length < 0 || length > UINT64_MAX - 1) {
		return panic_error("Invalid string length");
	}

	/* fixed array member*/
	StringObject* string = macro_allocate_fixed_array(StringObject, char, length + 1, obj_string);
	string->length = length;
	memset(string->chars, 0, length + 1);
	if (chars != NULL) {
		memcpy_s(string->chars, length, chars, length);
	}
	string->chars[length] = '\0';

	return string;
}

void free_string(StringObject* string) {
	if (string == NULL) {
		panic_error("Invalid string");
		return;
	}
	if (string->chars == NULL) {
		panic_error("Invalid string chars");
		return;
	}
	if (string->length < 0 || string->length > UINT64_MAX - 1) {
		panic_error("Invalid string length");
		return;
	}
	macro_free_fixed_array(StringObject, string, char, string->length + 1);
}

StringObject* concat_string(StringObject* left, StringObject* right) {
	if (left == NULL || right == NULL) {
		return panic_error("Invalid string concatenation");
	}
	if (left->chars == NULL || right->chars == NULL) {
		return panic_error("Invalid string concatenation");
	}

	size_t new_length = left->length + right->length;
	StringObject* result = macro_allocate_fixed_array(StringObject, char, new_length + 1, obj_string);
	result->length = new_length;
	memcpy_s(result->chars, new_length, left->chars, left->length);
	memcpy_s(result->chars + left->length, new_length - left->length, right->chars, right->length);
	result->chars[new_length] = '\0';

	return result;
}

bool string_equal(StringObject* left, StringObject* right) {
	return left->length == right->length 
		&& memcmp(left->chars, right->chars, left->length) == 0;
}

void print_string(StringObject* string) {
	printf("%s", string->chars);
}


/*
* Allocate a new object of the given size and type.
* 
* -------Object Layout------
* |  |-----Sub Object----| |
* |  |                   | |
* |  |                   | |
* |  |-------------------| |
* --------------------------
* 
* Object Size = SubObject Size
* The object object is just the shell of the concrete object, 
* and the specific size of the object object is determined by the specific type.
* 
* The sub object is the actual object data, and its size is determined by the specific type.
*/
static Object* allocate_object(size_t size, ObjectType type) {
	Object* object = (Object*)reallocate(NULL, 0, size);
	if (object == NULL) {
		return panic_error("Failed to allocate object");
	} 
	object->type = type;
	return object;
}

void free_object(Object* object) {
	if (object == NULL) return;
	switch (object->type) {
	case obj_string: free_string(macro_as_string_from_obj(object)); break;
	default: break;
	}
}

static bool type_check(Object* object, ObjectType expected_type) {
	return object->type == expected_type;
}

bool object_equal(Object* left, Object* right) {
	if (!type_check(left, right->type)) return false;
	switch (left->type) {
	case obj_string:
		return string_equal(macro_as_string_from_obj(left), macro_as_string_from_obj(right));
	default:
		return false;
	}
}

void print_object(Object* object) {
	switch (object->type) {
	case obj_string:
		print_string(macro_as_string_from_obj(object));
		break;
	default:
		printf("Unknown object type\n");
		break;
	}
}
