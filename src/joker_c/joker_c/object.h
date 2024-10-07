#pragma once


#ifndef joker_object_h
#define joker_object_h

#include "common.h"
#include "value.h"

#define macro_obj_type(value) (macro_as_obj(value)->type)

#define macro_is_string(value) (is_obj_type(value, obj_string))

#define macro_as_string(value) ((StringObject*)macro_as_obj(value))
#define macro_as_cstring(value) (macro_as_string(value)->chars)

#define macro_as_string_from_obj(obj) ((StringObject*)obj)
#define macro_as_cstring_from_obj(obj) (macro_as_string_from_obj(obj)->chars)



/* Object type */
typedef enum JokerObjectType {
	obj_string,
} ObjectType;



/* Object structure： store object total of information */
typedef struct Object {
	ObjectType type;			// label of object type
} Object;

bool object_equal(Object* left, Object* right);
void free_object(Object* object);
void print_object(Object* object);


/* flexible array members( 灵活数组成员): 
*     1. flexible array member is the last member of a struct.
*     2. flexible array member is declared with the keyword "flexible array member" (e.g. char chars[];).
*     3. flexible array member can be accessed using the pointer to the struct (e.g. chars).
*     4. flexible array member can be resized dynamically.
*/

typedef struct StringObject {
	Object base;
	size_t length;
	char chars[];	// flexible array member: from char* need double pointer to char[].
	// flexible array member
} StringObject;



StringObject* new_string(const char* chars, size_t length);
void free_string(StringObject* string);
StringObject* concat_string(StringObject* left, StringObject* right);
bool string_equal(StringObject* left, StringObject* right);
void print_string(StringObject* string);



/* Why not just put the body of this function right in the macro? 
* It's not a good practice to put a function body inside a macro, because it can cause 
* unexpected behavior when the macro is expanded.
* (e.g. is_string(pop()) will double pop)
*/
static inline bool is_obj_type(Value value, ObjectType type) {
	return macro_is_obj(value) && macro_as_obj(value)->type == type;  // is object and type match
}



#endif /* joker_object_h */
