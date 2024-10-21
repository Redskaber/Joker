#pragma once


#ifndef joker_object_h
#define joker_object_h

#include "common.h"
#include "value.h"

#define macro_obj_type(value) (macro_as_obj(value)->type)
#define macro_obj_ptr_type(value_ptr) (macro_as_obj_ptr(value_ptr)->type)

/* Object type */
typedef enum ObjectType {
	obj_string,
	obj_fn,
	obj_native,
} ObjectType;



/* Object structure£º store object total of information */
typedef struct Object {
	ObjectType type;			// label of object type
} Object;

Object* allocate_object(size_t size, ObjectType type);
bool object_equal(Object* left, Object* right);
void free_object(Object* object);
void print_object(Object* object);


// macro for allocate object
#define macro_allocate_object(type, object_type) \
	(type*)allocate_object(sizeof(type), object_type)


/* Why not just put the body of this function right in the macro? 
* It's not a good practice to put a function body inside a macro, because it can cause 
* unexpected behavior when the macro is expanded.
* (e.g. is_string(pop()) will double pop)
*/
static inline bool is_obj_type(Value value, ObjectType type) {
	return macro_is_obj(value) && macro_as_obj(value)->type == type;  // is object and type match
}



#endif /* joker_object_h */
