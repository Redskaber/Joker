/*
* object.c
*  Object(struct) <-----> Operation(trait[interface])
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "memory.h"
#include "object.h"
#include "string.h"
#include "native.h"
#include "fn.h"
#include "value.h"



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
Object* allocate_object(size_t size, ObjectType type) {
	Object* object = (Object*)reallocate(NULL, 0, size);
	if (object == NULL) {
		return panic("[Panic] [allocate_object] Expected to allocate memory for object, Found NULL");
	} 
	object->type = type;
	return object;
}

void free_object(Object* object) {
	if (object == NULL) return;
	switch (object->type) {
	case obj_string: free_string(macro_as_string_from_obj(object)); break;
	case obj_fn:     free_fn(macro_as_fn_from_obj(object)); break;
	case obj_native: free_native(macro_as_native_from_obj(object)); break;
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
	case obj_string:	print_string(macro_as_string_from_obj(object)); break;
	case obj_fn:	    print_fn(macro_as_fn_from_obj(object)); break;
	case obj_native:	print_native(macro_as_native_from_obj(object)); break;
	default:			warning("[Warning] [print_object] Unsupported object type: %d", object->type); break;
	}
}
