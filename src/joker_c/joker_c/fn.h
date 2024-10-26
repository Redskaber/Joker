#ifndef __joker_function_h__
#define __joker_function_h__

#include "common.h"
#include "object.h"
#include "chunk.h"

#define macro_as_fn_from_obj(obj)				((Fn*)obj)
#define macro_as_fn(value)						((Fn*)macro_as_obj(value))
#define macro_as_fn_from_obj_ptr(obj_ptr)		((Fn*)macro_as_obj_ptr(obj_ptr))

#define macro_as_native_from_obj(obj)			((Native*)obj)
#define macro_as_native(value_ptr)				(((Native*)macro_as_obj_ptr(value_ptr))->fn)
#define macro_as_native_from_obj_ptr(obj_ptr)	((Native*)macro_as_obj_ptr(obj_ptr))

#define macro_as_closure_from_obj(obj)			((Closure*)obj)
#define macro_as_closure(value_ptr)				((Closure*)macro_as_obj_ptr(value_ptr))
#define macro_as_closure_from_obj_ptr(obj_ptr)	((Closure*)macro_as_obj_ptr(obj_ptr))

#define macro_is_fn(value)		is_obj_type(value, obj_fn)
#define macro_is_native(value)	is_obj_type(value, obj_native)
#define macro_is_closure(value) is_obj_type(value, obj_closure)

typedef enum FnType {
	type_fn,
	type_script,
} FnType;

typedef struct Fn {
	Object base;
	int arity;
	Chunk chunk;
	String* name;
	int upvalue_count;
} Fn;

Fn* new_fn();
void free_fn(Fn* self);
bool is_anonymous_fn(Fn* self);
void print_fn(Fn* self);

#endif // __joker_function_h__
