

#ifndef __joker_function_h__
#define __joker_function_h__

#include "common.h"
#include "object.h"
#include "chunk.h"

#define macro_as_function_from_obj(obj) ((Function*)obj)
#define macro_is_function(value) is_obj_type(value, obj_function)
#define macro_as_function(value) ((Function*)macro_as_obj(value))
#define macro_as_function_from_obj_ptr(obj_ptr) ((Function*)macro_as_obj_ptr(obj_ptr))

#define macro_as_native_from_obj(obj) ((Native*)obj)
#define macro_is_native(value) is_obj_type(value, obj_native)
#define macro_as_native(value_ptr) (((Native*)macro_as_obj_ptr(value_ptr))->func)
#define macro_as_native_from_obj_ptr(obj_ptr) ((Native*)macro_as_obj_ptr(obj_ptr))


typedef enum FunctionType {
	type_function,
	type_script,
} FunctionType;


typedef struct Function {
	Object base;
	int arity;
	Chunk chunk;
	String* name;
} Function;


Function* new_function();
void free_function(Function* function);
bool is_anonymous_fn(Function* self);
void print_function(Function* function);


/* Native function type */
typedef Value (*NativeFnPtr)(Value* stack_first_arg, int arg_count);

typedef struct Native {
	Object base;
	NativeFnPtr func;
} Native;

Native* new_native(NativeFnPtr func);
void free_native(Native* native);
void print_native(Native* native);




#endif // __joker_function_h__
