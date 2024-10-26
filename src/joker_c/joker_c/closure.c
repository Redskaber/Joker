#include <stdio.h>

#include "error.h"
#include "memory.h"
#include "Fn.h"
#include "closure.h"

/* upvalue_ptrs: { upvalue_ptr, upvalue_ptr, upvalue_ptr,... } */
Closure* new_closure(Fn* fn) {
	UpvaluePtrs upvalue_ptrs = macro_allocate_object(UpvaluePtr, fn->upvalue_count);
	for (int i = 0; i < fn->upvalue_count; i++) {
		upvalue_ptrs[i] = NULL;
	}

	Closure* closure = macro_allocate_object(Closure, obj_closure);
	closure->fn = fn;
	closure->upvalue_ptrs = upvalue_ptrs;
	closure->upvalue_count = fn->upvalue_count;
	return closure;
}

void free_closure(Closure* self) {
	macro_free_array(UpvaluePtr, self->upvalue_ptrs, self->upvalue_count);
	macro_free(Closure, self);
}

void print_closure(Closure* self) {
	if (self == NULL) {
		printf("null\n");
		return;
	}
	print_fn(self->fn);
}