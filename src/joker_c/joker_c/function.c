#include <stdio.h>

#include "memory.h"
#include "chunk.h"
#include "string.h"

#include "error.h"
#include "function.h"




Function* new_function() {
	Function* func = macro_allocate_object(Function, obj_function);
	func->arity = 0;
	func->name = NULL;
	init_chunk(&func->chunk);
	return func;
}

void free_function(Function* self) {
	if (self != NULL) {
		free_chunk(&self->chunk);
		macro_free(Function, self);
	}
}

bool is_anonymous_fn(Function* self) {
	return self->name == NULL;
}

void print_function(Function* self) {
	if (self == NULL) {
		panic("Panic: [Function::print_function] Expected parameter 'function' to be non-null, Found null instead.");
		return;
	}

	if (is_anonymous_fn(self)) {
		printf("<scipt>");
		return;
	}

	printf("<Fn %s(args:%d)>", self->name->chars, self->arity);
}


Native* new_native(NativeFnPtr func) {
	Native* native =macro_allocate_object(Native, obj_native);
	native->func = func;
	return native;
}

void free_native(Native* native) {
	if (native != NULL) {
		macro_free(Native, native);
	}
}

void print_native(Native* native) {
	if (native == NULL) {
		panic("Panic: [Native::print_native] Expected parameter 'native' to be non-null, Found null instead.");
		return;
	}

	printf("<Native Fn>");
}

