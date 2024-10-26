#include <stdio.h>

#include "memory.h"
#include "chunk.h"
#include "string.h"

#include "error.h"
#include "fn.h"

Fn* new_fn() {
	Fn* fn = macro_allocate_object(Fn, obj_fn);
	fn->name = NULL;
	fn->arity = 0;
	fn->upvalue_count = 0;
	init_chunk(&fn->chunk);
	return fn;
}

void free_fn(Fn* self) {
	if (self != NULL) {
		free_chunk(&self->chunk);
		macro_free(Fn, self);
	}
}

bool is_anonymous_fn(Fn* self) {
	return self->name == NULL;
}

void print_fn(Fn* self) {
	if (self == NULL) {
		panic("Panic: [Function::print_function] Expected parameter 'function' to be non-null, Found null instead.");
		return;
	}

	if (is_anonymous_fn(self)) {
		printf("<scipt>");
		return;
	}

	printf("<Fn %s(args: %d)>", self->name->chars, self->arity);
}