#pragma once

#ifndef __joker_closure_h__
#define __joker_closure_h__

#include "object.h"
#include "fn.h"
#include "upvalue.h"

typedef struct Closure {
	Object base;				// object base
	Fn* fn;						// function object
	UpvaluePtrs upvalue_ptrs;	// upvalue pointer array (used heap memory, once Upvalue pointer) [upvalue link list]
	int upvalue_count;			// upvalue count
} Closure;

Closure* new_closure(Fn* fn);
void free_closure(Closure* self);
void print_closure(Closure* self);

#endif // __joker_closure_h__
