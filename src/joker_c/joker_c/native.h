#pragma once

#ifndef __joker_c_native_h__
#define __joker_c_native_h__
#include "fn.h"

/* Native function type */
typedef struct VirtualMachine VirtualMachine;
typedef Value(*NativeFnPtr)(VirtualMachine* vm, int arg_count, Value* args);

typedef struct Native {
	Object base;
	NativeFnPtr fn;
} Native;

Native* new_native(NativeFnPtr fn);
void free_native(Native* self);
void print_native(Native* self);

/* time block */
Value native_clock(VirtualMachine* vm, int arg_count, Value* args);
Value native_time(VirtualMachine* vm, int arg_count, Value* args);
Value native_sleep(VirtualMachine* vm, int arg_count, Value* args);
Value native_date(VirtualMachine* vm, int arg_count, Value* args);
Value native_now(VirtualMachine* vm, int arg_count, Value* args);

#endif // __joker_c_native_h__
