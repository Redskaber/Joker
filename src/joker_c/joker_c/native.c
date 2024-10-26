#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>

#ifdef _WIN32
#include <windows.h>
#define sleep(x) Sleep((x) * 1000)
#else
#include <unistd.h>
#define sleep(x) usleep((x) * 1000000)
#endif

#include "error.h"
#include "memory.h"
#include "string.h"
#include "vm.h"

#include "native.h"

Native* new_native(NativeFnPtr fn) {
	Native* native = macro_allocate_object(Native, obj_native);
	native->fn = fn;
	return native;
}

void free_native(Native* self) {
	if (self != NULL) {
		macro_free(Native, self);
	}
}

void print_native(Native* self) {
	if (self == NULL) {
		panic("Panic: [Native::print_native] Expected parameter 'native' to be non-null, Found null instead.");
		return;
	}

	printf("<Native Fn>");
}

/* time block */
Value native_clock(VirtualMachine* vm, int arg_count, Value* args) {
	return macro_f64_from_val(clock() / (double)CLOCKS_PER_SEC);
}

Value native_time(VirtualMachine* vm, int arg_count, Value* args) {
	time_t t = time(NULL);
	return macro_f64_from_val((double)t);
}

Value native_sleep(VirtualMachine* vm, int arg_count, Value* args) {
	if (arg_count != 1) {
		panic("Panic: [Native::sleep] Expected 1 argument, found %d", arg_count);
		return macro_null_var;
	}
	if (!macro_matches(args[0], VAL_I32)) {
		panic("Panic: [Native::sleep] Expected argument to be an i32, found %s", macro_type_name(args[0]));
		return macro_null_var;
	}
	sleep(macro_as_i32(args[0]));
	return macro_null_var;
}

Value native_date(VirtualMachine* vm, int arg_count, Value* args) {
	if (arg_count != 0) {
		panic("Panic: [Native::date] Expected 0 arguments, found %d", arg_count);
		return macro_null_var;
	}

	time_t t = time(NULL);
	struct tm tm = *localtime(&t);
	char date_str[100];
	strftime(date_str, 100, "%Y-%m-%d %H:%M:%S", &tm);
	String* str = new_string(&vm->strings, date_str, (int32_t)strlen(date_str));
	return macro_obj_from_val(str);
}

Value native_now(VirtualMachine* vm, int arg_count, Value* args) {
	return native_date(vm, arg_count, args);
}