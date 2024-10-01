#include <stdio.h>


#include "memory.h"
#include "value.h"


void init_value_array(ValueArray* array) {
	array->count = 0;
	array->capacity = 0;
	array->values = NULL;
}

void free_value_array(ValueArray* array) {
	macro_free_array(Value, array->values, array->capacity);
	init_value_array(array);
}

void write_value_array(ValueArray* array, Value value) {
	if (array->capacity < array->count + 1) {
		int old_capacity = array->capacity;
		array->capacity = macro_grow_capacity(old_capacity);
		array->values = macro_grow_array(Value, array->values, old_capacity, array->capacity);
	}

	array->values[array->count] = value;
	array->count++;
}


void print_value(Value value) {
	printf("%g", value);
}

