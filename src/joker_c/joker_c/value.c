#include <stdio.h>


#include "memory.h"
#include "object.h"
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

bool values_equal(Value left, Value right) {
	if (left.type != right.type) {
		return false;
	}
	switch (left.type) {
	case VAL_NULL:	return true;
	case VAL_I32:	return macro_as_i32(left) == macro_as_i32(right);
	case VAL_F64:	return macro_as_f64(left) == macro_as_f64(right);
	case VAL_BOOL:	return macro_as_bool(left) == macro_as_bool(right);
	case VAL_OBJECT: return object_equal(macro_as_obj(left), macro_as_obj(right));
	default:		return false;
	}
}

void print_value(Value value) {
	switch (value.type) {
	case VAL_I32: printf("%d", value.as.i32); break;
	case VAL_F64: printf("%f", value.as.f64); break;
	case VAL_BOOL: printf("%s", value.as.boolean ? "true" : "false"); break;
	case VAL_NULL: printf("null"); break;
	case VAL_OBJECT: print_object(macro_as_obj(value)); break;
	default: printf("unknown"); break;
	}
}

void printf_value(Value value) {
	print_value(value);
	printf("\n");
}
