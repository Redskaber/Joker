#include "memory.h"

#include "option.h"


static __declspec(noreturn) void panic(const char* message) {
	printf("Panic: %s\n", message);
	exit(EXIT_FAILURE);
}



Option_* create_some(Value value) {
	Some_* some = macro_allocate(Some_, 1);
	some->base.state = SomeState;
	some->value = value;
	return (Option_*)some;
}

Option_* create_none() {
	Option_* option = macro_allocate(Option_, 1);
	option->state = NoneState;
	return option;
}

void free_option(Option_* option) {
	if (option != NULL) {
		switch (option->state)
		{
		case SomeState: macro_free(Some_, option); break;
		case NoneState: macro_free(Option_, option); break;
		default: panic("Expected Some or None state, Found invalid state."); break;
		}
	}
}

static void print_some(Some_* some) {
	printf("Some(");
	print_value(((Some_*)some)->value);
	printf(")");
}

void print_option(Option_* option) {
	if (option != NULL) {
		switch (option->state)
		{
		case SomeState: print_some((Some_*)option); break;
		case NoneState: printf("None"); break;
		default: panic("Expected Some or None state, Found invalid state."); break;
		}
	}
	else {
		panic("Expected Some or None state, Found option is NULL.");
	}
}

void printf_option(Option_* option) {
	print_option(option);
	printf("\n");
}
bool match(Option_* option, ValueType expected_type) {
	if (option->state == SomeState) {
		return ((Some_*)option)->value.type == expected_type;
	}
	return false;
}
