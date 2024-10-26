#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "error.h"

void* warning(const char* fmt, ...) {
	va_list args;
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	return NULL;
}

void* error(const char* fmt, ...) {
	va_list args;
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	exit(EXIT_FAILURE);
	return NULL;
}

void* panic(const char* fmt, ...) {
	va_list args;
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	abort();
	return NULL;
}