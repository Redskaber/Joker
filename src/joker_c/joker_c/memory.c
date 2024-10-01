#include <stdlib.h>
#include <stdio.h>


#include "memory.h"

/*
* Reallocate memory for a pointer.
* If new_size is 0, the pointer is freed and NULL is returned.
* If realloc fails, an error message is printed and the program exits.
* Returns the new pointer.
*/
void* reallocate(void* pointer, size_t old_size, size_t new_size) {
	if (new_size == 0) {
		free(pointer);
		return NULL;
	}

	void* new_pointer = realloc(pointer, new_size);
	if (new_pointer == NULL) {
		fprintf(stderr, "Error: Failed to reallocate memory.\n");
		exit(EXIT_FAILURE);
	}

	return new_pointer;
}
