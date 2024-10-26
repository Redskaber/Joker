#pragma once

#ifndef joker_memory_h
#define joker_memory_h

#include "common.h"

// macro for allocate array
#define macro_allocate(type, capacity) \
	(type*)reallocate(NULL, 0, sizeof(type) * (capacity))

// macro for free pointer
#define macro_free(type, pointer) \
    reallocate(pointer, sizeof(type), 0)

// macro for grow capacity
#define macro_grow_capacity(capacity) ((capacity) < 8 ? 8 : (capacity) * 2)
// macro for grow array
#define macro_grow_array(type, pointer, old_capacity, new_capacity) \
	(type*)reallocate(												\
		pointer,													\
		sizeof(type) *(old_capacity),								\
        sizeof(type) *(new_capacity)								\
	)
// macro for free array
#define macro_free_array(type, pointer, old_capacity)	\
    reallocate(											\
		pointer,										\
		sizeof(type) * (old_capacity),					\
		0												\
	)

/*
* reallocate memory
*
	oldSize	 new_size	 operator
		0	   !0			Allocate new block.			�����¿�
	   !0		0			Free allocation.			�ͷ��ѷ����ڴ�
	   !0		< oldSize	Shrink existing allocation. �����ѷ����ڴ�
	   !0		> oldSize	Grow existing allocation.	�����ѷ����ڴ�
*/
void* reallocate(void* pointer, size_t old_size, size_t new_size);

#endif /* joker_memory_h */
