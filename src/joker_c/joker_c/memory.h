#pragma once

#ifndef joker_memory_h
#define joker_memory_h

#include "common.h"

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
		0	   !0			Allocate new block.			分配新块
	   !0		0			Free allocation.			释放已分配内存
	   !0		< oldSize	Shrink existing allocation. 收缩已分配内存
	   !0		> oldSize	Grow existing allocation.	增加已分配内存
*/
void* reallocate(void* pointer, size_t old_size, size_t new_size);


/*
#ifndef joker_memory_worker
#define joker_memory_worker

#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>

#define HEAP_SIZE 1024 * 1024  // 1MB的堆内存

typedef struct Block {
    size_t size;       // 块的大小
    struct Block* next; // 指向下一个空闲块
    int is_free;       // 是否被释放
} Block;

static uint8_t private_heap[HEAP_SIZE]; // 私人堆内存
static Block* free_list = NULL; // 空闲块链表
static size_t used_size = 0; // 当前已使用的内存大小

// 初始化内存管理系统
void init_memory() {
    free_list = (Block*)private_heap; // 设置空闲列表的开头
    free_list->size = HEAP_SIZE - sizeof(Block);
    free_list->next = NULL;
    free_list->is_free = 1;
}

// 分配内存块
void* my_malloc(size_t size) {
    Block* curr = free_list;
    while (curr) {
        if (curr->is_free && curr->size >= size) {
            // 找到合适的块
            curr->is_free = 0; // 标记为已分配
            // 如果剩余空间足够，分割块
            if (curr->size >= size + sizeof(Block)) {
                Block* next_block = (Block*)((uint8_t*)curr + sizeof(Block) + size);
                next_block->size = curr->size - size - sizeof(Block);
                next_block->next = curr->next;
                next_block->is_free = 1;

                curr->size = size; // 调整当前块的大小
                curr->next = next_block; // 更新当前块的指向
            }
            return (void*)((uint8_t*)curr + sizeof(Block)); // 返回数据区地址
        }
        curr = curr->next; // 移动到下一个空闲块
    }
    return NULL; // 没有足够的内存
}

// 释放内存块
void my_free(void* ptr) {
    if (!ptr) return; // 空指针不处理

    Block* block = (Block*)((uint8_t*)ptr - sizeof(Block));
    block->is_free = 1; // 标记为已释放

    // 合并相邻的空闲块可以在这里实现（可选）
}

// reallocate 实现
void* reallocate(void* ptr, size_t new_size) {
    if (ptr == NULL) {
        return my_malloc(new_size); // 如果原指针为空，等效于 malloc
    }

    if (new_size == 0) {
        my_free(ptr); // 如果 new_size 为 0，等效于 free
        return NULL;
    }

    Block* old_block = (Block*)((uint8_t*)ptr - sizeof(Block));

    // 判断是否可以在原内存块中满足请求
    if (new_size <= old_block->size) {
        return ptr; // 直接返回原指针
    }
    else {
        // 需要新的内存块
        void* new_ptr = my_malloc(new_size);
        if (new_ptr) {
            // 复制数据
            memcpy(new_ptr, ptr, old_block->size);  // 复制旧数据
            my_free(ptr); // 释放旧的内存块
        }
        return new_ptr; // 返回新指针
    }
}

int main() {
    init_memory(); // 初始化内存管理
    // 示例用法
    int* arr = (int*)reallocate(NULL, 10 * sizeof(int));
    for (int i = 0; i < 10; i++) {
        arr[i] = i;
    }

    arr = (int*)reallocate(arr, 20 * sizeof(int)); // 扩展大小
    for (int i = 10; i < 20; i++) {
        arr[i] = i;
    }

    my_free(arr); // 释放内存

    return 0;
}


#endif / * joker_memory_worker * /
*/


#endif /* joker_memory_h */

