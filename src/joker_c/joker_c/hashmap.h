#pragma once

#ifndef __joker_hashmap_h__
#define __joker_hashmap_h__

#include "common.h"
#include "value.h"
#include "option.h"



/* TODO: trait impl generic for HashMap */
typedef struct Entry {
	String* key;
	Value value;
} Entry;


typedef struct HashMap {
	size_t count;
	size_t capacity;
	Entry* entries;
} HashMap;

void init_hashmap(HashMap* self);
void free_hashmap(HashMap* self);
bool hashmap_set(HashMap* self, String* key, Value value);
void hashmap_add_all(HashMap* from, HashMap* to);
Option(Value) hashmap_get(HashMap* self, String* key);
bool hashmap_remove(HashMap* self, String* key);
String* hashmap_find_key(HashMap* self, const char* key, uint32_t len, uint32_t hash);



#endif /* __joker__hashmap_h__ */
