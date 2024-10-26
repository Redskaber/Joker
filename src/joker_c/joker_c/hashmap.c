/*
* hashmap.c: hashmap implementation
*	- open addressing(开放地址法):
*   - separate chaining(拉链法):
*
* string interning(字符串驻留技术): 解决字符串比较消耗时间的问题.
*   - 字符串驻留技术本质上是一个数据去重的过程.(python{set}?)
*   - 字符串驻留技术的实现:
*      - 维护一个字符串表(string table), 保存所有字符串的哈希值和指针.
*      - 当遇到一个新的字符串时, 先计算它的哈希值, 然后在字符串表中查找是否有相同的哈希值.
*      - 如果有相同的哈希值, 则比较两个字符串是否相等, 如果相等, 则返回相同的指针.
*      - 如果没有相同的哈希值, 则将新的字符串添加到字符串表中, 返回它的指针.
*      - 字符串表的大小可以根据需要动态调整.
*
*/
#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "option.h"
#include "string.h"
#include "hashmap.h"

#define const_max_load_factor 0.75

static Entry* find_entry(Entry* entries, size_t capacity, String* key);
static void adjust_capacity(HashMap* map, size_t new_capacity);

bool is_empty_entry(Entry* entry) {
	return entry->key == NULL && macro_is_null(entry->value);
}

void init_hashmap(HashMap* self) {
	self->count = 0;
	self->capacity = 0;
	self->entries = NULL;
}

void free_hashmap(HashMap* self) {
	macro_free_array(Entry, self->entries, self->capacity);
	init_hashmap(self);
}

/* return bool? return find entry? */
bool hashmap_set(HashMap* self, String* key, Value value) {
	if (self->count + 1 > self->capacity * const_max_load_factor) {
		size_t new_capacity = macro_grow_capacity(self->capacity);
		adjust_capacity(self, new_capacity);
	}
	Entry* entry = find_entry(self->entries, self->capacity, key);
	bool is_new_entry = entry->key == NULL;
	if (is_new_entry && macro_is_null(entry->value)) self->count++; // new entry, count++
	entry->key = key;
	entry->value = value;
	return is_new_entry;
}

/*
* find_entry: find entry in entries by key.
*   - return Entry*: entry if found, else NULL.
*   - used string self hash % capacity to index entries.
*      - if entry->key == key, return entry.
*      - if entry->key != key, used open addressing to find next entry. [(index +1) % capacity].
*      - if find entry open addressing find next entry->key == NULL, open addressing over, return NULL.
*
* used motbstone to mark a deleted entry.
*   - note(log) it and keep going.
*
* how tombstones interact with the table’s load factor and resizing?
*   - combstone: we used conbstone to one entry, not decrease load factor.(avoid deap loop)
*/
static Entry* find_entry(Entry* entries, size_t capacity, String* key) {
	uint32_t index = key->hash % capacity;
	Entry* tombstone = NULL;

	while (true) {
		Entry* entry = &entries[index];
		// handler tombstone
		if (entry->key == NULL) {
			if (macro_is_null(entry->value)) {
				return tombstone != NULL ? tombstone : entry;
			}
			else {
				// tombstone: {key: null, value: true(bool)}
				if (tombstone == NULL) tombstone = entry;
			}
		}
		else if (entry->key == key) {
			return entry;
		}
		index = (index + 1) % capacity;
	} // dead loop?: no! bucause exist load factor, so have entry->key == NULL.
}

// allocate new entries and copy old entries to new entries
static void adjust_capacity(HashMap* self, size_t new_capacity) {
	Entry* new_entries = macro_allocate(Entry, new_capacity);
	// init new_entries
	for (int i = 0; i < new_capacity; i++) {
		new_entries[i].key = NULL;
		new_entries[i].value = macro_null_var;
	}
	// copy old entries to new entries + reset count(!combstone)
	self->count = 0;
	for (int i = 0; i < self->capacity; i++) {
		Entry* entry = &self->entries[i];
		if (entry->key != NULL) {
			Entry* new_entry = find_entry(new_entries, new_capacity, entry->key);
			new_entry->key = entry->key;
			new_entry->value = entry->value;
			self->count++;
		}
	}
	// free old entries
	macro_free_array(Entry, self->entries, self->capacity);
	// assign new entries to map
	self->entries = new_entries;
	self->capacity = new_capacity;
}

// add all entries from from_map to to_map
void hashmap_add_all(HashMap* from, HashMap* to) {
	for (int i = 0; i < from->capacity; i++) {
		Entry* entry = &from->entries[i];
		if (entry->key != NULL) {
			hashmap_set(to, entry->key, entry->value);
		}
	}
}
/* note: this function can return one block of memory, so use over it need to free it. */
Option(Value) hashmap_get(HashMap* self, String* key) {
	if (self->count == 0) return None;
	Entry* entry = find_entry(self->entries, self->capacity, key);
	return entry->key == NULL ? None : Some(entry->value);
}

/*
* note: used open addressing, so if set value to null is bad idea.used tombstone instead.
*    - tombstone: a special value to mark a deleted entry.
*    - when remove an entry, set its key to null and value to true(bool).
*/
bool hashmap_remove(HashMap* self, String* key) {
	if (self->count == 0) return false;
	Entry* entry = find_entry(self->entries, self->capacity, key);
	if (entry->key == NULL) return false;

	entry->key = NULL;
	entry->value = macro_bool_from_val(true);
	// map->count--;  // bucause, we use tombstone instead. tombstone count not need to decrease.
	return true;
}

/* note: this function used to find a key in hashmap, but not used to set value. */
String* hashmap_find_key(HashMap* self, const char* key, uint32_t len, uint32_t hash) {
	if (self->count == 0) return NULL;
	uint32_t index = hash % self->capacity;
	while (true) {
		Entry* entry = &self->entries[index];
		if (entry->key == NULL) {
			if (macro_is_null(entry->value)) return NULL; // non-combstone, return null.
		}
		else if (
			entry->key->length == len &&
			entry->key->hash == hash &&
			memcmp(entry->key->chars, key, len) == 0
			) {
			return entry->key; // found
		}
		index = (index + 1) % self->capacity;
	}
}

bool hashmap_contains_key(HashMap* self, String* key) {
	if (self->count == 0) return false;
	Entry* entry = find_entry(self->entries, self->capacity, key);
	return entry->key != NULL;
}

/* note: this function used to get entry in hashmap, if not found return new entry. */
Entry* hashmap_get_entry(HashMap* self, String* key) {
	if (self->count + 1 > self->capacity * const_max_load_factor) {
		size_t new_capacity = macro_grow_capacity(self->capacity);
		adjust_capacity(self, new_capacity);
	}
	return find_entry(self->entries, self->capacity, key);
}