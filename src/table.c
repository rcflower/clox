#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

/* 
* We implement a hash map: ObjString* (key) -> Value (value).
*
*	-- capacity is always a power of 2
*	-- Tombstone: (key, val) = (NULL, TRUE_VAL)
*/

#define TABLE_MAX_LOAD 0.75

// ===== local helper function declarations =====

static Entry* _find_entry(Entry* entries, int capacity, ObjString* key);
// Return the entry with given key if existed, otherwise return the first
// available entry (whose key is NULL) for such key,

static void _adjust_cap(Table* table, int newCap);
// Allocate a new table (really table->entries) with capacity = newCap 
// and copy the current table there.

// ===== main functions =====

void table_init(Table* table) {
	table->count = 0;
	table->capacity = 0;
	table->entries = NULL;
}

void table_free(Table* table) {
	FREE_ARRAY(Entry, table->entries, table->capacity);
	Table_init(table);
}

bool table_get(Table* table, ObjString* key, Value* value) {
	if (table->capacity == 0) return false;

	Entry* entry = _find_entry(table->entries, table->capacity, key);
	if (entry->key == NULL) return false;
	*value = entry->value;
	return true;
}

bool table_set(Table* table, ObjString* key, Value value) {
	if (table->count+1 > table->capacity * TABLE_MAX_LOAD) {
		int newCap = GROW_CAPACITY(table->capacity);
		_adjust_cap(table, newCap);
	}

	Entry* entry = _find_entry(table->entries, table->capacity, key);
	bool isNewKey = entry->key == NULL;
	if (isNewKey && IS_NIL(entry->value)) table->count++;  // not a tombstone

	entry->key = key;
	entry->value = value;
	return isNewKey;
}

bool table_del(Table* table, ObjString* key) {
	if (table->capacity == 0) return false;

	Entry* entry = _find_entry(table->entries, table->capacity, key);
	if (entry->key == NULL) return false;

	entry->key = NULL;
	entry->value = BOOL_VAL(true);		
	// place a tombstone so that open addressing is still valid

	return true;
}

void table_add_table(Table* from, Table* to) {
	for (int i=0; i<from->capacity; ++i) {
		Entry* entry = from->entries + i;
		if(entry->key != NULL) {
			tableSet(to, entry->key, entry->value);
		}
	}
}

ObjString* table_find_str(Table* table, const char* chars, int length, uint32_t hash) {
	if (table->count == 0) return NULL;

	HASH_T index = hash & (table->capacity-1);
	for (;;) {
		Entry* entry = table->entries + index;
		if (entry->key == NULL) {
			if (IS_NIL(entry->value)) return NULL;
		} else if (
			entry->key->length == length &&
			entry->key->hash == hash &&
			memcmp(entry->key->chars, chars, length) == 0
		) {
			return entry->key;
		}

		index = (index+1) & (table->capacity-1);
	}
}

void table_rm_white(Table* table) {
	for (int i=0; i<table->capacity; ++i) {
		Entry* entry = table->entries + i;
		if (entry->key != NULL && !entry->key->obj.isMarked) {
			tableDelete(table, entry->key);
		}
	}
}

void table_mark(Table* table) {
	for (int i=0; i<table->capacity; ++i) {
		Entry* entry = table->entries + i;
		markObj((Obj*)entry->key);
		markValue(entry->value);
	}
}


// ===== local helper functions =====

static Entry* _find_entry(Entry* entries, int capacity, ObjString* key) {
	HASH_T index = key->hash & (capacity-1);
	Entry* tombstone = NULL;

	for (;;) {
		Entry* entry = entries + index;
		if (entry->key == NULL) {

			// e.g. [k1] [tombstone] [k2] [NULL]
			// Our key might be k2, so we shouldn't just return the tombstone
			// If no key, return the first tombstone or [NULL]

			if (IS_NIL(entry->value)) {
				return tombstone != NULL ? tombstone : entry;	// return the tombstone
			} else {
				if (tombstone == NULL) tombstone = entry;		// set the first tombstone
			} 
		} else if (entry->key == key) {
			return entry;
		}
		index = (index+1) & (capacity-1);
	}
}

static void _adjust_cap(Table* table, int newCap) {
	Entry* entries = ALLOCATE(Entry, newCap);
	for (int i=0; i<newCap; ++i) {
		entries[i].key = NULL;
		entries[i].value = NIL_VAL;
	}

	// copy the old table to the new and modify their positions
	table->count = 0;
	for (int i=0; i<table->capacity; ++i) {
		Entry* entry = table->entries + i;
		if (entry->key == NULL) continue;

		Entry* dest = _find_entry(entries, newCap, entry->key);
		dest->key = entry->key;
		dest->value = entry->value;
		table->count++;
	}

	FREE_ARRAY(Entry, table->entries, table->capacity);
	table->entries = entries;
	table->capacity = newCap;
}







