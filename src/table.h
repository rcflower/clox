#ifndef clox_table_h
#define clox_table_h

#include "common.h"
#include "value.h"

typedef struct {
	ObjString* key;
	Value value;
} Entry;

typedef struct {
	int count;
	int capacity;
	Entry* entries;
} Table;

void table_init(Table* table);

void table_free(Table* table);

bool table_get(Table* table, ObjString* key, Value* value);	
// value = table[key]; Return false if key is not found.

bool table_set(Table* table, ObjString* key, Value value);
// table[key] = value; Return if key is new.

bool table_del(Table* table, ObjString* key);
// Return if key is in table.

void table_add_table(Table* from, Table* to);

ObjString* table_find_str(Table* table, const char* chars, int length, uint32_t hash);

void table_rm_white(Table* table);

void table_mark(Table* table);

#endif