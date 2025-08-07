#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "value.h"
#include "memory.h"
#include "object.h"

// ===== main functions =====

bool value_equal(Value a, Value b) {
	if (IS_NUMBER(a) && IS_NUMBER(b)) {
		return AS_NUMBER(a) == AS_NUMBER(b);
	}
	return a == b;
}

void value_print(Value value) {
	if (IS_NIL(value)) {
		printf("nil");
	} else if (IS_BOOL(value)) {
		printf(AS_BOOL(value) ? "true" : "false");
	} else if (IS_NUMBER(value)) {
		printf("%g", AS_NUMBER(value));
	} else if (IS_OBJ(value)) {
		obj_print(value);
	}
}

// ===== Array methods =====

void array_init(Array* array) {
	array->count = 0;
	array->capacity = 0;
	array->values = NULL;
}

void array_free(Array* array) {
	FREE_ARRAY(Value, array->values, array->capacity);
	array_init(array);
}

void array_add(Array* array, Value value) {
	if (array->capacity < array->count+1) {
		int oldCapacity = array->capacity;
		array->capacity = GROW_CAPACITY(oldCapacity);
		array->values = (Value*)reallocate(
			array->values,
			sizeof(Value) * oldCapacity,
			sizeof(Value) * array->capacity
		);
	}

	array->values[array->count++] = value;
}











