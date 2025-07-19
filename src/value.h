#ifndef clox_value_h
#define clox_value_h

#include <stdbool.h>
#include <string.h>

#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

#define QNAN		((uint64_t)0x7ffc000000000000) // quiet NaN: [50:62] bits are 1, the rest are 0.

#define MASK		((uint64_t)0xfffc000000000000)	// SIGN_BIT | QNAN

#define TAG_NIL		1			// 01
#define TAG_FALSE	2 		// 10
#define TAG_TRUE	3			// 11

typedef uint64_t Value;

// IS_... 	check type
#define IS_NIL(value)				( (value) == NIL_VAL )
#define IS_BOOL(value)			( ((value) | 1) == TRUE_VAL )
#define IS_NUMBER(value)		( ((value) & QNAN) != QNAN )
#define IS_OBJ(value)				( ((value) & MASK) == MASK )

// AS_... 	value -> data
#define AS_NUMBER(value)		value2Num(value)
#define AS_BOOL(value)			( (value) == TRUE_VAL ) 
#define AS_OBJ(value)				( (Obj*)(uintptr_t)((value) & ~MASK) )

// ..._VAL  data -> value
#define BOOL_VAL(b)				( (b) ? TRUE_VAL : FALSE_VAL )
#define FALSE_VAL					( (Value)(uint64_t)(QNAN | TAG_FALSE) )
#define TRUE_VAL					( (Value)(uint64_t)(QNAN | TAG_TRUE) )
#define NIL_VAL						( (Value)(uint64_t)(QNAN | TAG_NIL) )
#define NUMBER_VAL(num) 		num2Value(num)
#define OBJ_VAL(obj) 			( (Value)(MASK | (uint64_t)(uintptr_t)(obj)) )

static inline double value2Num(Value value) {
  double num;
  memcpy(&num, &value, sizeof(Value));
  return num;
}

static inline Value num2Value(double num) {
  Value value;
  memcpy(&value, &num, sizeof(double));
  return value;
}

// ===== vector<Value> dynamic array =====

typedef struct {
	int capacity;
	int count;
	Value* values;
} Array;

bool value_equal(Value a, Value b);
void value_print(Value value);

void array_init(ValueArray* array);
void array_free(ValueArray* array);
void array_add(ValueArray* array, Value value);

#endif