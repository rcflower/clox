#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "value.h"
#include "table.h"
#include "object.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct {
	ObjClosure* closure;
	uint8_t* ip;				// return address pointing to caller's CallFrame
	Value* slots;				// points into vm.stack at the 1st slot this function can use
} CallFrame;

typedef struct {
	// Chunk* chunk;
	// uint8_t* ip;				// pointer to traverse/jump around in chunk->code
	CallFrame frames[FRAMES_MAX];
	int frameCount;

	Value stack[STACK_MAX];
	Value* stackTop;			// points to where the next value will be pushed

	Obj* objects;				// head of linked list of all objects
	ObjUpvalue* openUpvalues;
	Table globals;				// global variable: name -> value
	Table strings;				// hash table for all distinct strings
	ObjString* initString;

	size_t bytesAllocated;
	size_t nextGC;
	int grayCount;
	int grayCapacity;
	Obj** grayStack;
} VM;

typedef enum {
	INTERPRET_OK,
	INTERPRET_COMPILE_ERROR,
	INTERPRET_RUNTIME_ERROR
} InterpretResult;

extern VM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();

#endif



