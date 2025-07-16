#include <stdio.h>
#include <string.h>

#include "common.h"
#include "memory.h"
#include "object.h"
#include "vm.h"
#include "table.h"

#define ALLOCATE_OBJ(type, objType) \
	(type*)allocateObj(sizeof(type), objType)

static Obj* allocateObj(size_t size, ObjType type) {
	Obj* obj = (Obj*)reallocate(NULL, 0, size);
	obj->type = type;
	obj->isMarked = false;

	obj->next = vm.objects;
	vm.objects = obj;			// insert obj as the new head

#ifdef DEBUG_LOG_GC
	printf("%p allocate %zu for %d\n", (void*)obj, size, type);
#endif

	return obj;
}

ObjBoundMethod* newBoundMethod(Value receiver, ObjClosure* method) {
	ObjBoundMethod* bound = ALLOCATE_OBJ(ObjBoundMethod, OBJ_BOUND_METHOD);
	bound->receiver = receiver;
	bound->method = method;
	return bound;
}

ObjClass* newClass(ObjString* name) {
	ObjClass* klass = ALLOCATE_OBJ(ObjClass, OBJ_CLASS);
	klass->name = name;
	initTable(&klass->methods);
	return klass;
}

ObjClosure* newClosure(ObjFunction* function) {
	ObjUpvalue** upvalues = ALLOCATE(ObjUpvalue*, function->upvalueCount);

	// for (int i=0; i<function->upvalueCount; ++i) {
	// 	upvalues[i] = NULL;
	// }
	memset(upvalues, 0, function->upvalueCount * sizeof(ObjUpvalue*));

	ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
	closure->function = function;
	closure->upvalues = upvalues;
	closure->upvalueCount = function->upvalueCount;
	return closure;
}

ObjFunction* newFunction() {
	ObjFunction* fun = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
	fun->arity = 0;
	fun->upvalueCount = 0;
	fun->name = NULL;
	initChunk(&fun->chunk);
	return fun;
}

ObjInstance* newInstance(ObjClass* klass) {
	ObjInstance* instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE);
	instance->klass = klass;
	initTable(&instance->fields);
	return instance;
}

ObjNative* newNative(NativeFn function) {
	ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
	native->function = function;
	return native;
}

ObjUpvalue* newUpvalue(Value* slot) {
	ObjUpvalue* up = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
	up->closed = NIL_VAL;
	up->slot = slot;
	up->next = NULL;
	return up;
}

static ObjString* allocateString(char* chars, int length, HASH_T hash) {
	ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
	string->length = length;
	string->chars = chars;
	string->hash = hash;

	push(OBJ_VAL(string));
	tableSet(&vm.strings, string, NIL_VAL);
	pop();

	return string;
}

static HASH_T hashString(const char* key, int length) {
	HASH_T hash = 2166136261u;
	for (int i=0; i<length; ++i) {
		hash ^= (uint8_t)key[i];
		hash *= 16777619;
	}
	return hash;
}

ObjString* copyString(const char* chars, int length) {
	HASH_T hash = hashString(chars, length);

	ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
	if (interned != NULL) return interned;

	char* heapChars = ALLOCATE(char, length+1);
	memcpy(heapChars, chars, length);
	heapChars[length] = '\0';
	return allocateString(heapChars, length, hash);
}

ObjString* takeString(char* chars, int length) {
	HASH_T hash = hashString(chars, length);
	ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
	if (interned != NULL) {
		FREE_ARRAY(char, chars, length+1);
		return interned;
	}
	return allocateString(chars, length, hash);
}

static void printFunction(ObjFunction* fun) {
	if (fun->name == NULL) {
		printf("<script>");
		return;
	}
	printf("<fn %s>", fun->name->chars);
}

void printObj(Value value) {
	switch (OBJ_TYPE(value)) {
		case OBJ_BOUND_METHOD:
			printf("bound: ");
			printFunction(AS_BOUND_METHOD(value)->method->function);
			break;
		case OBJ_CLASS:
			printf("<class %s>", AS_CLASS(value)->name->chars);
			break;
		case OBJ_INSTANCE:
			printf("%s instance", AS_INSTANCE(value)->klass->name->chars);
			break;
		case OBJ_CLOSURE:
			printFunction(AS_CLOSURE(value)->function);
			break;
		case OBJ_FUNCTION:
			printFunction(AS_FUNCTION(value));
			break;
		case OBJ_NATIVE:
			printf("<native fn>");
			break;
		case OBJ_STRING:
			printf("%s", AS_CSTRING(value));
			break;
		case OBJ_UPVALUE:
			printf("upvalue");
			break;
	}
}

