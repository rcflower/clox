#include <stdlib.h>

#include "compiler.h"
#include "memory.h"
#include "vm.h"
#include "chunk.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "debug.h"
#endif

#define GC_HEAP_GROW_FACTOR 2

void* reallocate(void* ptr, size_t oldSize, size_t newSize) {
	vm.bytesAllocated += newSize - oldSize;
	if (newSize > oldSize) {
#ifdef DEBUG_STRESS_GC
		collectGarbage();
#endif

		if (vm.bytesAllocated > vm.nextGC) {
			collectGarbage();
		}
	}

	if (newSize == 0) {
		free(ptr);
		return NULL;
	}

	void* result = realloc(ptr, newSize);
	if (result == NULL) exit(1);
	return result;
}

static void freeObject(Obj* obj) {
#ifdef DEBUG_LOG_GC
	printf("%p free type %d\n", (void*)obj, obj->type);
#endif
	switch (obj->type) {
		case OBJ_BOUND_METHOD: {
			FREE(ObjBoundMethod, obj);
			break;
		}
		case OBJ_CLASS: {
			ObjClass* klass = (ObjClass*)obj;
			freeTable(&klass->methods);
			FREE(ObjClass, obj);
			break;
		}
		case OBJ_INSTANCE: {
			ObjInstance* instance = (ObjInstance*)obj;
			freeTable(&instance->fields);
			FREE(ObjInstance, obj);
			break;
		}
		case OBJ_CLOSURE: {
			ObjClosure* closure = (ObjClosure*)obj;
			FREE_ARRAY(ObjUpvalue*, closure->upvalues, closure->upvalueCount);
			FREE(ObjClosure, obj);
			break;
		}
		case OBJ_FUNCTION: {
			ObjFunction* fun = (ObjFunction*)obj;
			freeChunk(&fun->chunk);
			FREE(ObjFunction, obj);
			break;
		} 
		case OBJ_NATIVE: {
			FREE(ObjNative, obj);
			break;
		}
		case OBJ_STRING: {
			ObjString* str = (ObjString*)obj;
			FREE_ARRAY(char, str->chars, str->length + 1);
			FREE(ObjString, obj);
			break;
		}
		case OBJ_UPVALUE: {
			FREE(ObjUpvalue, obj);
			break;
		}
	}
}

void markObj(Obj* obj) {
	if (obj == NULL) return;
	if (obj->isMarked) return;

#ifdef DEBUG_LOG_GC
	printf("%p mark ", (void*)obj);
	printValue(OBJ_VAL(obj));
	printf("\n");
#endif

	obj->isMarked = true;

	if (vm.grayCapacity < vm.grayCount + 1) {
		vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
		vm.grayStack = (Obj**)realloc(
			vm.grayStack, 
			sizeof(Obj*) * vm.grayCapacity
		);
		if (vm.grayStack == NULL) exit(1);
	}

	vm.grayStack[vm.grayCount++] = obj;
}

void markValue(Value value) {
	if (IS_OBJ(value)) markObj(AS_OBJ(value));
}

static void markArray(ValueArray* array) {
	for (int i=0; i<array->count; ++i) {
		markValue(array->values[i]);
	}
}

static void markRoots() {
	for (Value* slot=vm.stack; slot<vm.stackTop; ++slot) {
		markValue(*slot);
	}

	for (int i=0; i<vm.frameCount; ++i) {
		markObj((Obj*)vm.frames[i].closure);
	}

	for (ObjUpvalue* up=vm.openUpvalues; up!=NULL; up=up->next) {
		markObj((Obj*)up);
	}

	markTable(&vm.globals);
	markCompilerRoots();
	markObj((Obj*)vm.initString);
}

static void blackenObj(Obj* obj) {
#ifdef DEBUG_LOG_GC
	printf("%p blacken ", (void*)obj);
	printValue(OBJ_VAL(obj));
	printf("\n");
#endif
	switch(obj->type) {
		case OBJ_BOUND_METHOD: {
			ObjBoundMethod* bound = (ObjBoundMethod*)obj;
			markValue(bound->receiver);
			markObj((Obj*)bound->method);
			break;
		}
		case OBJ_CLASS: {
			ObjClass* klass = (ObjClass*)obj;
			markObj((Obj*)klass->name);
			markTable(&klass->methods);
			break;
		}
		case OBJ_INSTANCE: {
			ObjInstance* instance = (ObjInstance*)obj;
			markObj((Obj*)instance->klass);
			markTable(&instance->fields);
			break;
		}
		case OBJ_CLOSURE: {
			ObjClosure* closure = (ObjClosure*)obj;
			markObj((Obj*)closure->function);
			for (int i=0; i<closure->upvalueCount; ++i) {
				markObj((Obj*)closure->upvalues[i]);
			}
			break;
		}
		case OBJ_FUNCTION: {
			ObjFunction* fun = (ObjFunction*)obj;
			markObj((Obj*)fun->name);
			markArray(&fun->chunk.constants);
			break;
		}
		case OBJ_UPVALUE:
			markValue(((ObjUpvalue*)obj)->closed);
			break;
		case OBJ_NATIVE:
		case OBJ_STRING:
			break;
	}
}

static void traceReferences() {
	while (vm.grayCount > 0) {
		Obj* obj = vm.grayStack[--vm.grayCount];
		blackenObj(obj);
	}
}

static void sweep() {
	Obj** it = &vm.objects;
	while (*it != NULL) {
		if ((*it)->isMarked) {
			// reset and pass
			(*it)->isMarked = false;
			it = &(*it)->next;
		} else {
			// delete
			Obj* tmp = *it;
			*it = (*it)->next;
			freeObject(tmp);
		}
	}
}

void collectGarbage() {
#ifdef DEBUG_LOG_GC
	printf("-- gc begin\n");
	size_t before = vm.bytesAllocated;
#endif

	markRoots();
	traceReferences();
	tableRemoveWhite(&vm.strings);
	sweep();

	vm.nextGC = vm.bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
	printf("-- gc end\n");
	printf("    collected %zu bytes (from %zu to %zu) next at %zu\n",
		   before-vm.bytesAllocated, before, vm.bytesAllocated, vm.nextGC);
#endif
}

void freeObjects() {
	Obj* iter = vm.objects;
	while (iter != NULL) {
		Obj* next = iter->next;
		freeObject(iter);
		iter = next;
	}

	free(vm.grayStack);
}