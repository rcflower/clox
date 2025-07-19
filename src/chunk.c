#include <stdlib.h>

#include "chunk.h"
#include "memory.h"
#include "vm.h"

void chunk_init(Chunk* chunk) {
	chunk->count = 0;
	chunk->capacity = 0;
	chunk->code = NULL;
	chunk->lines = NULL;
	array_init(&chunk->constants);
}

void chunk_free(Chunk* chunk) {
	reallocate(chunk->code, sizeof(uint8_t) * chunk->capacity);
	reallocate(chunk->lines, sizeof(int) * chunk->capacity);
	array_free(&chunk->constants);
	chunk_init(chunk);
}

void chunk_add_code(Chunk* chunk, uint8_t byte, int line) {
	if (chunk->capacity < chunk->count+1) {
		int oldCapacity = chunk->capacity;
		chunk->capacity = GROW_CAPACITY(oldCapacity);
		chunk->code = (uint8_t*)reallocate(
			chunk->code,
			sizeof(uint8_t) * oldCapacity,
			sizeof(uint8_t) * chunk->capacity)
		);
		chunk->lines = (int*)reallocate(
			chunk->lines,
			sizeof(int) * oldCapacity,
			sizeof(int) * chunk->capacity
		);
	}

	chunk->code[chunk->count] = byte;
	chunk->lines[chunk->count] = line;
	chunk->count++;
}

int chunk_add_const(Chunk* chunk, Value value) {
	push(value);
	array_add(&chunk->constants, value);
	pop();
	return chunk->constants.count - 1;
}



