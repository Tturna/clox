#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "value.h"

#define STACK_MAX 256

typedef struct {
    Chunk* chunk;
    uint8_t* ip; // Instruction Pointer. Location of the next instruction to execute.
    Value stack[STACK_MAX]; // used to store temporary(?) values during expression evaluation
    Value* stackTop; // always points one element past the last item
    Obj* objects; // points to the head of the heap allocated object linked list
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

// "extern" exposes a variable to external files. "vm" can now be reference from a file that
// includes this file. Functions are external implicitly so no extern keyword is needed.
extern VM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);

#endif
