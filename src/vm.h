#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "table.h"
#include "value.h"
#include "object.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

// CallFrame represents a single on-going function call.
typedef struct {
    ObjFunction* function; // the function being called
    uint8_t* ip; // not really "return address" but used to jump after the function is done(?)
    Value* slots; // first slot in the VM's value stack that the function can use
} CallFrame;

typedef struct {
    // Function call stack
    CallFrame frames[FRAMES_MAX];
    int frameCount;

    Value stack[STACK_MAX]; // used to store temporary(?) values during expression evaluation
    Value* stackTop; // always points one element past the last item
    Table globals; // global variables
    Table strings; // interned strings
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
