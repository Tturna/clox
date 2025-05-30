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
    ObjClosure* closure; // the closure (which might just wrap a simple function) being called
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
    ObjString* initString;
    ObjUpvalue* openUpvalues; // head of linked list of upvalues still on the stack

    size_t bytesAllocated;
    size_t nextGC;

    Obj* objects; // points to the head of the heap allocated object linked list
    int grayCount; // For tracing objects for GC
    int grayCapacity; // -||-
    Obj** grayStack; // -||-
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
void push();
Value pop();

#endif
