#include "common.h"
#include "debug.h"
#include <stdio.h>
#include "vm.h"

VM vm;

static void resetStack() {
    vm.stackTop = vm.stack;
}

void initVM() {
    resetStack();
}

void freeVM() {
}

// The value stack doesn't actually remove any data. It just moves the stackTop
// pointer to indicate the size of the used stack. If stackTop == stack, the stack is "empty".
void push(Value value) {
    *vm.stackTop = value;
    vm.stackTop++;
}

Value pop() {
    vm.stackTop--;
    return *vm.stackTop;
}

static InterpretResult run() {
// Essentially just replace every instance of "READ_BYTE()" with "(*vm.ip++)".
// It dereferences vm.ip which is a pointer to the next instruction, then increment it.
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])

// do-while is used as a macro trick to include multiple statements while also
// supporting a trailing semicolon. Consider: if (condition) BINARY_OP(+);
// Without do-while, that would produce an error because of the trailing semicolon.
#define BINARY_OP(op) \
    do { \
        double b = pop(); \
        double a = pop(); \
        push(a op b); \
    } while (false)

    printf("\nRunning chunk...\n\n");
    for (;;) {

#ifdef DEBUG_TRACE_EXECUTION
        // print the whole value stack from bottom up
        printf("Value stack: ");
        for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
            printf("[ ");
            printValue(*slot);
            printf(" ] ");
        }
        printf("\n");
        printf("Running instruction...\n    ");

        // take the instruction pointer (ip) and make its location relative to
        // the current chunk's bytecode array, then disassemble instruction.
        disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
        printf("\n");
#endif

        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                push(constant);
                break;
            }
            case OP_ADD: BINARY_OP(+); break;
            case OP_SUBTRACT: BINARY_OP(-); break;
            case OP_MULTIPLY: BINARY_OP(*); break;
            case OP_DIVIDE: BINARY_OP(/); break;
            case OP_NEGATE:     push(-pop()); break;
            case OP_RETURN: {
                printValue(pop());
                printf("\n");
                return INTERPRET_OK;
            }
        }
    }

#undef READ_BYTE
#undef READ_CONSTANT
#undef BINARY_OP
}

InterpretResult interpret(Chunk* chunk) {
    vm.chunk = chunk;
    vm.ip = vm.chunk->code;
    return run();
}
