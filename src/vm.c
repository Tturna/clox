#include <stdarg.h>
#include <stdio.h>
#include "common.h"
#include "debug.h"
#include "vm.h"
#include "compiler.h"
#include "value.h"

VM vm;

static void resetStack() {
    vm.stackTop = vm.stack;
}

static void runtimeError(const char* format, ...) {
    va_list args; // declare variable for variadic arguments
    va_start(args, format); // initialize "args" with the arguments that come after "format"
    vfprintf(stderr, format, args); // print formatted message to stderr
    va_end(args); // clean up "args"
    fputs("\n", stderr); // ensure error message is followed by a new line

    size_t instruction = vm.ip - vm.chunk->code - 1;
    int line = vm.chunk->lines[instruction];
    fprintf(stderr, "[line %d] in script\n", line);
    resetStack();
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

static Value peekStack(int distance) {
    return vm.stackTop[-1 - distance];
}

static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !FROM_BOOL_VAL(value));
}

static InterpretResult run() {
// Essentially just replace every instance of "READ_BYTE()" with "(*vm.ip++)".
// It dereferences vm.ip which is a pointer to the next instruction, then increment it.
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])

// do-while is used as a macro trick to include multiple statements while also
// supporting a trailing semicolon. Consider: if (condition) BINARY_OP(+);
// Without do-while, that would produce an error because of the trailing semicolon.
#define BINARY_OP(TO_X_VAL, op) \
    do { \
        if (!IS_NUMBER(peekStack(0)) || !IS_NUMBER(peekStack(1))) { \
            runtimeError("Operands must be numbers."); \
            return INTERPRET_RUNTIME_ERROR; \
        } \
        double b = FROM_NUM_VAL(pop()); \
        double a = FROM_NUM_VAL(pop()); \
        push(TO_X_VAL(a op b)); \
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
            case OP_NIL: push(TO_NIL_VAL); break;
            case OP_TRUE: push(TO_BOOL_VAL(true)); break;
            case OP_FALSE: push(TO_BOOL_VAL(false)); break;
            case OP_EQUAL: {
               Value b = pop();
               Value a = pop();
               push(TO_BOOL_VAL(valuesEqual(a, b)));
               break;
            }
            case OP_GREATER: BINARY_OP(TO_BOOL_VAL, >); break;
            case OP_LESS: BINARY_OP(TO_BOOL_VAL, <); break;
            // notice how we pass a macro as an argument to a macro ;)
            case OP_ADD: BINARY_OP(TO_NUM_VAL, +); break;
            case OP_SUBTRACT: BINARY_OP(TO_NUM_VAL, -); break;
            case OP_MULTIPLY: BINARY_OP(TO_NUM_VAL, *); break;
            case OP_DIVIDE: BINARY_OP(TO_NUM_VAL, /); break;
            case OP_NOT:
                push(TO_BOOL_VAL(isFalsey(pop())));
                break;
            case OP_NEGATE: {
                if (!IS_NUMBER(peekStack(0))) {
                    runtimeError("Operand must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                push(TO_NUM_VAL(-FROM_NUM_VAL(pop())));
                break;
            }
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

InterpretResult interpret(const char* source) {
    Chunk chunk;
    initChunk(&chunk);

    if (!compile(source, &chunk)) {
        freeChunk(&chunk);
        return INTERPRET_COMPILE_ERROR;
    }

    vm.chunk = &chunk;
    vm.ip = vm.chunk->code;

    InterpretResult result = run();

    freeChunk(&chunk);
    return result;
}
