#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include "chunk.h"
#include "common.h"
#include "debug.h"
#include "vm.h"
#include "compiler.h"
#include "memory.h"
#include "object.h"
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
    vm.objects = NULL;
    initTable(&vm.globals);
    initTable(&vm.strings);
}

void freeVM() {
    freeTable(&vm.globals);
    freeTable(&vm.strings);
    freeObjects();
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

static void concatenate() {
    ObjString* b = AS_STRING_OBJ(pop());
    ObjString* a = AS_STRING_OBJ(pop());

    int length = a->length + b->length;
    char* chars = ALLOCATE(char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    ObjString* result = takeString(chars, length);
    push(TO_OBJ_VAL((Obj*)result));
}

static InterpretResult run() {
// Essentially just replace every instance of "READ_BYTE()" with "(*vm.ip++)".
// It dereferences vm.ip which is a pointer to the next instruction, then increment it.
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
#define READ_STRING() AS_STRING_OBJ(READ_CONSTANT())

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
            case OP_POP: pop(); break;
            case OP_GET_LOCAL: {
                uint8_t slot = READ_BYTE();
                push(vm.stack[slot]);
                break;
            }
            case OP_SET_LOCAL: {
                uint8_t slot = READ_BYTE();
                vm.stack[slot] = peekStack(0);
                break;
            }
            case OP_GET_GLOBAL: {
                ObjString* name = READ_STRING();
                Value value;

                if (!tableGet(&vm.globals, name, &value)) {
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }

                push(value);
                break;
            }
            case OP_DEFINE_GLOBAL: {
                ObjString* name = READ_STRING();
                tableSet(&vm.globals, name, peekStack(0));
                pop();
                break;
            }
            case OP_SET_GLOBAL: {
                ObjString* name = READ_STRING();

                // prevent implicit variable declaration
                if (tableSet(&vm.globals, name, peekStack(0))) {
                    tableDelete(&vm.globals, name);
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_EQUAL: {
               Value b = pop();
               Value a = pop();
               push(TO_BOOL_VAL(valuesEqual(a, b)));
               break;
            }
            case OP_GREATER: BINARY_OP(TO_BOOL_VAL, >); break;
            case OP_LESS: BINARY_OP(TO_BOOL_VAL, <); break;
            // notice how we pass a macro as an argument to a macro ;)
            case OP_ADD: {
                if (IS_STRING_OBJ(peekStack(0)) && IS_STRING_OBJ(peekStack(1))) {
                    concatenate();
                } else if (IS_NUMBER(peekStack(0)) && IS_NUMBER(peekStack(1))) {
                    double b = FROM_NUM_VAL(pop());
                    double a = FROM_NUM_VAL(pop());
                    push(TO_NUM_VAL(a + b));
                } else {
                    runtimeError("Operands must be two numbers or two strings.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
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
            case OP_PRINT: {
                printValue(pop());
                printf("\n");
                break;
            }
            case OP_RETURN: {
                // exit
                return INTERPRET_OK;
            }
        }
    }

#undef READ_BYTE
#undef READ_CONSTANT
#undef READ_STRING
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
