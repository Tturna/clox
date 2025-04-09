#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "chunk.h"
#include "common.h"
#include "debug.h"
#include "vm.h"
#include "compiler.h"
#include "memory.h"
#include "object.h"
#include "value.h"

VM vm;

static Value clockNative(int argCount, Value* args) {
    return TO_NUM_VAL((double)clock() / CLOCKS_PER_SEC);
}

static void resetStack() {
    vm.stackTop = vm.stack;
    vm.frameCount = 0;
    vm.openUpvalues = NULL;
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

static void runtimeError(const char* format, ...) {
    va_list args; // declare variable for variadic arguments
    va_start(args, format); // initialize "args" with the arguments that come after "format"
    vfprintf(stderr, format, args); // print formatted message to stderr
    va_end(args); // clean up "args"
    fputs("\n", stderr); // ensure error message is followed by a new line

    for (int i = vm.frameCount - 1; i >= 0; i--) {
        CallFrame* frame = &vm.frames[i];
        ObjFunction* function = frame->closure->function;
        size_t instruction = frame->ip - function->chunk.code - 1;
        fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);

        if (function->name == NULL) {
            fprintf(stderr, "script\n");
        } else {
            fprintf(stderr, "%s()\n", function->name->chars);
        }
    }

    resetStack();
}

static void defineNative(const char* name, NativeFn function) {
    // Push and pop the name and the function because copyString and newNative dynamically
    // allocate memory, meaning the GC can potentially be triggered. Putting the values on
    // the stack makes sure the GC doesn't get rid of them prematurely(?)
    push(TO_OBJ_VAL((Obj*)copyString(name, (int)strlen(name))));
    push(TO_OBJ_VAL((Obj*)newNative(function)));
    tableSet(&vm.globals, AS_STRING_OBJ(vm.stack[0]), vm.stack[1]);
    pop();
    pop();
}

void initVM() {
    resetStack();
    vm.objects = NULL;
    vm.bytesAllocated = 0;
    vm.nextGC = 1024 * 1024;

    vm.grayCount = 0;
    vm.grayCapacity = 0;
    vm.grayStack = NULL;

    initTable(&vm.globals);
    initTable(&vm.strings);

    defineNative("clock", clockNative);
}

void freeVM() {
    freeTable(&vm.globals);
    freeTable(&vm.strings);
    freeObjects();
}

static Value peekStack(int distance) {
    return vm.stackTop[-1 - distance];
}

static bool call(ObjClosure* closure, int argCount) {
    if (argCount != closure->function->arity) {
        runtimeError("Expected %d arguments but got %d.", closure->function->arity, argCount);
        return false;
    }

    if (vm.frameCount == FRAMES_MAX) {
        runtimeError("Stack overflow.");
        return false;
    }

    CallFrame* frame = &vm.frames[vm.frameCount++];
    frame->closure = closure;
    frame->ip = closure->function->chunk.code;
    frame->slots = vm.stackTop - argCount - 1;
    return true;
}

static bool callValue(Value callee, int argCount) {
    if (IS_OBJ(callee)) {
        switch (GET_OBJ_TYPE(callee)) {
            case OBJ_BOUND_METHOD: {
                ObjBoundMethod* bound = AS_BOUND_METHOD(callee);
                return call(bound->method, argCount);
            }
            case OBJ_CLASS: {
                ObjClass* klass = AS_CLASS(callee);
                vm.stackTop[-argCount - 1] = TO_OBJ_VAL((Obj*)newInstance(klass));
                return true;
            }
            case OBJ_CLOSURE:
                return call(AS_CLOSURE(callee), argCount);
            case OBJ_NATIVE: {
                NativeFn native = AS_NATIVE(callee);
                Value result = native(argCount, vm.stackTop - argCount);
                vm.stackTop -= argCount + 1;
                push(result);
                return true;
            }
            default:
                break; // non-callable object type
        }
    }

    runtimeError("Can only call functions and classes.");
    return false;
}

static bool bindMethod(ObjClass* klass, ObjString* name) {
    Value method;

    if (!tableGet(&klass->methods, name, &method)) {
        runtimeError("Undefined property '%s'.", name->chars);
        return false;
    }

    ObjBoundMethod* bound = newBoundMethod(peekStack(0), AS_CLOSURE(method));
    pop();
    push(TO_OBJ_VAL((Obj*)bound));
    return true;
}

static ObjUpvalue* captureUpvalue(Value* local) {
    ObjUpvalue* prevUpvalue = NULL;
    ObjUpvalue* upvalue = vm.openUpvalues;

    // Ensure that closures share the same reference to a captured variable
    while (upvalue != NULL && upvalue->location > local) {
        prevUpvalue = upvalue;
        upvalue = upvalue->next;
    }

    if (upvalue != NULL && upvalue->location == local) {
        return upvalue;
    }

    ObjUpvalue* createdUpvalue = newUpvalue(local);
    createdUpvalue->next = upvalue;

    if (prevUpvalue == NULL) {
        vm.openUpvalues = createdUpvalue;
    } else {
        prevUpvalue->next = createdUpvalue;
    }

    return createdUpvalue;
}

static void closeUpvalues(Value* last) {
    while (vm.openUpvalues != NULL && vm.openUpvalues->location >= last) {
        ObjUpvalue* upvalue = vm.openUpvalues;
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        vm.openUpvalues = upvalue->next;
    }
}

static void defineMethod(ObjString* name) {
    Value method = peekStack(0);
    ObjClass* klass = AS_CLASS(peekStack(1));
    tableSet(&klass->methods, name, method);
    pop();
}

static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !FROM_BOOL_VAL(value));
}

static void concatenate() {
    ObjString* b = AS_STRING_OBJ(peekStack(0));
    ObjString* a = AS_STRING_OBJ(peekStack(1));

    int length = a->length + b->length;
    char* chars = ALLOCATE(char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    ObjString* result = takeString(chars, length);
    pop();
    pop();
    push(TO_OBJ_VAL((Obj*)result));
}

static InterpretResult run() {
    CallFrame* frame = &vm.frames[vm.frameCount - 1];

// Essentially just replace every instance of "READ_BYTE()" with "(*vm.ip++)".
// It dereferences vm.ip which is a pointer to the next instruction, then increments it.
#define READ_BYTE() (*frame->ip++)
#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])
// Takes the next two bytes from the chunk and builds a 16 bit uint from them.
#define READ_SHORT() \
    (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))
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
        disassembleInstruction(&frame->closure->function->chunk, (int)(frame->ip - frame->closure->function->chunk.code));
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
                push(frame->slots[slot]);
                break;
            }
            case OP_SET_LOCAL: {
                uint8_t slot = READ_BYTE();
                frame->slots[slot] = peekStack(0);
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
            case OP_GET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                push(*frame->closure->upvalues[slot]->location);
                break;
            }
            case OP_SET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                *frame->closure->upvalues[slot]->location = peekStack(0);
                break;
            }
            case OP_GET_PROPERTY: {
                if (!IS_INSTANCE(peekStack(0))) {
                    runtimeError("Only instances have properties");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjInstance* instance = AS_INSTANCE(peekStack(0));
                ObjString* name = READ_STRING();
                Value value;

                // Check if attempting to get a field
                if (tableGet(&instance->fields, name, &value)) {
                    pop(); // Instance
                    push(value); // Property value
                    break;
                }

                // Check if attempting to get a method
                if (!bindMethod(instance->klass, name)) {
                    return INTERPRET_RUNTIME_ERROR;
                }

                break;
            }
            case OP_SET_PROPERTY: {
                if (!IS_INSTANCE(peekStack(1))) {
                    runtimeError("Only instances have fields");
                    return INTERPRET_RUNTIME_ERROR;
                }
                ObjInstance* instance = AS_INSTANCE(peekStack(1));
                tableSet(&instance->fields, READ_STRING(), peekStack(0));
                // Do this funky stack juggle because I guess we want it so that
                // `print toast.jam = "grape"` prints "grape".
                Value value = pop();
                pop();
                push(value);
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
            case OP_JUMP: {
                uint16_t offset = READ_SHORT();
                frame->ip += offset;
                break;
            }
            case OP_JUMP_IF_FALSE: {
                uint16_t offset = READ_SHORT();
                if (isFalsey(peekStack(0))) frame->ip += offset;
                break;
            }
            case OP_LOOP: {
                uint16_t offset = READ_SHORT();
                frame->ip -= offset;
                break;
            }
            case OP_CALL: {
                int argCount = READ_BYTE();

                if (!callValue(peekStack(argCount), argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }

                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_CLOSURE: {
                ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
                ObjClosure* closure = newClosure(function);
                push(TO_OBJ_VAL((Obj*)closure));

                for (int i = 0; i < closure->upvalueCount; i++) {
                    uint8_t isLocal = READ_BYTE();
                    uint8_t index = READ_BYTE();

                    if (isLocal) {
                        closure->upvalues[i] = captureUpvalue(frame->slots + index);
                    } else {
                        closure->upvalues[i] = frame->closure->upvalues[index];
                    }
                }

                break;
            }
            case OP_CLOSE_UPVALUE:
                // The variable we are capturing is on top of the stack.
                // closeUpvalues() moves the local from the stack to the heap.
                closeUpvalues(vm.stackTop - 1);
                // remove local from the stack
                pop();
                break;
            case OP_RETURN: {
                // Pop the result of the function and store it
                Value result = pop();
                closeUpvalues(frame->slots);
                vm.frameCount--;

                // If no call frames remain, it means we returned from the top level implicit function(?)
                if (vm.frameCount == 0) {
                    // pop the function from the stack
                    pop();
                    return INTERPRET_OK;
                }

                // stuff from the function is not needed anymore so point the top of the stack
                // to whatever was before it(?)
                vm.stackTop = frame->slots;
                // push the stored return value to the stack
                push(result);
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_CLASS:
                push(TO_OBJ_VAL((Obj*)newClass(READ_STRING())));
                break;
            case OP_METHOD: {
                defineMethod(READ_STRING());
                break;
            }
        }
    }

#undef READ_BYTE
#undef READ_CONSTANT
#undef READ_SHORT
#undef READ_STRING
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
    ObjFunction* function = compile(source);

    if (function == NULL) return INTERPRET_COMPILE_ERROR;

    push(TO_OBJ_VAL((Obj*)function)); // again a GC thing to push the function only to pop it after
    ObjClosure* closure = newClosure(function);
    pop();
    push(TO_OBJ_VAL((Obj*)closure));
    call(closure, 0);

    return run();
}
