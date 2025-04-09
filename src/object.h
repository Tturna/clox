#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "chunk.h"
#include "value.h"
#include "table.h"

#define GET_OBJ_TYPE(value) (FROM_OBJ_VAL(value)->type) 

#define IS_BOUND_METHOD(value) isObjType(value, OBJ_BOUND_METHOD)
#define IS_CLASS(value) isObjType(value, OBJ_CLASS)
#define IS_CLOSURE(value) isObjType(value, OBJ_CLOSURE)
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_INSTANCE(value) isObjType(value, OBJ_INSTANCE)
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)
#define IS_STRING_OBJ(value) isObjType(value, OBJ_STRING)

#define AS_BOUND_METHOD(value) ((ObjBoundMethod*)FROM_OBJ_VAL(value))
#define AS_CLASS(value) ((ObjClass*)FROM_OBJ_VAL(value))
#define AS_CLOSURE(value) ((ObjClosure*)FROM_OBJ_VAL(value))
#define AS_FUNCTION(value) ((ObjFunction*)FROM_OBJ_VAL(value))
#define AS_INSTANCE(value) ((ObjInstance*)FROM_OBJ_VAL(value))
#define AS_NATIVE(value) (((ObjNative*)FROM_OBJ_VAL(value))->function)
#define AS_STRING_OBJ(value) ((ObjString*)FROM_OBJ_VAL(value))
#define AS_CSTRING(value) (((ObjString*)FROM_OBJ_VAL(value))->chars)

// all heap allocated types
typedef enum {
    OBJ_BOUND_METHOD,
    OBJ_CLASS,
    OBJ_CLOSURE,
    OBJ_FUNCTION,
    OBJ_INSTANCE,
    OBJ_NATIVE, // native function
    OBJ_STRING,
    OBJ_UPVALUE
} ObjType;

// In C, the fields of a struct are arranged in memory in the order they are declared.
// The fields of a struct inside a struct are expanded in place.
// A pointer to a struct also points to the location of the first field. With this, you can
// essentially implement inheritance.
//
// Below, ObjString has an Obj struct as its first field. In memory, this expands so that
// ObjString declares all the fields of Obj and then its own. A pointer to ObjString is also
// a pointer to the Obj field and vice versa.
struct Obj {
    ObjType type;
    bool isMarked; // For GC
    struct Obj* next; // intrusive linked list implementation to track heap allocated objects
};

typedef struct {
    Obj obj; // Lox functions are first class
    int arity; // primARY (1 param), secondARY (2 params)...
    int upvalueCount;
    Chunk chunk; // body
    ObjString* name;
} ObjFunction;

typedef Value (*NativeFn)(int argCount, Value* args);

typedef struct {
    Obj obj; // "inherit" Obj
    NativeFn function;
} ObjNative;

struct ObjString {
    Obj obj;
    int length;
    char* chars;
    uint32_t hash; // store the hash for a string here so the hash can be cached
};

// Use objects to store upvalues where the local variable has escaped "the stack".
typedef struct ObjUpvalue {
    Obj obj;
    Value* location; // "points to the closed-over variable"
    Value closed; // Value of the captured variable once it goes out of the stack.
    // Store "open upvalues" or upvalues still on the stack in a linked list.
    struct ObjUpvalue* next;
} ObjUpvalue;

// At compile time, we create an ObjFunction to represent a function. At runtime, that object
// is wrapped in an ObjClosure to hold info about the lexical scope.
typedef struct {
    Obj obj;
    ObjFunction* function;
    ObjUpvalue** upvalues;
    int upvalueCount; // redundant because ObjFunction also has this value but GC needs it
} ObjClosure;

typedef struct {
    Obj obj;
    ObjString* name;
    Table methods;
} ObjClass;

typedef struct {
    Obj obj;
    ObjClass* klass;
    Table fields;
} ObjInstance;

// Used to represent class methods where "this" is handled correctly.
typedef struct {
    Obj obj;
    Value receiver;
    ObjClosure* method;
} ObjBoundMethod;

ObjBoundMethod* newBoundMethod(Value receiver, ObjClosure* method);
ObjClass* newClass(ObjString* name);
ObjClosure* newClosure(ObjFunction* function);
ObjFunction* newFunction();
ObjInstance* newInstance(ObjClass* klass);
ObjNative* newNative(NativeFn function);
ObjString* takeString(char* chars, int length);
ObjString* copyString(const char* chars, int length);
ObjUpvalue* newUpvalue(Value* slot);
void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && GET_OBJ_TYPE(value) == type;
}

#endif
