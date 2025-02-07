#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "chunk.h"
#include "value.h"

#define GET_OBJ_TYPE(value) (FROM_OBJ_VAL(value)->type) 

#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)
#define IS_STRING_OBJ(value) isObjType(value, OBJ_STRING)

#define AS_FUNCTION(value) ((ObjFunction*)FROM_OBJ_VAL(value))
#define AS_NATIVE(value) (((ObjNative*)FROM_OBJ_VAL(value))->function)
#define AS_STRING_OBJ(value) ((ObjString*)FROM_OBJ_VAL(value))
#define AS_CSTRING(value) (((ObjString*)FROM_OBJ_VAL(value))->chars)

// all heap allocated types
typedef enum {
    OBJ_FUNCTION,
    OBJ_NATIVE, // native function
    OBJ_STRING,
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
    struct Obj* next; // intrusive linked list implementation to track heap allocated objects
};

typedef struct {
    Obj obj; // Lox functions are first class
    int arity; // primARY (1 param), secondARY (2 params)...
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

ObjFunction* newFunction();
ObjNative* newNative(NativeFn function);
ObjString* takeString(char* chars, int length);
ObjString* copyString(const char* chars, int length);
void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && GET_OBJ_TYPE(value) == type;
}

#endif
