#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "value.h"

#define GET_OBJ_TYPE(value) (FROM_OBJ_VAL(value)->type) 
#define IS_STRING_OBJ(value) isObjType(value, OBJ_STRING)
#define AS_STRING_OBJ(value) ((ObjString*)FROM_OBJ_VAL(value))
#define AS_CSTRING(value) (((ObjString*)FROM_OBJ_VAL(value))->chars)

// all heap allocated types
typedef enum {
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

struct ObjString {
    Obj obj;
    int length;
    char* chars;
};

ObjString* takeString(char* chars, int length);
ObjString* copyString(const char* chars, int length);
void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
    return IS_OBJ(value) && GET_OBJ_TYPE(value) == type;
}

#endif
