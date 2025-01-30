#ifndef clox_value_h
#define clox_value_h

#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

typedef enum {
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
    VAL_OBJ, // all values whose actual data lives in the heap and not the stack is an "object"
} ValueType;

typedef struct {
    ValueType type;
    union {
        bool boolean;
        double number;
        Obj* obj;
    } as;
} Value;

#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NIL(value) ((value).type == VAL_NIL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)
#define IS_OBJ(value) ((value).type == VAL_OBJ)

#define FROM_BOOL_VAL(value) ((value).as.boolean)
#define FROM_NUM_VAL(value) ((value).as.number)
#define FROM_OBJ_VAL(value) ((value).as.obj)

#define TO_BOOL_VAL(value) ((Value){VAL_BOOL, {.boolean = value}})
#define TO_NIL_VAL ((Value){VAL_NIL, {.number = 0}})
#define TO_NUM_VAL(value) ((Value){VAL_NUMBER, {.number = value}})
#define TO_OBJ_VAL(value) ((Value){VAL_OBJ, {.obj = value}})

// "constant pool" or value array
typedef struct {
    int capacity;
    int count;
    Value* values;
} ValueArray;

bool valuesEqual(Value a, Value b);
void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);

#endif
