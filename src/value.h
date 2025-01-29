#ifndef clox_value_h
#define clox_value_h

#include "common.h"

typedef enum {
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
} ValueType;

typedef struct {
    ValueType type;
    union {
        bool boolean;
        double number;
    } as;
} Value;

#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NIL(value) ((value).type == VAL_NIL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)

#define FROM_BOOL_VAL(value) ((value).as.boolean)
#define FROM_NUM_VAL(value) ((value).as.number)

#define TO_BOOL_VAL(value) ((Value){VAL_BOOL, {.boolean = value}})
#define TO_NIL_VAL ((Value){VAL_NIL, {.number = 0}})
#define TO_NUM_VAL(value) ((Value){VAL_NUMBER, {.number = value}})

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
