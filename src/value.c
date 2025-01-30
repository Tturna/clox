#include <stdio.h>
#include <string.h>
#include "memory.h"
#include "object.h"
#include "value.h"

void initValueArray(ValueArray* array) {
    array->values = NULL;
    array->capacity = 0;
    array->count = 0;
}

void writeValueArray(ValueArray* array, Value value) {
    if (array->capacity < array->count + 1) {
        int oldCapacity = array->capacity;
        array->capacity = GROW_CAPACITY(oldCapacity);
        array->values = GROW_ARRAY(Value, array->values, oldCapacity, array->capacity);
    }

    array->values[array->count] = value;
    array->count++;
}

void freeValueArray(ValueArray* array) {
    FREE_ARRAY(Value, array->values, array->capacity);
    initValueArray(array);
}

void printValue(Value value) {
    switch (value.type) {
        case VAL_BOOL:
            printf(FROM_BOOL_VAL(value) ? "true" : "false");
            break;
        case VAL_NIL:
            printf("nil");
            break;
        case VAL_NUMBER:
            // "%g" prints a number as an int, float, or scientific notation depending on which
            // makes the "most sense". It's basically a pretty way to print any number(?).
            printf("%g", FROM_NUM_VAL(value));
            break;
        case VAL_OBJ:
            printObject(value);
            break;
    }
}

bool valuesEqual(Value a, Value b) {
    if (a.type != b.type) return false;

    switch (a.type) {
        case VAL_BOOL: return FROM_BOOL_VAL(a) == FROM_BOOL_VAL(b);
        case VAL_NIL: return true;
        case VAL_NUMBER: return FROM_NUM_VAL(a) == FROM_NUM_VAL(b);
        case VAL_OBJ: {
            ObjString* aString = AS_STRING_OBJ(a);
            ObjString* bString = AS_STRING_OBJ(b);

            // compare string data so that "string" == "string" is truthy even though both
            // strings are technically separate objects on the heap
            return aString->length == bString->length &&
                   memcmp(aString->chars, bString->chars, aString->length) == 0;
        }
        default: return false; // unreachable
    }
}
