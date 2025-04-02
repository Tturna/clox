#include <stdlib.h>
#include <string.h>
#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75

void initTable(Table *table) {
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void freeTable(Table *table) {
    FREE_ARRAY(Entry, table->entries, table->capacity);
    initTable(table);
}

static Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
    uint32_t index = key->hash % capacity;
    Entry* tombstone = NULL;

    for (;;) {
        Entry* entry = &entries[index];

        // This will never loop infinitely as long as TABLE_MAX_LOAD is < 1.
        // The table would have to be full of keys that don't match for this
        // to loop infinitely, but a TABLE_MAX_LOAD of < 1 ensures there will always
        // be empty buckets.
        //
        // The table could still fill up if we considered tombstones as empty buckets and therefore
        // decremented the count when we deleted items. That's why we don't decrement on deletion
        // so tombstones are considered full buckets.
        if (entry->key == NULL) {
            // either empty or tombstone bucket
            if (IS_NIL(entry->value)) {
                // empty bucket
                // If we passed a tombstone, return it so that insertion can overwrite it
                return tombstone != NULL ? tombstone : entry;
            } else {
                // tombstone bucket
                if (tombstone == NULL) tombstone = entry;
            }
        } else if (entry->key == key) {
            // found matching bucket
            return entry;
        }

        index = (index + 1) % capacity;
    }
}

bool tableGet(Table* table, ObjString* key, Value* value) {
    if (table->count == 0) return false;

    Entry* entry = findEntry(table->entries, table->capacity, key);

    if (entry->key == NULL) return false;

    *value = entry->value;
    return true;
}

static void adjustCapacity(Table* table, int capacity) {
    Entry* entries = ALLOCATE(Entry, capacity);

    for (int i = 0; i < capacity; i++) {
        entries[i].key = NULL;
        entries[i].value = TO_NIL_VAL;
    }

    // reset count so tombstones don't mess it up
    table->count = 0;

    // iterate over the old hash table and move the data from each populated
    // bucket to the new hash table array
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        
        // skip empty buckets and tombstones
        if (entry->key == NULL) continue;

        Entry* dest = findEntry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
        table->count++;
    }

    FREE_ARRAY(Entry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}

// return true if new entry was added
bool tableSet(Table *table, ObjString *key, Value value) {
    // the "load factor" of a hash table (or I guess any collection?) is "element count / capacity".
    // 0 = empty, 1 = full, 0.5 = half full/empty.
    // TABLE_MAX_LOAD indicates how full the table can be before it is grown. 0.75 = 75%
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
        int capacity = GROW_CAPACITY(table->capacity);
        adjustCapacity(table, capacity);
    }

    Entry* entry = findEntry(table->entries, table->capacity, key);
    bool isNewKey = entry->key == NULL;

    // tombstones have key = NULL and value = true.
    // If replacing a tombstone, don't increment count because it's already accounted for.
    // It's accounted for because deletion doesn't decrement (because we count tombstones
    // as full buckets to prevent infinite loops).
    if (isNewKey && IS_NIL(entry->value)) table->count++;

    entry->key = key;
    entry->value = value;
    return isNewKey;
}

bool tableDelete(Table* table, ObjString* key) {
    if (table->count == 0) return false;

    Entry* entry = findEntry(table->entries, table->capacity, key);

    if (entry->key == NULL) return false;

    // place tombstone
    // In clox, key = NULL, value = true represents a tombstone.
    // In reality, any representation that can't be confused with empty or valid populated
    // bucket works.
    entry->key = NULL;
    entry->value = TO_BOOL_VAL(true);
    return true;
}

// copy all values from one hash table to another, overwriting existing ones but keeping
// ones that weren't overwritten
void tableAddAll(Table *from, Table *to) {
    for (int i = 0; i < from->capacity; i++) {
        Entry* entry = &from->entries[i];

        if (entry->key != NULL) {
            tableSet(to, entry->key, entry->value);
        }
    }
}

// A version of findEntry() that doesn't just compare the key objects, but compares
// they key length, hash, and finally the contents of the strings so that we can intern
// all strings that have identical data.
ObjString* tableFindString(Table* table, const char* chars, int length, uint32_t hash) {
    if (table->count == 0) return NULL;

    uint32_t index = hash % table->capacity;

    for (;;) {
        Entry* entry = &table->entries[index];

        if (entry->key == NULL) {
            // stop if we find an empty non-tombstone entry
            if (IS_NIL(entry->value)) return NULL;
        }
        else if (entry->key->length == length &&
                 entry->key->hash == hash &&
                 memcmp(entry->key->chars, chars, length) == 0)
        {
            return entry->key;
        }

        index = (index + 1) % table->capacity;
    }
}

void markTable(Table* table) {
    for (int i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        markObject((Obj*)entry->key);
        markValue(entry->value);
    }
}
