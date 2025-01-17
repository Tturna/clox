#include "chunk.h"
#include "debug.h"
#include "vm.h"

int main(int argc, const char* argv[]) {
    initVM();

    Chunk chunk;
    initChunk(&chunk);

    // calculate -((1.2 +  3.4) / 5.6)
    // The results of operations feed into the next operation because they all use
    // the value stack under the hood. Constants get added to the stack so addition
    // operations can just pop two and push the result. The following division again pops
    // two, getting the result of the previous addition naturally.
    int constantIndex = addConstant(&chunk, 1.2);
    writeChunk(&chunk, OP_CONSTANT, 123);
    writeChunk(&chunk, constantIndex, 123);

    constantIndex = addConstant(&chunk, 3.4);
    writeChunk(&chunk, OP_CONSTANT, 123);
    writeChunk(&chunk, constantIndex, 123);

    writeChunk(&chunk, OP_ADD, 123);

    constantIndex = addConstant(&chunk, 5.6);
    writeChunk(&chunk, OP_CONSTANT, 123);
    writeChunk(&chunk, constantIndex, 123);

    writeChunk(&chunk, OP_DIVIDE, 123);
    writeChunk(&chunk, OP_NEGATE, 123);

    // implicit cast because enum values are int types and the function takes
    // a type of uint8_t;
    writeChunk(&chunk, OP_RETURN, 123);

    disassembleChunk(&chunk, "test chunk");
    interpret(&chunk);
    freeVM();
    freeChunk(&chunk);

    return 0;
}
