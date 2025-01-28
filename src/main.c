#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "vm.h"

/*int main(int argc, const char* argv[]) {*/
/*    initVM();*/
/**/
/*    Chunk chunk;*/
/*    initChunk(&chunk);*/
/**/
/*    // calculate -((1.2 +  3.4) / 5.6)*/
/*    // The results of operations feed into the next operation because they all use*/
/*    // the value stack under the hood. Constants get added to the stack so addition*/
/*    // operations can just pop two and push the result. The following division again pops*/
/*    // two, getting the result of the previous addition naturally.*/
/*    int constantIndex = addConstant(&chunk, 1.2);*/
/*    writeChunk(&chunk, OP_CONSTANT, 123);*/
/*    writeChunk(&chunk, constantIndex, 123);*/
/**/
/*    constantIndex = addConstant(&chunk, 3.4);*/
/*    writeChunk(&chunk, OP_CONSTANT, 123);*/
/*    writeChunk(&chunk, constantIndex, 123);*/
/**/
/*    writeChunk(&chunk, OP_ADD, 123);*/
/**/
/*    constantIndex = addConstant(&chunk, 5.6);*/
/*    writeChunk(&chunk, OP_CONSTANT, 123);*/
/*    writeChunk(&chunk, constantIndex, 123);*/
/**/
/*    writeChunk(&chunk, OP_DIVIDE, 123);*/
/*    writeChunk(&chunk, OP_NEGATE, 123);*/
/**/
/*    // implicit cast because enum values are int types and the function takes*/
/*    // a type of uint8_t;*/
/*    writeChunk(&chunk, OP_RETURN, 123);*/
/**/
/*    disassembleChunk(&chunk, "test chunk");*/
/*    interpret(&chunk);*/
/*    freeVM();*/
/*    freeChunk(&chunk);*/
/**/
/*    return 0;*/
/*}*/

static void repl() {
    char line[1024];
    for (;;) {
        printf("> ");

        if (!fgets(line, sizeof(line), stdin)) {
            printf("\n");
            break;
        }

        interpret(line);
    }
}

static char* readFile(const char* path) {
    FILE* file = fopen(path, "rb");

    if (file == NULL) {
        fprintf(stderr, "Could not open file \"%s\".\n", path);
        exit(74);
    }

    // seek to the end of the file, see how many bytes we are from the start and rewind back to start.
    // classic trick to find the file size
    fseek(file, 0L, SEEK_END);
    size_t fileSize = ftell(file);
    rewind(file);

    char* buffer = (char*)malloc(fileSize + 1);

    if (buffer == NULL) {
        fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
        exit(74);
    }

    size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);

    if (bytesRead < fileSize) {
        fprintf(stderr, "Could not read file \"%s\".\n", path);
        exit(74);
    }

    buffer[bytesRead] = '\0';

    fclose(file);
    return buffer;
}

static void runFile(const char* path) {
    char* source = readFile(path);
    InterpretResult result = interpret(source);

    // free the source code string after interpretation and execution is done
    // so that tokens that reference their lexemes by pointers to this source string
    // don't point to nothing.
    free(source);

    if (result == INTERPRET_COMPILE_ERROR) exit(65);
    if (result == INTERPRET_RUNTIME_ERROR) exit(70);
}

int main(int argc, const char* argv[]) {
    initVM();

    if (argc == 1) {
        repl();
    } else if (argc == 2) {
        runFile(argv[1]);
    } else {
        fprintf(stderr, "Usage: clox [path]\n");
        exit(64);
    }

    freeVM();
    return 0;
}
