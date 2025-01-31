#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "scanner.h"
#include "value.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT, // =
    PREC_OR,         
    PREC_AND,
    PREC_EQUALITY,   // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM,       // + -
    PREC_FACTOR,     // * /
    PREC_UNARY,      // ! -
    PREC_CALL,       // . ()
    PREC_PRIMARY,
} Precedence;

// ParseFn represents a function that takes no arguments and returns nothing
typedef void (*ParseFn)(bool canAssign);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

typedef struct {
    Token name; // var name
    int depth; // scope depth of the block where var was declared
} Local;

typedef struct {
    // Stack(?) of local variables in scope at any point during compilation.
    // We use a 1 byte instruction to declare(?) a local and another to indicate
    // The index of the local in a collection, meaning the limit of simultaneous locals
    // is 2^8 = 256.
    Local locals[UINT8_COUNT]; 
    int localCount; // how many locals are in scope at any time (element count in locals array)
    int scopeDepth; // "number of blocks surrounding the current bit of code being compiled"
} Compiler;

Parser parser;
Compiler* current = NULL;
Chunk* compilingChunk;

static Chunk* currentChunk() {
    return compilingChunk;
}

static void errorAt(Token* token, const char* message) {
    if (parser.panicMode) return;
    parser.panicMode = true;

    fprintf(stderr, "[line %d] Error", token->line);

    if (token->line == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // nothing
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

static void error(const char* message) {
    errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
    errorAt(&parser.current, message);
}

static void advance() {
    parser.previous = parser.current;

    for (;;) {
        parser.current = scanToken();

        if (parser.current.type != TOKEN_ERROR) break;

        errorAtCurrent(parser.current.start);
    }
}

static bool check(TokenType type) {
    return parser.current.type == type;
}

static void consume(TokenType type, const char* message) {
    if (check(type)) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

static bool tryConsume(TokenType type) {
    bool matched = check(type);

    if (matched) advance();
    return matched;
}

static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static void emitReturn() {
    emitByte(OP_RETURN);
}

static uint8_t makeConstant(Value value) {
    int constantIndex = addConstant(currentChunk(), value);

    if (constantIndex > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constantIndex;
}

static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}

static void initCompiler(Compiler* compiler) {
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    current = compiler;
}

static void endCompiler() {
    emitReturn();

#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), "code");
    }
#endif
}

// This method just changes a variable that represents scope depth.
// In jlox, we created a whole new environment hash map for each scope.
static void beginScope() {
    current->scopeDepth++;
}

static void endScope() {
    current->scopeDepth--;

    // pop all locals from the ending scope out of the temp value stack
    while (current->localCount > 0 &&
           current->locals[current->localCount - 1].depth > current->scopeDepth)
    {
        emitByte(OP_POP);
        current->localCount--;
    }
}

static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);
static uint8_t identifierConstant(Token* name);
static int resolveLocal(Compiler* compiler, Token* name);
static uint8_t parseVariable(const char* errorMessage);
static void defineVariable(uint8_t global);

static void binary(bool canAssign) {
    TokenType operatorType = parser.previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence((Precedence)(rule->precedence + 1));

    switch (operatorType) {
        case TOKEN_BANG_EQUAL: emitBytes(OP_EQUAL, OP_NOT); break;
        case TOKEN_EQUAL_EQUAL: emitByte(OP_EQUAL); break;
        case TOKEN_GREATER: emitByte(OP_GREATER); break;
        case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break;
        case TOKEN_LESS: emitByte(OP_LESS); break;
        case TOKEN_LESS_EQUAL: emitBytes(OP_GREATER, OP_NOT); break;
        case TOKEN_PLUS: emitByte(OP_ADD); break;
        case TOKEN_MINUS: emitByte(OP_SUBTRACT); break;
        case TOKEN_STAR: emitByte(OP_MULTIPLY); break;
        case TOKEN_SLASH: emitByte(OP_DIVIDE); break;
        default: return; // unreachable
    }
}

static void literal(bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE: emitByte(OP_FALSE); break;
        case TOKEN_NIL: emitByte(OP_NIL); break;
        case TOKEN_TRUE: emitByte(OP_TRUE); break;
        default: return; // unreachable
    }
}

static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

static void block() {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void varDeclaration() {
    // add variable name (consume) to chunk's constant array and return index
    // unless it's a local, in which case add the var name to the compiler's
    // locals array (stack?) with its scope depth.
    uint8_t varIndex = parseVariable("Expect variable name.");

    if (tryConsume(TOKEN_EQUAL)) {
        expression();
    } else {
        // var a; is desugared to var a = nil;
        emitByte(OP_NIL);
    }

    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");
    defineVariable(varIndex);
}

static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(OP_POP);
}

static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

static void synchronize() {
    parser.panicMode = false;

    // Advance until we find something that looks like a statement boundary.
    // This is done by looking for a semicolon or something that starts a statement.
    // This effectively discards tokens until the erroneous statement is over.
    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) return;

        switch (parser.current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;
            default: ; // do nothing
        }

        advance();
    }
}

static void declaration() {
    if (tryConsume(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    // if a compilation error happens in the previous statement, start panicking and
    // synchronize.
    if (parser.panicMode) synchronize();
}

static void statement() {
    if (tryConsume(TOKEN_PRINT)) {
        printStatement();
    } else if (tryConsume(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

static void grouping(bool canAssign) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(bool canAssign) {
    double value = strtod(parser.previous.start, NULL); // convert string to double
    emitConstant(TO_NUM_VAL(value));
}

static void string(bool canAssign) {
    emitConstant(TO_OBJ_VAL((Obj*)copyString(parser.previous.start + 1,
                            parser.previous.length - 2)));
}

static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;

    // try to find a local variable with the given name. Use local operations if a var was
    // found, otherwise assume global.
    // If a local is found, arg is the index to the locals array in the compiler.
    // If it's not local, arg is the index of the value in the chunk's constants array.

    int arg = resolveLocal(current, &name); // try to get var index from locals array

    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else {
        arg = identifierConstant(&name); // get var index from constants array
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    // After evaluating an identifier, check if the next token is an equals sign.
    // If it is, this is a variable assignment and not a value lookup.
    // Use canAssign to check if assignment is permitted. Explanation in the caller <-
    if (canAssign && tryConsume(TOKEN_EQUAL)) {
        expression();
        emitBytes(setOp, (uint8_t)arg);
    } else {
        emitBytes(getOp, (uint8_t)arg);
    }
}

static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

static void unary(bool canAssign) {
    TokenType operatorType = parser.previous.type;

    // compile operand
    parsePrecedence(PREC_UNARY);

    // emit operator instruction
    switch (operatorType) {
        case TOKEN_BANG: emitByte(OP_NOT); break;
        case TOKEN_MINUS: emitByte(OP_NEGATE); break;
        default: return; // unreachable
    }
}

//  type                    prefix     infix   precedence
//                          function   function
ParseRule rules[] = {
    [TOKEN_LEFT_PAREN]    = {grouping, NULL,   PREC_NONE},
    [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE}, 
    [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_DOT]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
    [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
    [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
    [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
    [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
    [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
    [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
    [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
    [TOKEN_AND]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
    [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
    [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
    [TOKEN_OR]            = {NULL,     NULL,   PREC_NONE},
    [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_THIS]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
    [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};

// Vaughan Pratt's "top-down operator precedence parsing".
// Explanation attempt at the very end of the file ->
static void parsePrecedence(Precedence precedence) {
    advance();
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;

    if (prefixRule == NULL) {
        error("Expect expression.");
        return;
    }

    // canAssign is used to prevent weird variable assignment, like:
    // a * b = c + d;
    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(canAssign);

    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);

        // If assignment would be allowed and we evaluate a token, but the token leaves
        // an equals (=) token as the next token, then it either didn't consume it meaning
        // the token is not an identifier, or there are two consecutive equals tokens. In both
        // scenarios, it is an illegal assignment.
        if (canAssign && tryConsume(TOKEN_EQUAL)) {
            error("Invalid assignment target.");
        }
    }
}

static uint8_t identifierConstant(Token* name) {
    return makeConstant(TO_OBJ_VAL((Obj*)copyString(name->start, name->length)));
}

static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;

    return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler* compiler, Token* name) {
    // walk backwards through locals to find one with the given name. This ensures
    // variable shadowing works
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];

        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                error("Can't read local variable in its own initializer.");
            }

            return i;
        }
    }

    return -1;
}

static void addLocal(Token name) {
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }

    Local* local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1;
}

static void declareLocalVariable() {
    Token* name = &parser.previous;

    // Check if variable is already defined in current scope.
    for (int i = current->localCount - 1; i >= 0; i--) {
        Local* local = &current->locals[i];

        // If the newest variable is in a higher scope, break out because it means
        // this will be the first var in this scope
        if (local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error("Already a variable with this name in this scope.");
        }
    }

    addLocal(*name);
}

static uint8_t parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    // If we're in a block, return dummy index.
    // We stop here because the name of a local variable doesn't have to be stored in 
    // a chunk's constants array. It turns out with this setup, we can just leave the local
    // variable values on the temp value stack and store the var names in a locals array.
    if (current->scopeDepth > 0) {
        declareLocalVariable();
        return 0;
    }

    // if we're in global scope, create a global variable
    return identifierConstant(&parser.previous);
}

static void markInitialized() {
    current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void defineVariable(uint8_t global) {
    // if defining a local, mark it as usable (defined)
    if (current->scopeDepth > 0) {
        markInitialized();
        return;
    }

    emitBytes(OP_DEFINE_GLOBAL, global);
}

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

bool compile(const char *source, Chunk* chunk) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler);
    compilingChunk = chunk;

    parser.hadError = false;
    parser.panicMode = false;

    advance();

    while (!tryConsume(TOKEN_EOF)) {
        declaration();
    }

    endCompiler();

    return !parser.hadError;
}

// Vaughan Pratt parsing (in our context)...
// 
// You parse tokens left to right. You start with second highest precedence, assignment.
// Whatever token you encounter first, you run its function. If it's a literal, you just
// add it to an array of constants. Stuff like unary operators and variable declarations
// do their own thing. Whenever you move a step to the right, you compare the current
// precedence to the next token's precedence. If the next token has a higher precedence,
// you evaluate it's infix function. If the infix function needs to evaluate a right hand side,
// like in the case of a binary operation, you recursively call the parser with higher and higher
// precedence. This way the right hand side of the binary operation will stop evaluating if
// it encounters another of the same operation (left associativity). This recursive parser
// call takes steps to the next tokens, so when the evaluation stops and the call returns to
// the first parser call, it can now check the precedence of the next token again and continue
// evaluating the entire expression.

// Basically, you parse left to right and whenever you have an operation, you evaluate more to
// the right until you encounter something with lesser precedence. Then you apply the operation
// and continue evaluating. Every time you evaluate more to the right recursively, you either
// increment or decrement the precedence level, which determines the associativity(?).

// Here is my attempt at explaining how the parsing works here step by step.
//
// Consider we're parsing this expression: 3 * 4 - 8
//
// Call advance to put the first token into parser.current. current = NUM 3
// Call parsePrecedence with very low precedence (PREC_ASSIGNMENT).
// Call advance. previous = NUM 3, current = STAR
//
// Get prefix function for previous; number().
// Call number which puts previous (3) into the value array (VA). VA = [3]
// The index of the value is emitted as constant into the instruction chunk.
// PREC_ASSIGNMENT < precedence of STAR (FACTOR), so...
// Call advance. previous = STAR, current = NUM 4
//
// Get infix function for previous; binary().
// Call binary, where operator type is STAR
// Binary calls parsePrecedence with higher precedence (FACTOR + 1 = UNARY)
// Call advance. previous = NUM 4, current = MINUS
//
// Get prefix function for previous; number()
// Call number which puts previous (4) into the VA. VA = [3, 4]
// Value index emitted into chunk. Chunk: [CONST, 0, CONST, 1]
// PREC_UNARY is not <= precedence of MINUS, so we return to binary call
// Emit operator (STAR) into chunk. Chunk: [CONST, 0, CONST, 1, MULT]
// Return from binary back to while loop in parsePrecedence
// Precedence here is still PREC_ASSIGNMENT which is <= precedence of MINUS, so loop again...
// Call advance. previous = MINUS, current = NUM 8
//
// Get infix function for previous; binary()
// Call binary, where operator type is MINUS
// Binary calls parsePrecedence with higher precedence (TERM + 1 = FACTOR)
// Call advance. previous = NUM 8, current = EOF
//
// Get prefix function for previous; number()
// Call number which puts previous (8) into the VA. VA = [3, 4, 8]
// Value index emitted into chunk. Chunk: [CONST, 0, CONST, 1, MULT, CONST, 2]
// PREC_FACTOR is not <= precedence of EOF (NONE), se we return to binary call
// Emit operator (MINUS) into chunk. Chunk: [CONST, 0, CONST, 1, MULT, CONST, 2, SUBTR]
// Return from binary back to while loop in parsePrecedence
// Precedence here is still PREC_ASSIGNMENT which is not <= precedence of EOF, so return
// Compilation done
// Finish chunk with OP_RETURN instruction
//
// Chunk: [CONST, 0, CONST, 1, MULT, CONST, 2, SUBTR, RETURN]
// Value array: [3, 4, 8]
//
// Interpretation:
// CONST -> read next byte (0), use it as VA index and push to stack
// Stack: [3]
// CONST -> read next byte (1), use it as VA index and push to stack
// Stack: [3, 4]
// MULT -> pop twice and multiply values. push result to stack
// Stack: [12]
// CONST -> read next byte (2), use it as VA index and push to stack
// Stack: [12, 8]
// SUBTR -> pop twice and subtract values. push result to stack
// Stack: [4]
// RETURN -> pop once and print the value. prints 4
