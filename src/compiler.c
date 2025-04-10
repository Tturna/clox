#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "memory.h"
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
    bool isCaptured; // is the local captured by a closure?
} Local;

// Variables of outer scopes used in closures
typedef struct {
    uint8_t index; // "which local slot the upvalue is capturing"
    bool isLocal;
} Upvalue;

typedef enum {
    TYPE_FUNCTION,
    TYPE_INITIALIZER,
    TYPE_METHOD,
    TYPE_SCRIPT
} FunctionType;

typedef struct Compiler {
    struct Compiler* enclosing;
    // Store the current function we're compiling code for. In global scope, we use kind of an
    // implicit main() function. Global variables don't care about this though I guess.
    ObjFunction* function;
    FunctionType type; // so the compiler knows whether we are in top level code vs function body

    // Stack(?) of local variables in scope at any point during compilation.
    // We use a 1 byte instruction to declare(?) a local and another to indicate
    // The index of the local in a collection, meaning the limit of simultaneous locals
    // is 2^8 = 256.
    Local locals[UINT8_COUNT]; 
    int localCount; // how many locals are in scope at any time (element count in locals array)
    Upvalue upvalues[UINT8_COUNT];
    int scopeDepth; // "number of blocks surrounding the current bit of code being compiled"
} Compiler;

typedef struct ClassCompiler {
    struct ClassCompiler* enclosing;
} ClassCompiler;

Parser parser;
Compiler* current = NULL;
ClassCompiler* currentClass = NULL;

static Chunk* currentChunk() {
    return &current->function->chunk;
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

// This function is called "match()" in the book.
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

static void emitLoop(int loopStart) {
    emitByte(OP_LOOP);

    int offset = currentChunk()->count - loopStart + 2;

    if (offset > UINT16_MAX) error("Loop body too large.");

    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

static int emitJump(uint8_t instruction) {
    emitByte(instruction);
    // Emit a 2 byte placeholder operand for the jump offset.
    // Use backpatching to fill these placeholders later after we know the amount
    // of instructions to skip. This is done in patchJump().
    emitByte(0xff);
    emitByte(0xff);

    // count - 2 is the amount of instructions before the 2 placeholders, which also happens
    // to be the index of the first placeholder. Keep this in mind when you look at patchJump().
    return currentChunk()->count - 2;
}

static void emitReturn() {
    if (current->type == TYPE_INITIALIZER) {
        // For initializer methods, return the class instance.
        // This "loads slot 0, which contains the instance".
        emitBytes(OP_GET_LOCAL, 0);
    } else {
        emitByte(OP_NIL); // Somehow this makes it so functions *implicitly* return nil
    }

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

static void patchJump(int offset) {
    // At this point, count includes the instructions of the "then" block of the if statement.
    // offset is the index of the first jump length placeholder, which also happens to be the
    // amount of instructions that came before it (including the jump instruction).
    // Current instruction count - instructions before the placeholders - 2 placeholders
    // = the amount of instructions in the "then" block, assuming this function was called
    // right after that block was compiled.
    int jumpLength = currentChunk()->count - offset - 2;

    if (jumpLength > UINT16_MAX) {
        error("Too much code to jump over.");
    }

    // Instructions are 1 byte, but we want to store a 16 bit jump length indicator.
    // Take the first 8 bits of the value, shift them to be the last 8, and filter the result
    // so the first 8 bits are truly empty.
    // Filter the original value directly to only include the last 8 bits.
    // Effectively, this splits the 16 bit value into two 8 bit values.
    currentChunk()->code[offset] = (jumpLength >> 8) & 0xff;
    currentChunk()->code[offset + 1] = jumpLength & 0xff;
}

static void initCompiler(Compiler* compiler, FunctionType type) {
    compiler->enclosing = current;
    compiler->function = NULL; // assign null first because gc stuff(?)
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();
    current = compiler;

    if (type != TYPE_SCRIPT) {
        current->function->name = copyString(parser.previous.start, parser.previous.length);
    }

    // empty local in the first slot represents the implicit main function(?)
    Local* local = &current->locals[current->localCount++];
    local->depth = 0;
    local->isCaptured = false;

    // If method and not any old function(?)
    if (type != TYPE_FUNCTION) {
        local->name.start = "this";
        local->name.length = 4;
    } else {
        local->name.start = "";
        local->name.length = 0;
    }
}

static ObjFunction* endCompiler() {
    emitReturn();
    ObjFunction* function = current->function;

#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), function->name != NULL ? function->name->chars : "<script>");
    }
#endif

    current = current->enclosing;

    return function;
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
        if (current->locals[current->localCount - 1].isCaptured) {
            emitByte(OP_CLOSE_UPVALUE);
        } else {
            emitByte(OP_POP);
        }

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
static void and_(bool canAssign);
static void markInitialized();
static uint8_t argumentList();
static int resolveUpvalue(Compiler* compiler, Token* name);
static void declareLocalVariable();
static void namedVariable(Token name, bool canAssign);

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

static void call(bool canAssign) {
    uint8_t argCount = argumentList();
    emitBytes(OP_CALL, argCount);
}

static void dot(bool canAssign) {
    consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
    uint8_t name = identifierConstant(&parser.previous);

    if (canAssign && tryConsume(TOKEN_EQUAL)) {
        expression();
        emitBytes(OP_SET_PROPERTY, name);
    } else {
        emitBytes(OP_GET_PROPERTY, name);
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

static void function(FunctionType type) {
    Compiler compiler;
    initCompiler(&compiler, type);
    beginScope(); // scope doesn't need to be ended because we end the whole compiler

    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");

    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            current->function->arity++;

            if (current->function->arity > 255) {
                errorAtCurrent("Can't have more than 255 parameters.");
            }

            uint8_t constant = parseVariable("Expect parameter name.");
            defineVariable(constant);
        } while (tryConsume(TOKEN_COMMA));
    }

    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
    block();

    ObjFunction* endedFunction = endCompiler();

    // Use OP_CLOSURE to tell the VM to create a closure object that wraps the function object
    // at runtime
    emitBytes(OP_CLOSURE, makeConstant(TO_OBJ_VAL((Obj*)endedFunction)));

    // OP_CLOSURE has a variable amount of operands. For each upvalue, there are two operands.
    for (int i = 0; i < endedFunction->upvalueCount; i++) {
        emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(compiler.upvalues[i].index);
    }
}

static void method() {
    consume(TOKEN_IDENTIFIER, "Expect method name.");
    uint8_t constant = identifierConstant(&parser.previous);
    FunctionType type = TYPE_METHOD;

    // Check if compiling an initializer method. Used to return custom value ("this").
    if (parser.previous.length == 4 && memcmp(parser.previous.start, "init", 4) == 0) {
        type = TYPE_INITIALIZER;
    }

    function(type);

    emitBytes(OP_METHOD, constant);
}

static void classDeclaration() {
    consume(TOKEN_IDENTIFIER, "Expect class name.");
    Token className = parser.previous;
    uint8_t nameConstant = identifierConstant(&parser.previous);
    declareLocalVariable();

    emitBytes(OP_CLASS, nameConstant);
    defineVariable(nameConstant);

    ClassCompiler classCompiler;
    classCompiler.enclosing = currentClass;
    currentClass = &classCompiler;

    namedVariable(className, false);
    consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");

    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        method();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
    emitByte(OP_POP);

    currentClass = currentClass->enclosing;
}

static void funDeclaration() {
    uint8_t global = parseVariable("Expect function name.");
    markInitialized(); // mark variable immediately as initialized so functions can refer to themselves
    function(TYPE_FUNCTION); // compile params and body
    defineVariable(global);
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

// This shit is fucking confusing because there's a weird jumping dance that happens
// because in a traditional for loop, there is an increment clause that is textually before
// the loop body but it's expected to run after the body. This combined with the fact that
// this code exists to create the instructions for the loop logic will mess with your brain.
// You have to think about the instructions this creates and the order of them to understand
// how this works.
static void forStatement() {
    beginScope();
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");

    if (tryConsume(TOKEN_SEMICOLON)) {
        // no initializer
    } else if (tryConsume(TOKEN_VAR)) {
        varDeclaration();
    } else {
        expressionStatement();
    }

    // This variable indicates the point where the loop will jump after the loop body is done
    // executing. It will either be here in the condition evaluation portion of changed to be
    // in the increment portion if it exists.
    int loopStartOffset = currentChunk()->count;
    int exitJump = -1;
    
    if (!tryConsume(TOKEN_SEMICOLON)) {
        // Compile loop condition. This compiles the instructions that evaluate any variables
        // at runtime. That means if you jump to a point before this, any variables will be
        // pulled and evaluated again (by the same instructions). The variables are not evaluated
        // at compile time and that's why the condition can change and loops don't have to be infinite.
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // Emit a jump that skips the whole loop block if the condition evaluates to false
        exitJump = emitJump(OP_JUMP_IF_FALSE);

        // Pop the evaluated loop condition if it's true. This gets jumped over if it's false
        emitByte(OP_POP);
    }

    if (!tryConsume(TOKEN_RIGHT_PAREN)) {
        // If there is an increment block, start it with an instruction that skips itself.
        // Do this so that it doesn't get executed before the loop body. This could be done
        // differently if we didn't use a single-pass compiler.
        int preIncrementOffset = emitJump(OP_JUMP);

        // Record the point at which the increment block starts so the loop body can jump to it.
        int incrementOffset = currentChunk()->count;

        // Compile increment expression
        expression();

        // Pop the result of the expression because this increment block is only used
        // for its side effect
        emitByte(OP_POP);
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

        // Jump back to the loop condition evaluation portion that then jumps over this whole
        // increment portion to reach the loop body.
        emitLoop(loopStartOffset);

        // Change loop start point to be the increment start point. This makes it so if there is
        // no increment block, then after the loop body is done, it jumps back to the condition
        // evaluation portion that then naturally continues to the body again.
        // If there is an increment block, the loop body jumps to it and the increment portion
        // jumps to the condition evaluation, which naturally comes back to increment again, but
        // we have a jump that skips the increment after the condition check so it's all good.
        loopStartOffset = incrementOffset;

        // Backpatch the jump that skips the increment portion after the condition is evaluated.
        patchJump(preIncrementOffset);
    }

    // Compile loop body.
    statement();

    // Jump to condition evaluation if there is no increment block.
    // Jump to increment block if there is one. It will then jump to condition evaluation.
    emitLoop(loopStartOffset);

    // If there is a loop condition expression, patch the jump that skips the loop
    if (exitJump != -1) {
        patchJump(exitJump);

        // Pop the evaluated loop condition. At this point it's false and we jumped here
        // from the condition evaluation bit.
        emitByte(OP_POP);
    }

    endScope();
}

static void ifStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    // Capture the amount of instructions up to and including the if statement
    int ifStatementOffset = emitJump(OP_JUMP_IF_FALSE);

    // Pop the evaluated condition value from the stack. This is jumped over if the condition
    // is falsey.
    emitByte(OP_POP);
    statement(); // compile then-block

    // Capture the amount of instructions up to and including the else statement
    int elseStatementOffset = emitJump(OP_JUMP);

    // Patch the jump length for the then-block jump. The length is then-block +
    // the instructions to jump over the else-block if the condition was truthy.
    patchJump(ifStatementOffset);

    // Pop the condition value in the else-block if the condition was falsey.
    // Note: This makes it so technically there is an else-block even in if statements where
    // the user doesn't explicitly write one. In those cases, the only thing the else-block does
    // is pop the evaluated condition from the temp value stack.
    emitByte(OP_POP);

    // If there is an else-block, compile it.
    if (tryConsume(TOKEN_ELSE)) statement();

    // Patch the jump length for the else-block jump. The length is just else-block.
    patchJump(elseStatementOffset);
}

static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

static void returnStatement() {
    if (current->type == TYPE_SCRIPT) {
        error("Can't return from top-level code.");
    }

    if (tryConsume(TOKEN_SEMICOLON)) {
        emitReturn();
    } else {
        if (current->type == TYPE_INITIALIZER) {
            error("Can't return a value from an initializer.");
        }

        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
        emitByte(OP_RETURN);
    }
}

static void whileStatement() {
    // Record the point when the loop condition is evaluated.
    int preConditionOffset = currentChunk()->count;
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");

    // Compile the instructions to pull any variables and evaluate the loop condition
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    // Jump past the loop body if condition is false.
    int whileStatementOffset = emitJump(OP_JUMP_IF_FALSE);

    // Pop the evaluated condition expression value, which is true at this point.
    emitByte(OP_POP);
    statement();

    // Jump back to condition evaluation.
    emitLoop(preConditionOffset);

    // Jump here if the loop condition is falsey.
    patchJump(whileStatementOffset);

    // Pop the condition value from the temp value stack if condition was falsey.
    emitByte(OP_POP);
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
    if (tryConsume(TOKEN_CLASS)) {
        classDeclaration();
    } else if (tryConsume(TOKEN_FUN)) {
        funDeclaration();
    } else if (tryConsume(TOKEN_VAR)) {
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
    } else if (tryConsume(TOKEN_FOR)) {
        forStatement();
    } else if (tryConsume(TOKEN_IF)) {
        ifStatement();
    } else if (tryConsume(TOKEN_RETURN)) {
        returnStatement();
    } else if (tryConsume(TOKEN_WHILE)) {
        whileStatement();
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

// For the record, according to Crafting Interpreters (Robert Nystrom), this type of jump
// dance is inefficient and makes "or" slower than "and", which is a bit stupid.
static void or_(bool canAssign) {
    // At this point, the left side is already compiled and its value is on the stack.
    // If it's true, we need to skip the right hand side. That's done by jumping over it.
    // If it's false, we jump over the jump so the right hand gets compiled and evaluated.
    int wiggleJump = emitJump(OP_JUMP_IF_FALSE);
    int skipJump = emitJump(OP_JUMP);

    patchJump(wiggleJump);

    // Pop the left side if it's falsey and use the right side as the result.
    emitByte(OP_POP);
    parsePrecedence(PREC_OR);

    patchJump(skipJump);
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
    }
    // If variable is not local, check outer functions for an "upvalue"
    else if ((arg = resolveUpvalue(current, &name)) != -1) {
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
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

static void this_(bool canAssign) {
    if (currentClass == NULL) {
        error("Can't use 'this' outside of a class.");
        return;
    }

    variable(false);
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
    [TOKEN_LEFT_PAREN]    = {grouping, call,   PREC_CALL},
    [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE}, 
    [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_DOT]           = {NULL,     dot,    PREC_CALL},
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
    [TOKEN_AND]           = {NULL,     and_,   PREC_AND},
    [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
    [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
    [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
    [TOKEN_OR]            = {NULL,     or_,    PREC_OR},
    [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_THIS]          = {this_,    NULL,   PREC_NONE},
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

static int addUpvalue(Compiler* compiler, uint8_t index, bool isLocal) {
    int upvalueCount = compiler->function->upvalueCount;

    for (int i = 0; i < upvalueCount; i++) {
        Upvalue* upvalue = &compiler->upvalues[i];

        if (upvalue->index == index && upvalue->isLocal == isLocal) {
            return i;
        }
    }

    if (upvalueCount == UINT8_COUNT) {
        error("Too many closure variables in function.");
        return 0;
    }

    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;
    return compiler->function->upvalueCount++;
}

// Recursively find an upvalue. "On the way down", look for the upvalue.
// "On the way back up", capture the upvalue in each call/step.
static int resolveUpvalue(Compiler* compiler, Token* name) {
    // Check if we reached the outermost function
    if (compiler->enclosing == NULL) return -1;

    int local = resolveLocal(compiler->enclosing, name);

    if (local != -1) {
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(compiler, (uint8_t)local, true);
    }

    int upvalue = resolveUpvalue(compiler->enclosing, name);

    if (upvalue != -1) {
        return addUpvalue(compiler, (uint8_t)upvalue, false);
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
    local->isCaptured = false;
}

static void declareLocalVariable() {
    if (current->scopeDepth == 0) return;
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
    if (current->scopeDepth == 0) return;

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

static uint8_t argumentList() {
    uint8_t argCount = 0;

    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            expression();

            if (argCount == 255) {
                error("Can't have more than 255 arguments.");
            }

            argCount++;
        } while (tryConsume(TOKEN_COMMA));
    }

    consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

static void and_(bool canAssign) {
    // At this point, the left hand side of the and is compiled and on the stack.
    // Add a conditional jump to skip the right hand if left hand is false.
    int leftSideOffset = emitJump(OP_JUMP_IF_FALSE);

    // If left hand is true, pop it and use right hand as the result of the and operation.
    emitByte(OP_POP);
    parsePrecedence(PREC_AND);

    // Backpatch the length of the right hand side instructions so the short-circuit jump works.
    patchJump(leftSideOffset);
}

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

ObjFunction* compile(const char* source) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);

    parser.hadError = false;
    parser.panicMode = false;

    advance();

    while (!tryConsume(TOKEN_EOF)) {
        declaration();
    }

    ObjFunction* function = endCompiler();

    return parser.hadError ? NULL : function;
}

void markCompilerRoots() {
    Compiler* compiler = current;

    while (compiler != NULL) {
        markObject((Obj*)compiler->function);
        compiler = compiler->enclosing;
    }
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
