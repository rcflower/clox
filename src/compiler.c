#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "compiler.h"
#include "memory.h"
#include "common.h"
#include "scanner.h"
#include "chunk.h"
#include "value.h"
#include "object.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
	Token current;
	Token previous;
	bool hadError;
	bool panicMode;
} Parser;

// precedence low to high
typedef enum {
	PREC_NONE,
	PREC_ASSIGNMENT,		// =
	PREC_OR,				// or
	PREC_AND,				// and
	PREC_EQUALITY,			// == !=
	PREC_COMPARISON, 		// < > <= >=
	PREC_TERM,				// + -
	PREC_FACTOR,			// * /
	PREC_UNARY,				// ! -
	PREC_CALL,				// . ()
	PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool canAssign);

typedef struct {
	ParseFn prefix;
	ParseFn infix;
	Precedence precedence;
} ParseRule;

typedef struct {
	Token name;
	int depth;
	bool isCaptured;
} Local;

typedef struct {
	uint8_t index;					// points to local slot of which enclosing function?
	bool isLocal;					// recursively look for enclosing until isLocal
} Upvalue;

typedef enum {
	TYPE_FUNCTION,
	TYPE_METHOD,
	TYPE_INITIALIZER,
	TYPE_SCRIPT						// top-level code inside an implicit "main()"
} FunctionType;

typedef struct Compiler {
	struct Compiler* enclosing;
	ObjFunction* function;
	FunctionType type;				// compiling top-level code or function body?
	Local locals[UINT8_COUNT];
	int localCount;
	int scopeDepth;
	Upvalue upvalues[UINT8_COUNT];
} Compiler;

typedef struct ClassCompiler {
	struct ClassCompiler* enclosing;
	bool hasSuperclass;
} ClassCompiler;

// ======================== Global variables ======================

/*
*	
*/

Parser parser;
Compiler* curCompiler = NULL;
ClassCompiler* curClass = NULL;

static Chunk* currentChunk() {
	return &curCompiler->function->chunk;		
}

// ======================== Declarations ======================

// ========================= Error handling ========================

static void errorAt(Token* token, const char* message) {
	if (parser.panicMode) return;
	parser.panicMode = true;
	fprintf(stderr, "[line %d] Error", token->line);

	if (token->type == TOKEN_EOF) {
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

// ========================== advance ==============================

static void advance() {
	parser.previous = parser.current;

	// only report the first error in the current statement, ignore the later ones.
	for (;;) {
		parser.current = scanToken();
		if (parser.current.type != TOKEN_ERROR) break;
		errorAtCurrent(parser.current.start);
	}
}

static void consume(TokenType type, const char* message) {
	if (parser.current.type == type) {
		advance();
		return;
	}

	errorAtCurrent(message);
}

static bool checkCurrent(TokenType type) {
	return parser.current.type == type;
}

static bool matchCurrent(TokenType type) {
	if (!checkCurrent(type)) return false;
	advance();
	return true;
}

// ========================= emit to chunk =============================

static void emitByte(uint8_t byte) {
	writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
	emitByte(byte1);
	emitByte(byte2);
}

// Add a constant to compilingChunk->constants and return its location.
static uint8_t makeConstant(Value value) {
	int index = addConstant(currentChunk(), value);
	if (index > UINT24_MAX) {
		error("Too many constants in one chunk.");
		return 0;
	}

	return (uint8_t)index;
}

static void emitConstant(Value value) {
	if (currentChunk()->count <= UINT8_COUNT) {
		emitBytes(OP_CONSTANT, makeConstant(value));
	} else {
		int p = makeConstant(value);
		emitBytes(OP_CONSTANT, (p>>16) & 0xff);
		emitBytes((p>>8) & 0xff, p & 0xff);
	}
}

static int emitJump(uint8_t instruction) {
	emitByte(instruction);
	emitByte(0xff);
	emitByte(0xff);

	// Leave placeholder for future patch. 
	// Return to the first byte of jump location

	return currentChunk()->count - 2;
}

static void patchJump(int offset) {
	// Go back to offset (the first byte of jump) 
	// 		and update the correct jump.
	// Imgaine now we are at the second byte of jump: offset+1, 
	// 		we should jump to currentChunk()->count-1.

	int jump = currentChunk()->count - offset - 2;
	if (jump > UINT16_MAX) {
		error("patchJump: Too much code to jump over.");
	}

	currentChunk()->code[offset] = (jump>>8) & 0xff;
	currentChunk()->code[offset+1] = jump & 0xff;
}

static void emitLoop(int loopStart) {
	emitByte(OP_LOOP);

	int offset = currentChunk()->count - loopStart + 2;
	if (offset > UINT16_MAX) {
		error("emitLoop: Loop body too large.");
	}
	emitBytes((offset>>8) & 0xff, offset & 0xff);
}

static void emitReturn() {
	if (curCompiler->type == TYPE_INITIALIZER) {
		emitBytes(OP_GET_LOCAL, 0);			// return the instance
	} else {
		emitByte(OP_NIL);
	}

	emitByte(OP_RETURN);
}

static void initCompiler(Compiler* compiler, FunctionType type) {
	compiler->enclosing = curCompiler;
	compiler->type = type;
	compiler->localCount = 0;
	compiler->scopeDepth = 0;
	compiler->function = newFunction();
	curCompiler = compiler;
	if (type != TYPE_SCRIPT) {
		curCompiler->function->name = copyString(parser.previous.start,
												 parser.previous.length);
	}

	Local* local = &curCompiler->locals[curCompiler->localCount++];
	local->depth = 0;
	local->isCaptured = false;
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
	ObjFunction* function = curCompiler->function;

#ifdef DEBUG_PRINT_CODE
	if (!parser.hadError) {
		disassembleChunk(
			currentChunk(), 
			function->name!=NULL ? function->name->chars : "<script>"
		);
	}
#endif

	curCompiler = curCompiler->enclosing;
	return function;
}

// ========================= Parsing declaration ===========================

static void parsePrecedence(Precedence);
static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType);
static uint8_t identifierConstant(Token*);
static int resolveLocal(Compiler*, Token*);
static void varDeclaration();

// ============================== Operations ==============================


// For all the operations below, the operator has been consumed and 
// stored in parser.previous.

static void grouping(bool canAssign) {
	expression();
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(bool canAssign) {
	double value = strtod(parser.previous.start, NULL);
	emitConstant(NUMBER_VAL(value));
}

static void unary(bool canAssign) {
	TokenType opType = parser.previous.type;

	// Compile the operand, the number to be negated.
	parsePrecedence(PREC_UNARY);

	switch(opType) {
		case TOKEN_MINUS:	emitByte(OP_NEGATE); break;
		case TOKEN_BANG:	emitByte(OP_NOT); break;
		default: return;
	}
}

static uint8_t argumentList() {
	uint8_t argCount = 0;
	if (!checkCurrent(TOKEN_RIGHT_PAREN)) {
		do {
			expression();
			if (argCount == 255) {
				error("Can't have more than 255 arguments.");
			}
			argCount++;
		} while (matchCurrent(TOKEN_COMMA));
	}
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
	return argCount;
}

static void call(bool canAssign) {
	uint8_t argCount = argumentList();
	emitBytes(OP_CALL, argCount);
}

static void dot(bool canAssign) {
	consume(TOKEN_IDENTIFIER, "Expect property name after '.'");
	uint8_t name = identifierConstant(&parser.previous);

	if (canAssign && matchCurrent(TOKEN_EQUAL)) {
		expression();
		emitBytes(OP_SET_PROPERTY, name);
	} else if (matchCurrent(TOKEN_LEFT_PAREN)) {
		uint8_t argCount = argumentList();
		emitBytes(OP_INVOKE, name);
		emitByte(argCount);
	} else {
		emitBytes(OP_GET_PROPERTY, name);
	}
}

static void binary(bool canAssign) {
	TokenType opType = parser.previous.type;
	ParseRule* rule = getRule(opType);
	parsePrecedence((Precedence)(rule->precedence + 1));

	switch (opType) {
		case TOKEN_BANG_EQUAL:		emitBytes(OP_EQUAL, OP_NOT); break;
		case TOKEN_EQUAL_EQUAL:		emitByte(OP_EQUAL); break;
		case TOKEN_GREATER:			emitByte(OP_GREATER); break;
		case TOKEN_GREATER_EQUAL:	emitBytes(OP_LESS, OP_NOT); break;
		case TOKEN_LESS:			emitByte(OP_LESS); break;
		case TOKEN_LESS_EQUAL:		emitBytes(OP_GREATER, OP_NOT); break;
		case TOKEN_PLUS:			emitByte(OP_ADD); break;
		case TOKEN_MINUS:			emitByte(OP_SUBTRACT); break;
		case TOKEN_STAR:			emitByte(OP_MULTIPLY); break;
		case TOKEN_SLASH:			emitByte(OP_DIVIDE); break;
		default: return;
	}
}

static void literal(bool canAssign) {
	switch (parser.previous.type) {
		case TOKEN_NIL:		emitByte(OP_NIL); break;
		case TOKEN_TRUE:	emitByte(OP_TRUE); break;
		case TOKEN_FALSE:	emitByte(OP_FALSE); break;
		default: return;
	}
}

static void string(bool canAssign) {
	emitConstant(
		OBJ_VAL(copyString(parser.previous.start+1, parser.previous.length-2))
	);
}

static void and_(bool canAssign) {
	int endJump = emitJump(OP_JUMP_IF_FALSE);

	emitByte(OP_POP);
	parsePrecedence(PREC_AND);
	patchJump(endJump);
}

static void or_ (bool canAssign) {
	int falseJump = emitJump(OP_JUMP_IF_FALSE);
	int trueJump = emitJump(OP_JUMP);

	patchJump(falseJump);
	emitByte(OP_POP);

	parsePrecedence(PREC_OR);
	patchJump(trueJump);
}

static bool identifiersEqual(Token* a, Token* b) {
	if (a->length != b->length) return false;
	return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler* compiler, Token* name) {
	for (int i=compiler->localCount-1; i>=0; --i) {
		Local* local = compiler->locals + i;
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
	// returns index in compiler->upvalues

	int cnt = compiler->function->upvalueCount;			// why not storing the count and upvalues in the same place??
	
	if (cnt == UINT8_COUNT) {
		error("Too many closure variables in function.");
		return 0;
	}

	for (int i=0; i<cnt; ++i) {
		Upvalue* up = &compiler->upvalues[i];
		if (up->index == index && up->isLocal == isLocal) {
			return i;
		}
	}	

	compiler->upvalues[cnt].index = index;
	compiler->upvalues[cnt].isLocal = isLocal;

	return compiler->function->upvalueCount++;
}

static int resolveUpvalue(Compiler* compiler, Token* name) {
	if (compiler->enclosing == NULL) return -1; 	// script

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

static void namedVariable(Token name, bool canAssign) {
	// Set or get a variable but not define.

	uint8_t getOp, setOp;
	int arg = resolveLocal(curCompiler, &name);
	if (arg != -1) {
		// local variable.
		getOp = OP_GET_LOCAL;
		setOp = OP_SET_LOCAL;
	} else if ((arg = resolveUpvalue(curCompiler, &name)) != -1) {
		// upvalue in the surrounding/enclosing closure
		getOp = OP_GET_UPVALUE;
		setOp = OP_SET_UPVALUE;
	}
	else {
		// global variable.
		arg = identifierConstant(&name);
		getOp = OP_GET_GLOBAL;
		setOp = OP_SET_GLOBAL;
	}

	if (canAssign && matchCurrent(TOKEN_EQUAL)) {
		expression();
		emitBytes(setOp, (uint8_t)arg);
	} else {
		emitBytes(getOp, (uint8_t)arg);
	}
}

static void variable(bool canAssign) {
	namedVariable(parser.previous, canAssign);
}

static Token syntheticToken(const char* name) {
	Token token;
	token.start = name;
	token.length = (int)strlen(name);
	return token;
}

static void this_(bool canAssign) {
	if (curClass == NULL) {
		error("Can't use 'this' outside of a class.");
		return;
	}

	variable(false);
}

static void super_(bool canAssign) {
	if (curClass == NULL) {
		error("Can't use 'super' outside of a class.");
	} else if (!curClass->hasSuperclass) {
		error("Can't use 'super' in a class with no superclass.");
	}
	consume(TOKEN_DOT, "Expect '.' after 'super'.");
	consume(TOKEN_IDENTIFIER, "Expect superclass method name");
	uint8_t name = identifierConstant(&parser.previous);

	namedVariable(syntheticToken("this"), false);

	if (matchCurrent(TOKEN_LEFT_PAREN))	{
		uint8_t argCount = argumentList();
		namedVariable(syntheticToken("super"), false);
		emitBytes(OP_SUPER_INVOKE, name);
		emitByte(argCount);
	} else {
		namedVariable(syntheticToken("super"), false);
		emitBytes(OP_GET_SUPER, name);
	}
}

ParseRule rules[] = {
	[TOKEN_LEFT_PAREN] 		= {grouping, call, 	 PREC_CALL},
	[TOKEN_RIGHT_PAREN]		= {NULL,	 NULL,	 PREC_NONE},
	[TOKEN_LEFT_BRACE]		= {NULL,	 NULL,	 PREC_NONE}, 
	[TOKEN_RIGHT_BRACE]		= {NULL,	 NULL,	 PREC_NONE},
	[TOKEN_COMMA]			= {NULL,	 NULL,	 PREC_NONE},
	[TOKEN_DOT]				= {NULL,	 dot,	 PREC_CALL},
	[TOKEN_MINUS]			= {unary,	 binary, PREC_TERM},
	[TOKEN_PLUS]			= {NULL,	 binary, PREC_TERM},
	[TOKEN_SEMICOLON]		= {NULL, 	 NULL, 	 PREC_NONE},
	[TOKEN_SLASH]			= {NULL,	 binary, PREC_FACTOR},
	[TOKEN_STAR]			= {NULL,	 binary, PREC_FACTOR},
	[TOKEN_BANG] 			= {unary, 	 NULL, 	 PREC_NONE},
	[TOKEN_BANG_EQUAL] 		= {NULL, 	 binary, PREC_EQUALITY},
	[TOKEN_EQUAL] 			= {NULL, 	 NULL, 	 PREC_NONE},
	[TOKEN_EQUAL_EQUAL] 	= {NULL, 	 binary, PREC_EQUALITY},
	[TOKEN_GREATER] 		= {NULL, 	 binary, PREC_COMPARISON},
	[TOKEN_GREATER_EQUAL] 	= {NULL, 	 binary, PREC_COMPARISON},
	[TOKEN_LESS] 			= {NULL, 	 binary, PREC_COMPARISON},
	[TOKEN_LESS_EQUAL] 		= {NULL, 	 binary, PREC_COMPARISON},
	[TOKEN_IDENTIFIER] 		= {variable, NULL, 	 PREC_NONE},
	[TOKEN_STRING] 			= {string, 	 NULL, 	 PREC_NONE},
	[TOKEN_NUMBER] 			= {number, 	 NULL, 	 PREC_NONE},
	[TOKEN_AND] 			= {NULL, 	 and_, 	 PREC_NONE},
	[TOKEN_CLASS] 			= {NULL, 	 NULL, 	 PREC_NONE},
	[TOKEN_ELSE] 			= {NULL, 	 NULL, 	 PREC_NONE},
	[TOKEN_FALSE]			= {literal,	 NULL, 	 PREC_NONE},
	[TOKEN_FOR] 			= {NULL, 	 NULL, 	 PREC_NONE},
	[TOKEN_FUN] 			= {NULL, 	 NULL, 	 PREC_NONE},
	[TOKEN_IF] 				= {NULL, 	 NULL,   PREC_NONE},
	[TOKEN_NIL] 			= {literal,  NULL,   PREC_NONE},
	[TOKEN_OR] 				= {NULL, 	 or_,    PREC_NONE},
	[TOKEN_PRINT] 			= {NULL, 	 NULL, 	 PREC_NONE},
	[TOKEN_RETURN] 			= {NULL, 	 NULL, 	 PREC_NONE},
	[TOKEN_SUPER] 			= {super_, 	 NULL,   PREC_NONE},
	[TOKEN_THIS] 			= {this_, 	 NULL,   PREC_NONE},
	[TOKEN_TRUE] 			= {literal,  NULL,   PREC_NONE},
	[TOKEN_VAR] 			= {NULL, 	 NULL,   PREC_NONE},
	[TOKEN_WHILE] 			= {NULL, 	 NULL,   PREC_NONE},
	[TOKEN_ERROR] 			= {NULL, 	 NULL,   PREC_NONE},
	[TOKEN_EOF] 			= {NULL, 	 NULL,   PREC_NONE},
};

static ParseRule* getRule(TokenType type) {
	return rules + type;
}

// =============================== Parsing ==============================

static void parsePrecedence(Precedence precedence) {
	// Compile operations with precedences >= parameter precedence.
	// After calling this, parser.current points to the next token with 
	// strictly lower precedence.

	advance();
	ParseFn prefixRule = getRule(parser.previous.type)->prefix;
	if (prefixRule == NULL) {
		error("Expect expression (prefix rule).");
		fprintf(stderr, "%d\n", parser.current.type);
		return;
	}

	bool canAssign = precedence <= PREC_ASSIGNMENT;
	// When a prefix is called, the leading token has been consumed.
	prefixRule(canAssign);

	while(precedence <= getRule(parser.current.type)->precedence) {
		advance();
		ParseFn infixRule = getRule(parser.previous.type)->infix;

		// When an infix is called, the entire left-hand operand has been compiled
		// and the subsequent infix operator has been consumed.

		if (!infixRule) {
			error("Expect expression (infix rule).");
			return;
		}
		infixRule(canAssign);
	}

	if (canAssign && matchCurrent(TOKEN_EQUAL)) {
		error("Invalid assignment target.");
	}
}

static uint8_t identifierConstant(Token* token) {
	return makeConstant(OBJ_VAL(copyString(token->start, token->length)));
}

static void addLocal(Token name) {
	if (curCompiler->localCount == UINT8_COUNT) {
		error("Too many local variables in function.");
		return;
	}
	Local* local = &curCompiler->locals[curCompiler->localCount++];
	local->name = name;
	local->depth = -1;			// uninitialized state
}

static void declareLocalVariable() {
	if (curCompiler->scopeDepth == 0) return;

	Token* name = &parser.previous;

	// Check if there's any duplication in the current or deeper scope
	for (int i=curCompiler->localCount-1; i>=0; --i) {
		Local* local = curCompiler->locals + i;
		if (local->depth != -1 && local->depth < curCompiler->scopeDepth) {
			break;
		}

		if (identifiersEqual(name, &local->name)) {
			error("Already a variable with this name in this scope.");
		}
	}

	addLocal(*name);
}

static uint8_t parseVariable(const char* message) {
	consume(TOKEN_IDENTIFIER, message);

	declareLocalVariable();
	if (curCompiler->scopeDepth > 0) return 0;

	return identifierConstant(&parser.previous);
}

static void markInitialized() {
	// only mark local variable as intialized.
	if (curCompiler->scopeDepth == 0) return;
	curCompiler->locals[curCompiler->localCount-1].depth =
		curCompiler->scopeDepth;
}

static void defineVariable(uint8_t global) {
	if (curCompiler->scopeDepth > 0) {
		markInitialized();
		return;
	}
	emitBytes(OP_DEFINE_GLOBAL, global);
}

static void synchronize() {
	parser.panicMode = false;

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
			default:
				;
		}
		advance();
	}
}


// ==================== Statements =====================

static void expression() {
	parsePrecedence(PREC_ASSIGNMENT);
}

static void expressionStatement() {
	expression();
	consume(TOKEN_SEMICOLON, "expressionStmt: Expect ';' after expression.");
	emitByte(OP_POP);
}

static void printStatement() {
	expression();
	consume(TOKEN_SEMICOLON, "Expect ';' after value.");
	emitByte(OP_PRINT);
}

static void returnStatement() {
	if (curCompiler->type == TYPE_SCRIPT) {
		error("Can't return from top-level code.");
	}
	if (matchCurrent(TOKEN_SEMICOLON)) {
		emitReturn();
	} else {
		if (curCompiler->type == TYPE_INITIALIZER) {
			// don't expect nontrivial returns from init
			error("Can't return a value from an initializer.");		
		}
		expression();
		consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
		emitByte(OP_RETURN);
	}
}

static void ifStatement() {
	consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
	expression();
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

	int thenJump = emitJump(OP_JUMP_IF_FALSE);
	emitByte(OP_POP);			// pop the condition expression if true
	statement();

	int elseJump = emitJump(OP_JUMP);
	patchJump(thenJump);
	emitByte(OP_POP);			// pop the condition expression if false

	if (matchCurrent(TOKEN_ELSE)) statement();
	patchJump(elseJump);

}

static void whileStatement() {
	int loopStart = currentChunk()->count;
	consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
	expression();
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

	int exitJump = emitJump(OP_JUMP_IF_FALSE);
	emitByte(OP_POP);
	statement();			// while part
	emitLoop(loopStart);

	patchJump(exitJump);
	emitByte(OP_POP);		// clear the while condition

}

static void beginScope() {
	curCompiler->scopeDepth++;
}

static void endScope() {
	curCompiler->scopeDepth--;
	// Delete any local variables in the deeper scope
	int cnt = curCompiler->localCount;
	while (cnt>0 && 
		   curCompiler->locals[cnt-1].depth >
					curCompiler->scopeDepth) 
	{
		if (curCompiler->locals[cnt-1].isCaptured) {
			emitByte(OP_CLOSE_UPVALUE);
		} else {
			emitByte(OP_POP);
		}
		--cnt;
	}
	int n_pop = curCompiler->localCount - cnt;
	if (n_pop == 0) return;
	if (n_pop == 1) {
		emitByte(OP_POP);
	} else {
		emitBytes(OP_POPN, n_pop);
	}
	curCompiler->localCount = cnt;
}

static void block() {
	while (!checkCurrent(TOKEN_RIGHT_BRACE) && !checkCurrent(TOKEN_EOF)) {
		declaration();
	}

	consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(FunctionType type) {
	Compiler compiler;
	initCompiler(&compiler, type);
	beginScope();

	consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
	// parse parameters
	if (!checkCurrent(TOKEN_RIGHT_PAREN)) {
		do {
			curCompiler->function->arity++;
			if (curCompiler->function->arity > 255) {
				errorAtCurrent("Can't have ore than 255 parameters.");
			}
			uint8_t arg = parseVariable("Expect parameter name.");
			defineVariable(arg);
		} while (matchCurrent(TOKEN_COMMA));
	}
	consume(TOKEN_RIGHT_PAREN, "Expect ')' after function name.");
	consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
	block();

	ObjFunction* fun = endCompiler();
	emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(fun)));

	for (int i=0; i<fun->upvalueCount; ++i) {
		emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
		emitByte(compiler.upvalues[i].index);
	}
}

static void funDelcaration() {
	uint8_t global = parseVariable("Expect function name.");
	markInitialized(); 

	function(TYPE_FUNCTION);
	defineVariable(global);
}

static void forStatement() {
	// Syntax: for (initializer; condition; update/increment) {...}

	beginScope();
	consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");

	// intializer
	if (matchCurrent(TOKEN_SEMICOLON)) {
		// No initializer.
	} else if (matchCurrent(TOKEN_VAR)) {
		varDeclaration();
	} else {
		expressionStatement();
	}

	// condition
	int loopStart = currentChunk()->count;
	int exitJump = -1;
	if (!matchCurrent(TOKEN_SEMICOLON)) {		// non-trivial condition
		expression();							// expression in stack
		consume(TOKEN_SEMICOLON, "Expect ';'.");

		// Jump out of the loop if condition == false
		exitJump = emitJump(OP_JUMP_IF_FALSE);
		emitByte(OP_POP); 						// clear condition expression
	}

	// update/increment
	if (!matchCurrent(TOKEN_RIGHT_PAREN)) {		// non-trivial update
		int bodyJump = emitJump(OP_JUMP);		// if cond, jump to body
		int updateStart = currentChunk()->count;
		expression();
		emitByte(OP_POP);						// clear update expression
		consume(TOKEN_RIGHT_PAREN, "Expect ')' after 'for'.");

		emitLoop(loopStart);
		loopStart = updateStart;				// Jump to nontrivial update after body
		patchJump(bodyJump);
	}

	// body
	statement();
	emitLoop(loopStart);

	if (exitJump != -1) {
		patchJump(exitJump);
		emitByte(OP_POP);
	}

	endScope();
}

static void statement() {
	if (matchCurrent(TOKEN_PRINT)) {
		printStatement();
	} else if (matchCurrent(TOKEN_IF)) {
		ifStatement();
	} else if (matchCurrent(TOKEN_RETURN)) {
		returnStatement();
	} else if (matchCurrent(TOKEN_WHILE)) {
		whileStatement();
	} else if (matchCurrent(TOKEN_FOR)) {
		forStatement();
	} else if (matchCurrent(TOKEN_LEFT_BRACE)) {
		beginScope();
		block();
		endScope();
	} else {
		expressionStatement();
	}
}

static void method() {
	consume(TOKEN_FUN, "Expect 'fun' before method name.");
	consume(TOKEN_IDENTIFIER, "Expect method name.");
	uint8_t pos = identifierConstant(&parser.previous);

	FunctionType type = TYPE_METHOD;
	if (parser.previous.length == 4 &&
		memcmp(parser.previous.start, "init", 4) == 0) {
		type = TYPE_INITIALIZER;
	}
	function(type);
	emitBytes(OP_METHOD, pos);

}

// ==================== Declaration ==========================

static void varDeclaration() {
	uint8_t global = parseVariable("Expect variable name.");

	if (matchCurrent(TOKEN_EQUAL)) {
		expression();
	} else {
		emitByte(OP_NIL);
	}
	consume(TOKEN_SEMICOLON, "Expect ';' after expression.");

	defineVariable(global);
}

static void classDeclaration() {
	consume(TOKEN_IDENTIFIER, "Expect class name.");
	Token className = parser.previous;
	size_t pos = identifierConstant(&parser.previous);
	declareLocalVariable();

	emitBytes(OP_CLASS, pos);
	defineVariable(pos);

	ClassCompiler classCompiler;
	classCompiler.enclosing = curClass;
	classCompiler.hasSuperclass = false;
	curClass = &classCompiler;

	if (matchCurrent(TOKEN_COLON)) {
		consume(TOKEN_IDENTIFIER, "Expect superclass name.");
		variable(false);

		if (identifiersEqual(&className, &parser.previous)) {
			error("At class can't inherit from itself.");
		}

		beginScope();
		addLocal(syntheticToken("super"));
		defineVariable(0);			// initialize the local

		namedVariable(className, false);
		emitByte(OP_INHERIT);
		classCompiler.hasSuperclass = true;
	}

	namedVariable(className, false);
	consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");

	while (!checkCurrent(TOKEN_RIGHT_BRACE) && !checkCurrent(TOKEN_EOF)) {
		method();
	}

	consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
	emitByte(OP_POP);

	if (classCompiler.hasSuperclass) {
		endScope();
	}

	curClass = curClass->enclosing;
}


static void declaration() {
	if (matchCurrent(TOKEN_CLASS)) {
		classDeclaration();
	} else if (matchCurrent(TOKEN_FUN)) {
		funDelcaration();
	} else if (matchCurrent(TOKEN_VAR)) {
		varDeclaration();
	} else {
		statement();
	}

	if (parser.panicMode) synchronize();
}


// =============================== Compile ==============================

ObjFunction* compile(const char* source) {
	initScanner(source);
	Compiler compiler;
	initCompiler(&compiler, TYPE_SCRIPT);

	parser.hadError = false;
	parser.panicMode = false;

	advance();
	
	while (!matchCurrent(TOKEN_EOF)) {
		declaration();
	}

	ObjFunction* function = endCompiler();
	return parser.hadError ? NULL : function;
}

void markCompilerRoots() {
	Compiler* compiler = curCompiler;
	while (compiler != NULL) {
		markObj((Obj*)compiler->function);
		compiler = compiler->enclosing;
	}
}

/** Example 1 of Pratt's parsing. 
 *  Suppose we feed an expression 1 + 2 + 3 into compile
 *  First advance. cur = 1.
 *  Run expression() == parsePrecedence(=)
 * 		Advance. cur = +, prev = 1.
 * 		prefixRule = number. Emit constant 1.
 * 		Since prec(=) <= prec(+), advance. cur = 2, prev = +.
 * 		Run infixRule = binary. 
 * 			Run parsePrecedence(p = prec(+)+1)
 * 				Advance. cur = +, prev = 2.
 * 				prefixRule = number. Emit constant 2. 
 *				Since prec(+) < p, stop. Go back to binary.
 *			Emit +.
 *		Since prec(=) <= prec(+), advance. cur = 3, prev = +.
 * 			... Emit constant 3 and +.
 * 				
 * 
 * chunk->code: [OP_CONSTANT] [0] [OP_CONSTANT] [1] [OP_ADD] [OP_CONSTANT] [2] [OP_ADD] 
 * i.e. 1 2 + 3 + OR (1+2) + 3
 * 
 *
 * Example 2. a * b = 2; where a, b are global.
 * cur = a.
 * Run expression = parsePrecedence(=)
 * 		Advance. cur = *. canAssign = true.
 * 		prefix = variable. cur!='=' -> Emit get_global a.
 * 		Since prec(=) <= prec(*), advance. cur = b.
 * 		Run infix = binary.
 * 			Run parsePrecedence(p = prec(*)+1)
 * 				Advance. cur = '='. canAssign = false.
 * 				prefix = variable. !canAssign -> Emit get_global b.
 * 				Since p > prec(=), stop.
 * 		Now cur = '=' but it's not consumed.
 * 		This is an issue because if LHS could be assigned, then it would've assigned
 * 				in variable.
 * 		So, report an error.
 * 				
 * 		
 * 		
 * 				
 * 
 **/




