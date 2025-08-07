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

// ===== Global variables =====

Parser parser;
Compiler* curCompiler = NULL;
ClassCompiler* curClass = NULL;

static Chunk* _chunk_cur() {
	return &curCompiler->function->chunk;		
}

// ===== Declarations =====

// === error handling ===
static void _error_at(Token* token, const char* message); 
static void _error(const char* message);			// _error_at(&parser.previous)
static void _error_cur(const char* message);		// _error_at(&parser.current)

// === moving ===
static void _advance();
// move to next token, if meeting error skip everything

static bool _check(TokenType type);   // check if current token has type
static bool _match(TokenType type);   // if matched, advance

static void _consume(TokenType type, const char* message);
// consume the current token, if not matched, print error message


// === emitting ===
static uint8_t _add_const(Value value);		
// wrapper for chunk_add_const: add value to current chunk's constant pool
// if value is in pool, return its position

static uint8_t _add_identifier(Token* name);
// wrapper for _add_const when value is ObjString in name

static void _emit_byte(uint8_t);
static void _emit_bytes(uint8_t, uint8_t);
static void _emit_const(Value);
static int 	_emit_jump(uint8_t);
static void _patch_jump(int);
static void _emit_loop(int);
static void _emit_return();

// ===== Parsing =====

/* Structure Overview

-- declaration
	|-- class declaration
	|-- fun declaration
	|-- var declaration
	|-- statement
		|-- print stmt
		|-- if
		|-- return
		|-- while
		|-- for
		|-- expression
		|-- block
			|-- declarations
		
*/

// === parsing declarations ===

static void _decl();
static void _decl_class();
static void _decl_fun();
static void _fun_def(FunctionType);
static void _decl_var();
static void _decl_stmt();
static void _decl_sync();

// === parsing statements ===

static void _stmt_print();
static void _stmt_if();
static void _stmt_return();
static void _stmt_while();
static void _stmt_for();
static void _stmt_expr();
static void _stmt_block();
static void _scope_begin();
static void _scope_end();

// === parsing expression ===
static void _expr();			// just _parse_prec(=)

static void _parse_prec(Precedence);
// parse operations with prec >= arg prec.
// After this, parser.current = next token with strictly lower prec
// Expressions's Opcodes are stored in postfix.

// = prefix =
static void _group(bool canAssign);
static void _unary(bool canAssign);
static void _this(bool canAssign);
static void _super(bool canAssign);
static void _string(bool canAssign);
static void _number(bool canAssign);
static void _literal(bool canAssign);
static void _var(bool canAssign);		// just _parse_var(prev, cA)

// = var =
static uint8_t _var_decl(const char* message);
static void _var_def(uint8_t global);
static void _var_get(Token name);		// just _parse_var(name, false)
static void _parse_var(Token name, bool canAssign);
// Parse name. If next token is = and cA, parse expr and assign it to var
// Otherwise, emit ..._GET

// = infix =
static void _binary(bool canAssign);
static void _call(bool canAssign);
static void _dot(bool canAssign);
static void _and(bool canAssign);
static void _or (bool canAssign);

// rules chart:  token type -> (prefix, infix, prec)
ParseRule rules[] = {
	[TOKEN_LEFT_PAREN] 		= {_group,	_call,		PREC_CALL},
	[TOKEN_RIGHT_PAREN]		= {NULL,	NULL,	 	PREC_NONE},
	[TOKEN_LEFT_BRACE]		= {NULL,	NULL,	 	PREC_NONE}, 
	[TOKEN_RIGHT_BRACE]		= {NULL,	NULL,	 	PREC_NONE},
	[TOKEN_COMMA]			= {NULL,	NULL,	 	PREC_NONE},
	[TOKEN_DOT]				= {NULL,	_dot,		PREC_CALL},
	[TOKEN_MINUS]			= {_unary,	_binary, 	PREC_TERM},
	[TOKEN_PLUS]			= {NULL,	_binary, 	PREC_TERM},
	[TOKEN_SEMICOLON]		= {NULL, 	NULL, 	 	PREC_NONE},
	[TOKEN_SLASH]			= {NULL,	_binary, 	PREC_FACTOR},
	[TOKEN_STAR]			= {NULL,	_binary, 	PREC_FACTOR},
	[TOKEN_COLON]			= {NULL, 	NULL, 	 	PREC_NONE},
	[TOKEN_BANG] 			= {_unary, 	NULL, 	 	PREC_NONE},
	[TOKEN_BANG_EQUAL] 		= {NULL, 	_binary, 	PREC_EQUALITY},
	[TOKEN_EQUAL] 			= {NULL, 	NULL, 	 	PREC_NONE},
	[TOKEN_EQUAL_EQUAL] 	= {NULL, 	_binary, 	PREC_EQUALITY},
	[TOKEN_GREATER] 		= {NULL, 	_binary, 	PREC_COMPARISON},
	[TOKEN_GREATER_EQUAL] 	= {NULL, 	_binary, 	PREC_COMPARISON},
	[TOKEN_LESS] 			= {NULL, 	_binary, 	PREC_COMPARISON},
	[TOKEN_LESS_EQUAL] 		= {NULL, 	_binary, 	PREC_COMPARISON},
	[TOKEN_IDENTIFIER] 		= {_var, 	NULL, 	 	PREC_NONE},
	[TOKEN_STRING] 			= {_string, NULL, 	 	PREC_NONE},
	[TOKEN_NUMBER] 			= {_number,	NULL, 	 	PREC_NONE},
	[TOKEN_AND] 			= {NULL, 	_and, 	 	PREC_NONE},
	[TOKEN_CLASS] 			= {NULL, 	NULL, 	 	PREC_NONE},
	[TOKEN_ELSE] 			= {NULL, 	NULL, 	 	PREC_NONE},
	[TOKEN_FALSE]			= {_literal,NULL, 	 	PREC_NONE},
	[TOKEN_FOR] 			= {NULL, 	NULL, 	 	PREC_NONE},
	[TOKEN_FUN] 			= {NULL, 	NULL, 	 	PREC_NONE},
	[TOKEN_IF] 				= {NULL, 	NULL,   	PREC_NONE},
	[TOKEN_NIL] 			= {_literal, NULL,   	PREC_NONE},
	[TOKEN_OR] 				= {NULL, 	_or,    	PREC_NONE},
	[TOKEN_PRINT] 			= {NULL, 	NULL, 		PREC_NONE},
	[TOKEN_RETURN] 			= {NULL, 	NULL, 	 	PREC_NONE},
	[TOKEN_SUPER] 			= {_super, 	NULL,   	PREC_NONE},
	[TOKEN_THIS] 			= {_this, 	NULL,   	PREC_NONE},
	[TOKEN_TRUE] 			= {_literal,NULL,   	PREC_NONE},
	[TOKEN_VAR] 			= {NULL, 	NULL,   	PREC_NONE},
	[TOKEN_WHILE] 			= {NULL, 	NULL,   	PREC_NONE},
	[TOKEN_ERROR] 			= {NULL, 	NULL,   	PREC_NONE},
	[TOKEN_EOF] 			= {NULL, 	NULL,   	PREC_NONE},
};

// === parsing helpers ===
static uint8_t _parse_args();
static bool _token_cmp(Token* a, Token* b);
static int _local_resolve(Compiler* compiler, Token* name);
static int _up_add(Compiler* compiler, uint8_t index, bool isLocal);
static int _up_resolve(Compiler* compiler, Token* name);
static Token _make_token(const char* name);


// === compiler setup ===
static void _compiler_init(Compiler* compiler, FunctionType type);
static ObjFunction* _compiler_end();

// ===== main functions =====

ObjFunction* compile(const char* source) {
	scan_init(source);
	Compiler compiler;
	_compiler_init(&compiler, TYPE_SCRIPT);

	parser.hadError = false;
	parser.panicMode = false;

	_advance();
	
	while (!_match(TOKEN_EOF)) {
		_decl();
	}

	ObjFunction* fun = _compiler_end();
	return parser.hadError ? NULL : fun;
}

void markCompilerRoots() {
	Compiler* compiler = curCompiler;
	while (compiler != NULL) {
		markObj((Obj*)compiler->function);
		compiler = compiler->enclosing;
	}
}

// ===== local functions =====

// === error handling ===

static void _error_at(Token* token, const char* message) {
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

static void _error(const char* message) {
	_error_at(&parser.previous, message);
}

static void _error_cur(const char* message) {
	_error_at(&parser.current, message);
}


// === moving ===
static void _advance() {
	parser.previous = parser.current;

	// only report the first error in the current statement, ignore the rest.
	for (;;) {
		parser.current = scan_token();
		if (parser.current.type != TOKEN_ERROR) break;
		_error_cur(parser.current.start);
	}
}

static void _consume(TokenType type, const char* message) {
	if (parser.current.type == type) {
		_advance();
		return;
	}

	_error_cur(message);
}

static bool _check(TokenType type) {
	return parser.current.type == type;
}

static bool _match(TokenType type) {
	if (!_check(type)) return false;
	_advance();
	return true;
}

// === emit ===

static void _emit_byte(uint8_t byte) {
	writeChunk(_chunk_cur(), byte, parser.previous.line);
}

static void _emit_bytes(uint8_t byte1, uint8_t byte2) {
	_emit_byte(byte1);
	_emit_byte(byte2);
}

static uint8_t _add_const(Value value) {
	int index = chunk_add_const(_chunk_cur(), value);
	if (index > UINT8_MAX) {
		error("Too many constants in one chunk.");
		return 0;
	}

	return (uint8_t)index;
}

static uint8_t _add_identifier(Token* token) {
	return _add_const(OBJ_VAL(copyString(token->start, token->length)));
}

static void _emit_const(Value value) {
	_emit_bytes(OP_CONSTANT, _add_const(value));
}

static int _emit_jump(uint8_t instruction) {
	_emit_byte(instruction);
	_emit_byte(0xff);
	_emit_byte(0xff);

	// Leave placeholder for future patch. 
	// Return to the first byte of jump location

	return _chunk_cur()->count - 2;
}

static void _patch_jump(int offset) {
	// Go back to offset (the first byte of jump) 
	// 		and update the correct jump.
	// Imgaine now we are at the second byte of jump: offset+1, 
	// 		we should jump to _chunk_cur()->count-1.

	int jump = _chunk_cur()->count - offset - 2;
	if (jump > UINT16_MAX) {
		error("_patch_jump: Too much code to jump over.");
	}

	_chunk_cur()->code[offset] = (jump>>8) & 0xff;
	_chunk_cur()->code[offset+1] = jump & 0xff;
}

static void _emit_loop(int loopStart) {
	_emit_byte(OP_LOOP);

	int offset = _chunk_cur()->count - loopStart + 2;
	if (offset > UINT16_MAX) {
		error("_emit_loop: Loop body too large.");
	}
	_emit_bytes((offset>>8) & 0xff, offset & 0xff);
}

static void _emit_return() {
	if (curCompiler->type == TYPE_INITIALIZER) {
		_emit_bytes(OP_GET_LOCAL, 0);			// return the instance
	} else {
		_emit_byte(OP_NIL);
	}

	_emit_byte(OP_RETURN);
}

static void _compiler_init(Compiler* compiler, FunctionType type) {
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

static ObjFunction* _compiler_end() {
	_emit_return();
	ObjFunction* function = curCompiler->function;

#ifdef DEBUG_PRINT_CODE
	if (!parser.hadError) {
		disassembleChunk(
			_chunk_cur(), 
			function->name!=NULL ? function->name->chars : "<script>"
		);
	}
#endif

	curCompiler = curCompiler->enclosing;
	return function;
}

// ========================= Parsing declaration ===========================

static void _parse_prec(Precedence);
static void _expr();
static void _decl_stmt();
static void _decl();
static uint8_t _add_identifier(Token*);
static int _local_resolve(Compiler*, Token*);
static void _decl_var();


// === parsing declarations ===

static void _decl() {
	if (_match(TOKEN_CLASS)) {
		_decl_class();
	} else if (_match(TOKEN_FUN)) {
		_decl_fun();
	} else if (_match(TOKEN_VAR)) {
		_decl_var();
	} else {
		_decl_stmt();
	}

	if (parser.panicMode) _decl_sync();
}

static void _decl_class() {
	_consume(TOKEN_IDENTIFIER, "Expect class name.");
	Token className = parser.previous;
	size_t pos = _add_identifier(&parser.previous);
	_local_decl();

	_emit_bytes(OP_CLASS, pos);
	_var_def(pos);

	ClassCompiler classCompiler;
	classCompiler.enclosing = curClass;
	classCompiler.hasSuperclass = false;
	curClass = &classCompiler;

	if (_match(TOKEN_COLON)) {
		_consume(TOKEN_IDENTIFIER, "Expect superclass name.");
		_var(false);

		if (_token_cmp(&className, &parser.previous)) {
			_error("At class can't inherit from itself.");
		}

		_scope_begin();
		_local_add(_make_token("super"));
		_var_def(0);			// initialize the local

		// _parse_var(className, false);
		_var_get(className);
		_emit_byte(OP_INHERIT);
		classCompiler.hasSuperclass = true;
	}

	// _parse_var(className, false);
	_var_get(className);
	_consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");

	while (!_check(TOKEN_RIGHT_BRACE) && !_check(TOKEN_EOF)) {
		method();
	}

	_consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
	_emit_byte(OP_POP);

	if (classCompiler.hasSuperclass) {
		_scope_end();
	}

	curClass = curClass->enclosing;
}

static void _decl_fun() {
	uint8_t funName = _var_decl("Expect function name.");
	_local_init(); 

	_fun_def(TYPE_FUNCTION);
	_var_def(funName);
}

static void _fun_def(FunctionType type) {
	Compiler compiler;
	_compiler_init(&compiler, type);
	_scope_begin();

	_consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
	// parse parameters
	if (!_check(TOKEN_RIGHT_PAREN)) {
		do {
			curCompiler->function->arity++;
			if (curCompiler->function->arity > 255) {
				_error_cur("Can't have ore than 255 parameters.");
			}
			uint8_t arg = _var_decl("Expect parameter name.");
			_var_def(arg);
		} while (_match(TOKEN_COMMA));
	}
	_consume(TOKEN_RIGHT_PAREN, "Expect ')' after function name.");
	_consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
	_stmt_block();

	ObjFunction* fun = _compiler_end();
	_emit_bytes(OP_CLOSURE, _add_const(OBJ_VAL(fun)));

	for (int i=0; i<fun->upvalueCount; ++i) {
		_emit_byte(compiler.upvalues[i].isLocal ? 1 : 0);
		_emit_byte(compiler.upvalues[i].index);
	}
}

static void _decl_stmt();

static void _decl_var() {
	uint8_t global = _var_decl("Expect variable name.");

	if (_match(TOKEN_EQUAL)) {
		_expr();
	} else {
		_emit_byte(OP_NIL);
	}
	_consume(TOKEN_SEMICOLON, "Expect ';' after expression.");

	_var_def(global);
}

static void _decl_sync() {
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
		_advance();
	}
}


// === Parsing expressions ===

static void _expr() {
	_parse_prec(PREC_ASSIGNMENT);
}

static void _parse_prec(Precedence prec) {
	_advance();
	ParseFn prefixRule = rules[parser.previous.type].prefix;
	// e.g. [1.2]  [+]	 [3.4]
	// 		prev   cur 	  should call _number to parse [1.2] first

	if (prefixRule == NULL) {
		error("Expect expression (prefix rule).");
		fprintf(stderr, "%d\n", parser.current.type);
		return;
	}

	bool canAssign = prec <= PREC_ASSIGNMENT;
	// Can prev be assigned? e.g. a+b cannot be assigned.
	// Lox allows a = b = c;
	prefixRule(canAssign);

	while(prec <= rules[parser.current.type].precedence) {
		_advance();
		ParseFn infixRule = rules[parser.previous.type].infix;
		// e.g. [1.2]  [+]	 [3.4]
		// 			   prev   cur 

		if (!infixRule) {
			error("Expect expression (infix rule).");
			return;
		}
		infixRule(canAssign);
	}

	// Correct assignment would be parsed when parsing variables
	if (canAssign && _match(TOKEN_EQUAL)) {
		error("Invalid assignment target.");
	}
}

// For all the operations below, the operator is parser.previous 

static void _group(bool canAssign) {
	// e.g. [(]  [1.2]  [+]  [3.4]  [)]
	//		prev  cur
	_expr();
	// e.g. [(]  [1.2]  [+]  [3.4]  [)]
	//						  prev  cur
	_consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void _unary(bool canAssign) {
	TokenType opType = parser.previous.type;
	// e.g. 	[-]  [1.2]
	//			prev  cur
	_parse_prec(PREC_UNARY);

	switch(opType) {
		case TOKEN_MINUS:	_emit_byte(OP_NEGATE); break;
		case TOKEN_BANG:	_emit_byte(OP_NOT); break;
		default: return;
	}
}

static void _number(bool canAssign) {
	double value = strtod(parser.previous.start, NULL);
	_emit_const(NUMBER_VAL(value));
}

static void _literal(bool canAssign) {
	switch (parser.previous.type) {
		case TOKEN_NIL:		_emit_byte(OP_NIL); break;
		case TOKEN_TRUE:	_emit_byte(OP_TRUE); break;
		case TOKEN_FALSE:	_emit_byte(OP_FALSE); break;
		default: return;
	}
}

static void _string(bool canAssign) {
	// 		 [ " h i " ]
	// start + 0 1 2 3
	_emit_const(
		OBJ_VAL(copyString(parser.previous.start+1, parser.previous.length-2))
	);
}

static uint8_t _parse_args() {
	// e.g. [(]  [a] [,] [b] [)]
	//		prev cur
	uint8_t argCount = 0;
	if (!_check(TOKEN_RIGHT_PAREN)) {
		do {
			_expr();
			if (argCount == 255) {
				error("Can't have more than 255 arguments.");
			}
			argCount++;
		} while (_match(TOKEN_COMMA));
	}
	// e.g. [(]  [a] [,]  [b] [)]
	//				     prev cur
	_consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
	return argCount;
}

static void _call(bool canAssign) {
	uint8_t argCount = _parse_args();
	_emit_bytes(OP_CALL, argCount);
	// argCount tells how far we look back
}

static void _dot(bool canAssign) {
	_consume(TOKEN_IDENTIFIER, "Expect property name after '.'");
	uint8_t name = _add_identifier(&parser.previous);

	if (canAssign && _match(TOKEN_EQUAL)) {
		_expr();
		_emit_bytes(OP_SET_PROPERTY, name);
	} else if (_match(TOKEN_LEFT_PAREN)) {
		uint8_t argCount = _parse_args();
		_emit_bytes(OP_INVOKE, name);
		_emit_byte(argCount);
	} else {
		_emit_bytes(OP_GET_PROPERTY, name);
	}
}

static void _binary(bool canAssign) {
	// e.g. [1.2]  [+]  [3.4] * [5.6]
	//		       prev  cur			
	TokenType opType = parser.previous.type;
	ParseRule* rule = &rules[opType];
	_parse_prec((Precedence)(rule->precedence + 1));

	switch (opType) {
		case TOKEN_BANG_EQUAL:		_emit_bytes(OP_EQUAL, OP_NOT); break;
		case TOKEN_EQUAL_EQUAL:		_emit_byte(OP_EQUAL); break;
		case TOKEN_GREATER:			_emit_byte(OP_GREATER); break;
		case TOKEN_GREATER_EQUAL:	_emit_bytes(OP_LESS, OP_NOT); break;
		case TOKEN_LESS:			_emit_byte(OP_LESS); break;
		case TOKEN_LESS_EQUAL:		_emit_bytes(OP_GREATER, OP_NOT); break;
		case TOKEN_PLUS:			_emit_byte(OP_ADD); break;
		case TOKEN_MINUS:			_emit_byte(OP_SUBTRACT); break;
		case TOKEN_STAR:			_emit_byte(OP_MULTIPLY); break;
		case TOKEN_SLASH:			_emit_byte(OP_DIVIDE); break;
		default: return;
	}
}

static void _and(bool canAssign) {
	int endJump = _emit_jump(OP_JUMP_IF_FALSE);

	_emit_byte(OP_POP);
	_parse_prec(PREC_AND);
	_patch_jump(endJump);
}

static void _or (bool canAssign) {
	int falseJump = _emit_jump(OP_JUMP_IF_FALSE);
	int trueJump = _emit_jump(OP_JUMP);

	_patch_jump(falseJump);
	_emit_byte(OP_POP);

	_parse_prec(PREC_OR);
	_patch_jump(trueJump);
}

static bool _token_cmp(Token* a, Token* b) {
	if (a->length != b->length) return false;
	return memcmp(a->start, b->start, a->length) == 0;
}

static void _parse_var(Token name, bool canAssign) {
	// Set or get a variable but not define.

	uint8_t getOp, setOp;
	int arg = _local_resolve(curCompiler, &name);
	if (arg != -1) {
		// local variable
		getOp = OP_GET_LOCAL;
		setOp = OP_SET_LOCAL;
	} else if ((arg = _up_resolve(curCompiler, &name)) != -1) {
		// upvalue in the enclosing closure
		getOp = OP_GET_UPVALUE;
		setOp = OP_SET_UPVALUE;
	}
	else {
		// global variable
		arg = _add_identifier(&name);
		getOp = OP_GET_GLOBAL;
		setOp = OP_SET_GLOBAL;
	}

	if (canAssign && _match(TOKEN_EQUAL)) {
		_expr();
		_emit_bytes(setOp, (uint8_t)arg);
	} else {
		_emit_bytes(getOp, (uint8_t)arg);
	}
}

static void _var(bool canAssign) {
	_parse_var(parser.previous, canAssign);
}

static void _var_get(Token name) {
	_parse_var(name, false);
}

static uint8_t _var_decl(const char* message) {
	_consume(TOKEN_IDENTIFIER, message);

	if (curCompiler->scopeDepth > 0) {
		_local_decl();
		return 0;
	}

	return _add_identifier(&parser.previous);
}

static Token _make_token(const char* name) {
	Token token;
	token.start = name;
	token.length = (int)strlen(name);
	return token;
}

static void _this(bool canAssign) {
	if (curClass == NULL) {
		error("Can't use 'this' outside of a class.");
		return;
	}

	_var(false);
}

static void _super(bool canAssign) {
	if (curClass == NULL) {
		error("Can't use 'super' outside of a class.");
	} else if (!curClass->hasSuperclass) {
		error("Can't use 'super' in a class with no superclass.");
	}
	_consume(TOKEN_DOT, "Expect '.' after 'super'.");
	_consume(TOKEN_IDENTIFIER, "Expect superclass method name");
	uint8_t name = _add_identifier(&parser.previous);

	// _parse_var(_make_token("this"), false);
	_var_get(_make_token("this"));

	if (_match(TOKEN_LEFT_PAREN))	{
		uint8_t argCount = _parse_args();
		// _parse_var(_make_token("super"), false);
		_var_get(_make_token("super"));
		_emit_bytes(OP_SUPER_INVOKE, name);
		_emit_byte(argCount);
	} else {
		// _parse_var(_make_token("super"), false);
		_var_get(_make_token("super"));
		_emit_bytes(OP_GET_SUPER, name);
	}
}

static void _local_add(Token name) {
	if (curCompiler->localCount == UINT8_COUNT) {
		error("Too many local variables in function.");
		return;
	}
	Local* local = &curCompiler->locals[curCompiler->localCount++];
	local->name = name;
	local->depth = -1;			// uninitialized state
}

static void _local_decl() {
	if (curCompiler->scopeDepth == 0) return;

	Token* name = &parser.previous;

	// Check if there's any duplication in the current or deeper scope
	for (int i=curCompiler->localCount-1; i>=0; --i) {
		Local* local = curCompiler->locals + i;
		if (local->depth != -1 && local->depth < curCompiler->scopeDepth) {
			break;
		}

		if (_token_cmp(name, &local->name)) {
			_error("Already a variable with this name in this scope.");
		}
	}

	_local_add(*name);
}

static void _local_init() {
	// only mark local variable as intialized.
	if (curCompiler->scopeDepth == 0) return;
	curCompiler->locals[curCompiler->localCount-1].depth =
		curCompiler->scopeDepth;
}

static void _var_def(uint8_t global) {
	if (curCompiler->scopeDepth > 0) {
		_local_init();
		return;
	}
	_emit_bytes(OP_DEFINE_GLOBAL, global);
}

static int _local_resolve(Compiler* compiler, Token* name) {
	for (int i=compiler->localCount-1; i>=0; --i) {
		Local* local = compiler->locals + i;
		if (_token_cmp(name, &local->name)) {
			if (local->depth == -1) {
				error("Can't read local variable in its own initializer.");
			}
			return i;
		} 
	}
	return -1;
}

static int _up_add(Compiler* compiler, uint8_t index, bool isLocal) {
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

static int _up_resolve(Compiler* compiler, Token* name) {
	if (compiler->enclosing == NULL) return -1; 	// script

	int local = _local_resolve(compiler->enclosing, name);
	if (local != -1) {
		compiler->enclosing->locals[local].isCaptured = true;
		return _up_add(compiler, (uint8_t)local, true);
	}

	int upvalue = _up_resolve(compiler->enclosing, name);
	if (upvalue != -1) {
		return _up_add(compiler, (uint8_t)upvalue, false);
	}

	return -1;
}

// === Statements ===

static void _decl_stmt() {
	if (_match(TOKEN_PRINT)) {
		_stmt_print();
	} else if (_match(TOKEN_IF)) {
		_stmt_if();
	} else if (_match(TOKEN_RETURN)) {
		_stmt_return();
	} else if (_match(TOKEN_WHILE)) {
		_stmt_while();
	} else if (_match(TOKEN_FOR)) {
		_stmt_for();
	} else if (_match(TOKEN_LEFT_BRACE)) {
		_scope_begin();
		_stmt_block();
		_scope_end();
	} else {
		_stmt_expr();
	}
}

static void _stmt_expr() {
	_expr();
	_consume(TOKEN_SEMICOLON, "expressionStmt: Expect ';' after expression.");
	_emit_byte(OP_POP);
}

static void _stmt_print() {
	_expr();
	_consume(TOKEN_SEMICOLON, "Expect ';' after value.");
	_emit_byte(OP_PRINT);
}

static void _stmt_return() {
	if (curCompiler->type == TYPE_SCRIPT) {
		error("Can't return from top-level code.");
	}
	if (_match(TOKEN_SEMICOLON)) {
		_emit_return();
	} else {
		if (curCompiler->type == TYPE_INITIALIZER) {
			// don't expect nontrivial returns from init
			error("Can't return a value from an initializer.");		
		}
		_expr();
		_consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
		_emit_byte(OP_RETURN);
	}
}

static void _stmt_if() {
	_consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
	_expr();
	_consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

	int thenJump = _emit_jump(OP_JUMP_IF_FALSE);
	_emit_byte(OP_POP);			// pop the condition expression if true
	_decl_stmt();

	int elseJump = _emit_jump(OP_JUMP);
	_patch_jump(thenJump);
	_emit_byte(OP_POP);			// pop the condition expression if false

	if (_match(TOKEN_ELSE)) _decl_stmt();
	_patch_jump(elseJump);

}

static void _stmt_while() {
	int loopStart = _chunk_cur()->count;
	_consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
	_expr();
	_consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

	int exitJump = _emit_jump(OP_JUMP_IF_FALSE);
	_emit_byte(OP_POP);
	_decl_stmt();			// while part
	_emit_loop(loopStart);

	_patch_jump(exitJump);
	_emit_byte(OP_POP);		// clear the while condition

}

static void _scope_begin() {
	curCompiler->scopeDepth++;
}

static void _scope_end() {
	curCompiler->scopeDepth--;
	// Delete any local variables in the deeper scope
	int cnt = curCompiler->localCount;
	while (cnt>0 && 
		   curCompiler->locals[cnt-1].depth >
					curCompiler->scopeDepth) 
	{
		if (curCompiler->locals[cnt-1].isCaptured) {
			_emit_byte(OP_CLOSE_UPVALUE);
		} else {
			_emit_byte(OP_POP);
		}
		--cnt;
	}
	int n_pop = curCompiler->localCount - cnt;
	if (n_pop == 0) return;
	if (n_pop == 1) {
		_emit_byte(OP_POP);
	} else {
		_emit_bytes(OP_POPN, n_pop);
	}
	curCompiler->localCount = cnt;
}

static void _stmt_block() {
	while (!_check(TOKEN_RIGHT_BRACE) && !_check(TOKEN_EOF)) {
		_decl();
	}

	_consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}



static void _stmt_for() {
	// Syntax: for (initializer; condition; update/increment) {...}

	_scope_begin();
	_consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");

	// intializer
	if (_match(TOKEN_SEMICOLON)) {
		// No initializer.
	} else if (_match(TOKEN_VAR)) {
		_decl_var();
	} else {
		_stmt_expr();
	}

	// condition
	int loopStart = _chunk_cur()->count;
	int exitJump = -1;
	if (!_match(TOKEN_SEMICOLON)) {		// non-trivial condition
		_expr();							// expression in stack
		_consume(TOKEN_SEMICOLON, "Expect ';'.");

		// Jump out of the loop if condition == false
		exitJump = _emit_jump(OP_JUMP_IF_FALSE);
		_emit_byte(OP_POP); 						// clear condition expression
	}

	// update/increment
	if (!_match(TOKEN_RIGHT_PAREN)) {		// non-trivial update
		int bodyJump = _emit_jump(OP_JUMP);		// if cond, jump to body
		int updateStart = _chunk_cur()->count;
		_expr();
		_emit_byte(OP_POP);						// clear update expression
		_consume(TOKEN_RIGHT_PAREN, "Expect ')' after 'for'.");

		_emit_loop(loopStart);
		loopStart = updateStart;				// Jump to nontrivial update after body
		_patch_jump(bodyJump);
	}

	// body
	_decl_stmt();
	_emit_loop(loopStart);

	if (exitJump != -1) {
		_patch_jump(exitJump);
		_emit_byte(OP_POP);
	}

	_scope_end();
}



static void method() {
	_consume(TOKEN_FUN, "Expect 'fun' before method name.");
	_consume(TOKEN_IDENTIFIER, "Expect method name.");
	uint8_t pos = _add_identifier(&parser.previous);

	FunctionType type = TYPE_METHOD;
	if (parser.previous.length == 4 &&
		memcmp(parser.previous.start, "init", 4) == 0) {
		type = TYPE_INITIALIZER;
	}
	_fun_def(type);
	_emit_bytes(OP_METHOD, pos);

}

// ==================== Declaration ==========================





/** Example 1 of Pratt's parsing. 
 *  Suppose we feed an expression 1 + 2 + 3 into compile
 *  First _advance. cur = 1.
 *  Run expression() == _parse_prec(=)
 * 		Advance. cur = +, prev = 1.
 * 		prefixRule = number. Emit constant 1.
 * 		Since prec(=) <= prec(+), _advance. cur = 2, prev = +.
 * 		Run infixRule = _binary. 
 * 			Run _parse_prec(p = prec(+)+1)
 * 				Advance. cur = +, prev = 2.
 * 				prefixRule = number. Emit constant 2. 
 *				Since prec(+) < p, stop. Go back to _binary.
 *			Emit +.
 *		Since prec(=) <= prec(+), _advance. cur = 3, prev = +.
 * 			... Emit constant 3 and +.
 * 				
 * 
 * chunk->code: [OP_CONSTANT] [0] [OP_CONSTANT] [1] [OP_ADD] [OP_CONSTANT] [2] [OP_ADD] 
 * i.e. 1 2 + 3 + OR (1+2) + 3
 * 
 *
 * Example 2. a * b = 2; where a, b are global.
 * cur = a.
 * Run expression = _parse_prec(=)
 * 		Advance. cur = *. canAssign = true.
 * 		prefix = variable. cur!='=' -> Emit get_global a.
 * 		Since prec(=) <= prec(*), _advance. cur = b.
 * 		Run infix = _binary.
 * 			Run _parse_prec(p = prec(*)+1)
 * 				Advance. cur = '='. canAssign = false.
 * 				prefix = variable. !canAssign -> Emit get_global b.
 * 				Since p > prec(=), stop.
 * 		Now cur = '=' but it's not _consumed.
 * 		This is an issue because if LHS could be assigned, then it would've assigned
 * 				in variable.
 * 		So, report an error.
 * 				
 * 		
 * 		
 * 				
 * 
 **/




