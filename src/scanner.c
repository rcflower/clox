#include <stdio.h>
#include <string.h>

#include "common.h"
#include "scanner.h"

// ===== global variable scanner =====

typedef struct {
	const char* start;		// start of current token
	const char* current;	// current pointer to be processed
	int line;
} Scanner;

Scanner scanner;


// ===== local helper function declarations =====

// === simple moving ===
static char _peek();					// return current char
static char _peek_next();				// return next char
static char _advance();					// return current char, move to next 	
static bool _match(char expected);		// if matched with current, move to next
static void _skip_white();

// === simple checking ===
static bool _is_digit(char c);
static bool _is_alpha(char c);
static bool _is_end();

// === wrapping ===
static Token _make_token(TokenType type);
// after finding a token, wrap it up to return a Token instance

static Token _make_error_token(const char* message);

// === scanning ===
static Token _scan_string();
static Token _scan_number();
static Token _scan_identifier();
static TokenType _make_keyword(
	int start, 
	int length, 
	const char* rest, 
	TokenType type
);



// ===== main functions =====

void scan_init(const char* source) {
	scanner.start = source;
	scanner.current = source;
	scanner.line = 1;
}

Token scan_token() {
	_skip_white();
	scanner.start = scanner.current;

	if (_is_end()) return _make_token(TOKEN_EOF);

	char c = _advance();

	if (_is_digit(c)) return _scan_number();
	if (_is_alpha(c)) return _scan_identifier();

	switch (c) {
		case '(': return _make_token(TOKEN_LEFT_PAREN);
		case ')': return _make_token(TOKEN_RIGHT_PAREN);
		case '{': return _make_token(TOKEN_LEFT_BRACE);
		case '}': return _make_token(TOKEN_RIGHT_BRACE);
		case ',': return _make_token(TOKEN_COMMA);
		case '.': return _make_token(TOKEN_DOT);
		case '-': return _make_token(TOKEN_MINUS);
		case '+': return _make_token(TOKEN_PLUS);
		case ':': return _make_token(TOKEN_COLON);
		case ';': return _make_token(TOKEN_SEMICOLON);
		case '/': return _make_token(TOKEN_SLASH);
		case '*': return _make_token(TOKEN_STAR);
		case '!': 
			return _make_token(
				_match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG
			);
		case '=':
			return _make_token(
				_match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL
			);
		case '<':
			return _make_token(
				_match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS
			);
		case '>':
			return _make_token(
				_match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER
			);
		case '"': return _scan_string();

	}

	return _make_error_token("Unexpected character.");
}

void token_print(Token* token) {
	printf("Token: [");
	for (int i=0; i<token->length; ++i) {
		printf("%c", token->start[i]);
	}
	printf("] of length %d\n", token->length);
}


// ===== local helper functions =====

// === simple moving ===

static char _peek() {
	return *scanner.current;
}

static char _peek_next() {
	if (_is_end()) return '\0';
	return scanner.current[1];
}

static char _advance() {
	return *(scanner.current++);
}

static bool _match(char expected) {
	if (_is_end()) return false;
	if (_peek() != expected) return false;
	_advance();
	return true;
}

static void _skip_white() {
	for(;;) {
		char c = _peek();
		switch (c) {
			case ' ':
			case '\r':
			case '\t':
				_advance();
				break;
			case '\n':
				scanner.line++;
				_advance();
				break;
			case '/':
				if (_peek_next() == '/') {
					// ignore the rest of this line
					while (_peek() != '\n' && !_is_end()) _advance();
				} else 
					return; // leave it unprocessed
				break;
			default:
				return;
		}
	}
}

// === checking ===

static bool _is_digit(char c) {
	return c >= '0' && c <= '9';
}

static bool _is_alpha(char c) {
	return (c>='a' && c<='z') || 
		   (c>='A' && c<='Z') ||
		    c == '_';
}

static bool _is_end() {
	return _peek() == '\0';
}

// === wrapping ===

static Token _make_token(TokenType type) {
	Token token;
	token.type = type;
	token.start = scanner.start;
	token.length = (int)(scanner.current - scanner.start);
	token.line = scanner.line;
	return token;
}

static Token _make_error_token(const char* message) {
	Token token;
	token.type = TOKEN_ERROR;
	token.start = message;
	token.length = (int)strlen(message);
	token.line = scanner.line;
	return token;	
}

// === scanning ===

static Token _scan_string() {
	while (_peek() != '"' && !_is_end()) {
		if (_peek() == '\n') scanner.line++;
		_advance();
	}

	if (_is_end()) return _make_error_token("Unterminated string.");

	_advance();
	return _make_token(TOKEN_STRING);
}

static Token _scan_number() {
	while (_is_digit(_peek())) _advance();

	if (_peek()=='.' && _is_digit(_peek_next())) {
		_advance();
		while (_is_digit(_peek())) _advance();
	}

	return _make_token(TOKEN_NUMBER);
}

static TokenType _make_keyword(
	int start,
	int length,
	const char* rest,
	TokenType type	
) {
	// check if scanner.start[start: start+length] == rest[:length]
	// return the corresponding token

	if (scanner.current-scanner.start == start+length &&
		memcmp(scanner.start+start, rest, length) == 0)	 {
		return _make_token(type);
	}
	return _make_token(TOKEN_IDENTIFIER);
}

static Token _scan_identifier() {
	while (_is_alpha(peek()) || _is_digit(peek())) _advance();

	switch (*scanner.start) {
		case 'a': return _make_keyword(1, 2, "nd", TOKEN_AND);
		case 'c': return _make_keyword(1, 4, "lass", TOKEN_CLASS);
		case 'e': return _make_keyword(1, 3, "lse", TOKEN_ELSE);
		case 'f':
			if (scanner.current-scanner.start > 1) {
				switch (scanner.start[1]) {
					case 'a': return _make_keyword(2, 3, "lse", TOKEN_FALSE);
					case 'o': return _make_keyword(2, 1, "r", TOKEN_FOR);
					case 'u': return _make_keyword(2, 1, "n", TOKEN_FUN);
				}
			}
			break;
		case 'i': return _make_keyword(1, 1, "f", TOKEN_IF);
		case 'n': return _make_keyword(1, 2, "il", TOKEN_NIL);
		case 'o': return _make_keyword(1, 1, "r", TOKEN_OR);
		case 'p': return _make_keyword(1, 4, "rint", TOKEN_PRINT);
		case 'r': return _make_keyword(1, 5, "eturn", TOKEN_RETURN);
		case 's': return _make_keyword(1, 4, "uper", TOKEN_SUPER);
		case 't':
			if (scanner.current-scanner.start > 1) {
				switch(scanner.start[1]) {
					case 'h': return _make_keyword(2, 2, "is", TOKEN_THIS);
					case 'r': return _make_keyword(2, 2, "ue", TOKEN_TRUE);
				}
			}
			break;
		case 'v': return _make_keyword(1, 2, "ar", TOKEN_VAR);
		case 'w': return _make_keyword(1, 4, "hile", TOKEN_WHILE);
	}

	return _make_token(TOKEN_IDENTIFIER);
}






