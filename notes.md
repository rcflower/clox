## Parser

### identifierConstant

really `makeConstant(OBJ_VAL(copyString(token->start, token->length)))`
makeConstant is a cover for addConstant to the current chunk (of the compiler inside the closure).  
Add a string to the constant pool in chunk.  
Return the position in pool.

### parseVariable
Read and **declare** a variable.
* Consume the identifier token.  
* If current scope > 0, add to locals  and return 0.  
* Otherwise, add to the globals using `identifierConstant`.

### defineVariable
parameter: `uint8_t global`. no return.
* If scope > 0, mark the newly added local as initialized (change the scope from -1 to the current scope) and return.  
* If global, emit `OP_DEFINE_GLOBAL` and `global` (the position of the assigning value in the constant pool).

## Functions

### ObjFunction

```
typedef struct {
  Obj obj;
  int arity;
  Chunk chunk;
  ObjString* name;
} ObjFunction;
```

### funDeclaration

`parseVariable` reads and declares the function name.
The major part is `function(TYPE)`. Finally call `defineVariable` for the function. 

### function
no parameter, no return.

* Open a new compiler and new scope, and change the current compiler.
* Read the parameters using `parseVariable` and put them into locals. Intialize each parameter using `defineVariable`. Update `curCompiler->function`.
* Enter `block()`
* `fun = endCompiler()`. Now `curCompiler` is the **enclosing**.
* emit `OP_CLOSURE`, `makeConstant(fun)` (to the chunk in the enclosing function). Now the function obj is stored in the constant pool which contains everything it needs in its own chunk.
* For each upvalue, emit `isLocal`, `index`.

### VM: OP_CLOSURE

* If reading `OP_CLOSURE`, read the next constant which is an ObjFunction, called `function`.
* Make a new closure initialized by `function`, and push this closure into stack.
* Read `isLocal`, `index`'s for all upvalues.













