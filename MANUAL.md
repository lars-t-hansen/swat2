Roughly correct grammar.

Note that the compiler currently handles only a single module per file
and does not handle JS sections.  Working on it...

A general, temporary, semantic restriction is that struct types cannot
be exposed from the module: can't appear as types on pub/extern
globals; can't appear in signatures of pub/extern functions.

See TODO for a list of arbitrary restrictions that we want to lift ASAP.



Program ::= (Module | JS)*

Module ::= "module" "{" (Global | Function | Struct)* "}"

Global ::= "pub"? ("var" | "const") IdAndType "=" ConstExpr ";"
         | "extern" ("var" | "const") IdAndType ";"

Function ::= "pub"? "fn" Id "(" Formals ")" ("->" Type)? Block
           | "extern" "fn" Id" "(" Formals ")" ("->" Type)? ";"

Struct ::= "struct" Id "{" Formals "}"

Formals ::= (IdAndType ("," IdAndType)*)?
IdAndType ::= Id ":" Type

Type ::= "i32" | "i64" | "f32" | "f64" | "anyref" | Id | "[" Type "]"

   Here "Id" is a struct name and the type is pointer-to-instance-of-Id

Block ::= "{" BlockItems "}"

BlockItems ::= (BlockItem (";" BlockItem)*) ";"?

   Some BlockItems must be terminated by semicolon even if last, notably "let".

BlockItem ::= "let" IdAndType "=" Expr
	    | "break" Id
            | LValue "=" Expr
            | Expr

ConstExpr ::= NumLit | NullLit

Expr ::= Expr Binop Expr
       | Expr "is" Type
       | Expr "as" Type
       | Unop Expr
       | Expr "." Id
       | Id "(" Exprs ")"
       | Expr "[" Expr "]"
       | "(" Expr ")"
       | "if" Expr Block "else" Block
       | "if" Expr Block
       | "while" Expr Block
       | "loop" Id Block
       | "new" Id "{" Inits "}"
       | "new" "[" Type "]" "(" Expr ")"
       | Id
       | NumLit
       | IntLit

Inits ::= (Init ("," Init)*)?
Init ::= Id ":" Expr

LValue ::= Id | Expr "." Id | Expr "[" Expr "]"

Binop ::= + | - | * | ...
  
  Binops follow Rust in their precedence.  Generally, binop has lower precedence than
  unop, which has lower precedence than type-op, which has lower precedence than other
  primary expressions.

Unop ::= ! | ~ | - 

  Many unops are additionally expressed as intrinsics.

NumLit ::= Conventional number syntax with optional suffix i/I/l/L/f/F/d/D
           to denote specific representation: i=i32, l=i64, f=f32, d=f64,
           eg, 37L is an i64, 3.14e-3f is an f32.
           If no suffix then integers default to i32, floats to f64.  If
           the value doesn't fit in its designated type then there's a syntax 
           error.

NullLit ::= "null"

Comment ::= "//" until the end of line

JS ::= "%%JS<!<" any text ">!>"
