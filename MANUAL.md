// -*- fill-column: 80 -*-

Roughly correct grammar.

Note that the compiler currently does not handle JS sections.  Working on it...

A general, temporary, semantic restriction is that struct types cannot be
exposed from the module: can't appear as types on pub/extern globals; can't
appear in signatures of pub/extern functions.

The restriction also applies to arrays.

See TODO for a list of arbitrary restrictions that we want to lift ASAP.



Program ::= (Module | JS)*

  There can be at most one Module per file, it ends up in a .wast or .wasm file
  with the same base name as the input .swat file.

  The JS sections are concatenated in order into a .js file with the same base
  name as the input .swat file.

  The .js file is always generated even if there are no JS sections in the
  Program.
  
Module ::= "module" Id "{" (Global | Function | Struct)* "}"

Global ::= "pub"? ("var" | "const") IdAndType "=" ConstExpr ";"
         | "extern" ("var" | "const") IdAndType ";"

  BUG: Arrays are not nullable, but new-array expressions are not const
  expression, thus there can be no globals of array type at present, this is a
  problem.  This is quasi-workaroundable by creating a global of a nullable
  struct type that references the array.

  We really should have the default value of an array type be an empty array
  of that type, and we should set that up in the start function.

Function ::= "pub"? "fn" Id "(" Formals ")" ("->" Type)? Block
           | "extern" "fn" Id" "(" Formals ")" ("->" Type)? ";"

Struct ::= "struct" Id "{" Formals "}"

Formals ::= (IdAndType ("," IdAndType)*)?
IdAndType ::= Id ":" Type

Type ::= "i32" | "i64" | "f32" | "f64" | "anyref" | Id | "[" Type "]"

   Here "Id" is a struct name and the type is pointer-to-instance-of-Id

Block ::= "{" BlockItems "}"

BlockItems ::= (BlockItem (";"? BlockItem)*) ";"?

   A BlockItem that is not last must be separated from the next with a
   semicolon.  However there are some arbitrary adjustments:

   - Some BlockItems must be terminated by semicolon even if last, notably "let".

   - Some BlockItems must not be terminated by semicolon even if not last,
     notably those that carry blocks ("if", "while", "loop").

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
  
  Binops follow Rust in their precedence.  Generally, binop has lower precedence
  than unop, which has lower precedence than type-op, which has lower precedence
  than other primary expressions.

Unop ::= ! | ~ | - 

  Many unops are additionally expressed as intrinsics.

NumLit ::= Conventional number syntax with optional suffix i/I/l/L/f/F/d/D to
           denote specific representation: i=i32, l=i64, f=f32, d=f64, eg, 37L
           is an i64, 3.14e-3f is an f32.  If no suffix then integers default to
           i32, floats to f64.  If the value doesn't fit in its designated type
           then there's a syntax error.

NullLit ::= "null"

Comment ::= "//" until the end of line

JS ::= "<!<" any text ">!>"
