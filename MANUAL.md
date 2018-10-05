// Rough grammar, see src/grammar.lalrpop for the true definition

Program ::= Module | JS
Module ::= "module" "{" ModItem* "}"
ModItem ::= Global | Function | Struct
Struct ::= "struct" Id "{" Formals "}"
Global ::= "pub"? VarOrConst IdAndType "=" ConstExpr ";"
         | "extern" VarOrConst IdAndType ";"
VarOrConst ::= "var" | "const"
Function ::= "pub"? "fn" Id "(" Formals ")" ("->" Type)? Block
           | "extern" "fn" Id" "(" Formals ")" ("->" Type)? ";"
Formals ::= (IdAndType ("," IdAndType)*)?
IdAndType ::= Id ":" Type
Type ::= "i32" | "i64" | "f32" | "f64" | "anyref" | Id
   Here "Id" is a struct name and the type is pointer-to-instance-of-Id
Block ::= "{" BlockItems "}"
BlockItems ::= (BlockItem (";" BlockItem)*) ";"?
BlockItem ::= "let" IdAndType "=" Expr
            | Expr
ConstExpr ::= NumLit | NullLit
Expr ::= Expr Binop Expr
       | Expr "is" Type
       | Expr "as" Type
       | Unop Expr
       | Expr "." Id
       | LValue "=" Expr
       | Id "(" Exprs ")"
       | "(" Expr ")"
       | "if" Expr Block "else" Block
       | "while" Expr Block
       | "loop" Id Block
       | "break" Id
       | Id
       | NumLit
       | IntLit
LValue ::= Id | Expr "." Id
Binop ::= + | - | * | ...
Unop ::= ! | ~ | - | ...
NumLit ::= conventional number syntax possibly with suffix i/I/l/L/f/F/d/D
           to denote specific representation
NullLit ::= "null"

JS ::= "%%JS<!<" any text ">!>"

