Program ::= Module | JS
Module ::= "module" "{" ModItem* "}"
ModItem ::= Global | Function
Global ::= "pub"? "var" IdAndType "=" ConstExpr ";"
         | "extern" "var" IdAndType ";"
Function ::= "pub"? "fn" Id "(" Formals ")" ("->" Type)? Block
           | "extern" "fn" Id" "(" Formals ")" ("->" Type)? ";"
Formals ::= (IdAndType ("," IdAndType)*)?
IdAndType ::= Id ":" Type
Type ::= "i32" | "i64" | "f32" | "f64" | "anyref"
Block ::= "{" BlockItems "}"
BlockItems ::= (BlockItem (";" BlockItem)*) ";"?
BlockItem ::= "let" IdAndType "=" Expr
            | Expr
ConstExpr ::= NumLit | NullLit
Expr ::= Expr Binop Expr
       | Unop Expr
       | Id "(" Exprs ")"
       | "(" Expr ")"
       | "if" Expr Block "else" Block
       | "while" Expr Block
       | "loop" Id Block
       | "break" Id
       | Id
       | NumLit
       | IntLit
NumLit ::= conventional number syntax possibly with suffix i/I/l/L/f/F/d/D
           to denote specific representation
NullLit ::= "null"

JS ::= "%%JS<!<" any text ">!>"

