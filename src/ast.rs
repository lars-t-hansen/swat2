#[derive(Debug)]
pub struct Program {
    pub items: Vec<TopItem>
}

#[derive(Debug)]
pub enum TopItem {
    Mod(Module),
    Js(String)
}

#[derive(Debug)]
pub struct Module {
    pub name: Id,
    pub items: Vec<ModItem>
}

#[derive(Debug)]
pub enum ModItem {
    Var{mutable:bool,
        imported:bool,
        exported:bool,
        name:Id,
        ty:Type,
        init:Box<Expr>},
    Fn{imported: bool,
       exported: bool,
       name:     Id,
       formals:  Vec<(Id,Type)>,
       retn:     Option<Type>,
       body:     Box<Block>
    },
    Class,
    Virtual
}

#[derive(Debug)]
pub struct Block {
    pub items: Vec<BlockItem>
}

#[derive(Debug)]
pub enum BlockItem {
    Let{ name: Id, ty: Type, init: Box<Expr> },
    Expr(Box<Expr>),
    Semi
}

#[derive(Clone, Debug)]
pub struct Id {
    pub name: String
}

#[derive(Clone, Copy, Debug)]
pub enum Type {
    I32,
    I64,
    F32,
    F64
}

#[derive(Clone, Copy, Debug)]
pub enum Binop {
    Add,
    Sub,
    Less
}

#[derive(Debug)]
pub enum Expr {
    If{test:Box<Expr>, consequent:Box<Block>, alternate:Box<Block>},
    Binop{op:Binop, lhs:Box<Expr>, rhs:Box<Expr>},
    Call{name:Id, actuals:Vec<Box<Expr>>},
    Id(Id),
    NumLit(String)
}
