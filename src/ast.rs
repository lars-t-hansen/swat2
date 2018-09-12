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
    Func,
    Class,
    Virtual
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

#[derive(Debug)]
pub enum Expr {
    NumLit(String)
}
