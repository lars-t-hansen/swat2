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
    Var(Box<GlobalVar>),
    Fn(Box<FnDef>)
}

#[derive(Debug)]
pub struct GlobalVar {
    pub mutable:bool,
    pub imported:bool,
    pub exported:bool,
    pub name:Id,
    pub ty:Type,
    pub init:Box<Expr>
}

#[derive(Debug)]
pub struct FnDef {
    pub imported: bool,
    pub exported: bool,
    pub name:     Id,
    pub formals:  Vec<(Id,Type)>,
    pub retn:     Option<Type>,
    pub body:     Box<Block>
}

#[derive(Debug)]
pub struct Block {
    pub items: Vec<BlockItem>
}

#[derive(Debug)]
pub enum BlockItem {
    Let(Box<LetDefn>),
    Expr(Box<Expr>)
}

#[derive(Debug)]
pub struct LetDefn {
    pub name: Id,
    pub ty: Type,
    pub init: Box<Expr>
}

#[derive(Clone, Debug)]
pub struct Id {
    pub name: String
}

impl Id {
    pub fn is(&self, x:&str) -> bool {
        self.name == x
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    AnyRef
}

#[derive(Clone, Copy, Debug)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    UDiv,
    Rem,
    URem,
    ShiftLeft,
    ShiftRight,
    UShiftRight,
    BitAnd,
    BitOr,
    BitXor,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    Equal,
    NotEqual,
    ULess,
    ULessOrEqual,
    UGreater,
    UGreaterOrEqual,
    RotLeft,
    RotRight,
    Copysign,
}

#[derive(Clone, Copy, Debug)]
pub enum Unop {
    Neg,
    Not,
    BitNot,
    Clz,
    Ctz,
    Popcnt,
    Extend8,
    Extend16,
    Extend32,
    Sqrt,
    Ceil,
    Floor,
    Nearest,
    Trunc,
    // conversions here
}

#[derive(Debug)]
pub enum Expr {
    If{test:Box<Expr>, consequent:Box<Block>, alternate:Box<Block>},
    While{test:Box<Expr>, body:Box<Block>},
    Binop{op:Binop, lhs:Box<Expr>, rhs:Box<Expr>},
    Unop{op:Unop, e:Box<Expr>},
    Assign{lhs:LValue, rhs:Box<Expr>},
    Call{name:Id, actuals:Vec<Box<Expr>>},
    Id(Id),
    NumLit(Number),
    Void
}

#[derive(Debug)]
pub enum LValue {
    Id(Id)
}

#[derive(Clone, Copy, Debug)]
pub enum Number {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64)
}
