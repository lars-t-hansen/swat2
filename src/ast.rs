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
    pub locals:   Vec<LocalItem>,
    pub body:     Box<Block>
}

#[derive(Debug)]
pub struct LocalItem {
    pub name: Id,               // What the source program calls it
    pub aka:  Id,               // What we call it in Local and Global nodes
    pub ty:   Type              // Its type
}

#[derive(Debug)]
pub struct Block {
    pub items: Vec<BlockItem>,
    pub ty:    Option<Type>
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

#[derive(Clone, Debug, PartialEq)]
pub struct Id {
    pub name: String
}

impl Id {
    pub fn is(&self, x:&str) -> bool {
        self.name == x
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    AnyRef
}

// Binops have equal operand types and result type
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

// Unops have equal operand type and result type
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
}

// ConvOps have different input and output types
pub enum ConvOp {
    I32ToI64,
    U32ToI64,
    I64ToI32,
    F32ToF64,
    F64ToF32,
    F32ToI32Bits,
    F64ToI64Bits,
    I32BitsToF32,
    I64BitsToF64,
    // and many, many more
}

#[derive(Debug)]
pub struct Expr {
    pub ty: Option<Type>,
    pub u:  Uxpr
}

#[derive(Debug)]
pub enum Uxpr {
    Void,
    NumLit(Number),
    Id(Id),                     // Used before xform
    Local(Id),                  // Used after xform
    Global(Id),                 // Used after xform
    If{test:Box<Expr>, consequent:Box<Block>, alternate:Box<Block>},
    While{test:Box<Expr>, body:Box<Block>},
    Binop{op:Binop, lhs:Box<Expr>, rhs:Box<Expr>},
    Unop{op:Unop, e:Box<Expr>},
    Assign{lhs:LValue, rhs:Box<Expr>},
    Call{name:Id, actuals:Vec<Box<Expr>>},
}

#[derive(Debug)]
pub enum LValue {
    Id(Id),                     // Used before xform
    Local(Id),                  // Used after xform
    Global(Id)                  // Used after xform
}

#[derive(Clone, Copy, Debug)]
pub enum Number {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64)
}
