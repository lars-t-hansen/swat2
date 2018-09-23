use std::fmt;

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
    Fn(Box<FnDef>),
    Struct(Box<StructDef>)
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
    pub body:     Box<Block>,
    pub locals:   Option<Vec<(Id,Type)>>
}

#[derive(Debug)]
pub struct StructDef {
    pub name:   Id,
    pub fields: Vec<(Id,Type)>
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

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
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

// Unops usually have equal input and output types, but not for
// conversions.
#[derive(Clone, Copy, Debug)]
pub enum Unop {
    // Surface syntax that's removed by desugaring
    Neg,
    Not,
    BitNot,

    // Operators that are introduced by desugaring intrinsic calls
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
    Eqz,
    I32ToI64,
    U32ToI64,
    I64ToI32,
}

#[derive(Debug)]
pub struct Expr {
    // `ty` is stable after type checking.
    pub ty: Option<Type>,
    pub u:  Uxpr
}

#[derive(Debug)]
pub enum Uxpr {
    // `Block` is introduced by desugaring (for now).
    // `While` and `Loop` are removed by desugaring.
    // `Id`, `Assign`, and `Block` are removed by flattening.
    Void,
    NumLit(Number),
    NullLit,
    Id(Id),
    Deref{base:Box<Expr>, field:Id},
    New{ty_name:Id, values:Vec<(Id,Box<Expr>)>},
    If{test:Box<Expr>, consequent:Box<Block>, alternate:Box<Block>},
    While{test:Box<Expr>, body:Box<Block>},
    Loop{break_label:Id, body:Box<Block>},
    Break{label:Id},
    Block(Block),
    Binop{op:Binop, lhs:Box<Expr>, rhs:Box<Expr>},
    Unop{op:Unop, e:Box<Expr>},
    Assign{lhs:LValue, rhs:Box<Expr>},
    Call{name:Id, actuals:Vec<Box<Expr>>},

    // Introduced by desugaring.
    Iterate{break_label:Id, continue_label:Id, body:Box<Block>},

    // Introduced by flattening.
    Local(Id),
    Global(Id),
    SetLocal{name:Id, e:Box<Expr>},
    SetGlobal{name:Id, e:Box<Expr>},
    Sequence{ty:Option<Type>, body:Vec<Box<Expr>>},
    Drop(Box<Expr>)
}

#[derive(Debug)]
pub enum LValue {
    Id(Id),
    Field{base: Box<Expr>, field:Id}
}

#[derive(Clone, Copy, Debug)]
pub enum Number {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64)
}

// Utilities for AST construction and rewriting

pub fn box_void() -> Box<Expr> {
    Box::new(Expr{ ty: None, u: Uxpr::Void })
}

pub fn box_unop(ty:Option<Type>, op:Unop, e:Box<Expr>) -> Box<Expr> {
    Box::new(Expr{ ty, u: Uxpr::Unop{ op, e } })
}

pub fn box_binop(ty:Option<Type>, op:Binop, lhs:Box<Expr>, rhs:Box<Expr>) -> Box<Expr> {
    Box::new(Expr{ ty, u: Uxpr::Binop{ op, lhs, rhs } })
}

pub fn box_block(exprs:Vec<Box<Expr>>) -> Box<Block> {
    Box::new(Block{ ty: None, items: exprs.into_iter().map(|e| BlockItem::Expr(e)).collect() })
}

pub fn box_if(test:Box<Expr>, consequent:Box<Block>, alternate:Box<Block>) -> Box<Expr> {
    Box::new(Expr{ ty: None, u:  Uxpr::If{ test, consequent, alternate } })
}

pub fn box_empty_sequence() -> Box<Expr> {
    Box::new(Expr{ty: None, u:  Uxpr::Sequence{ty:None, body:vec![]}})
}

pub fn box_sequence(ty:Option<Type>, body:Vec<Box<Expr>>) -> Box<Expr> {
    Box::new(Expr{ ty, u: Uxpr::Sequence{ty, body}})
}

pub fn box_break(label:&Id) -> Box<Expr> {
    Box::new(Expr{ ty: None, u: Uxpr::Break{ label: label.clone() } })
}

pub fn box_iterate(break_label:&Id, continue_label:&Id, body:Box<Block>) -> Box<Expr> {
    Box::new(Expr{ ty:None, u:Uxpr::Iterate{ break_label: break_label.clone(),
                                             continue_label: continue_label.clone(),
                                             body } })
}

pub fn box_get_local(ty:Option<Type>, name:&Id) -> Box<Expr> {
    Box::new(Expr{ ty, u: Uxpr::Local(name.clone()) })
}

pub fn box_get_global(ty:Option<Type>, name:&Id) -> Box<Expr> {
    Box::new(Expr{ ty, u: Uxpr::Global(name.clone()) })
}

pub fn box_set_local(name:&Id, rhs:Box<Expr>) -> Box<Expr> {
    Box::new(Expr{ ty: None,
                   u:  Uxpr::SetLocal{name: name.clone(),
                                      e:    rhs} })
}

pub fn box_set_global(name:&Id, rhs:Box<Expr>) -> Box<Expr> {
    Box::new(Expr{ ty: None,
                   u:  Uxpr::SetGlobal{name: name.clone(),
                                       e:    rhs} })
}

pub fn box_intlit(n:i64, ty:Type) -> Box<Expr> {
    match ty {
        Type::I32 => Box::new(Expr{ ty: Some(ty), u: Uxpr::NumLit(Number::I32(n as i32)) }),
        Type::I64 => Box::new(Expr{ ty: Some(ty), u: Uxpr::NumLit(Number::I64(n)) }),
        Type::F32 => Box::new(Expr{ ty: Some(ty), u: Uxpr::NumLit(Number::F32(n as f32)) }),
        Type::F64 => Box::new(Expr{ ty: Some(ty), u: Uxpr::NumLit(Number::F64(n as f64)) }),
        _         => panic!("Can't happen")
    }
}

pub fn box_drop(e:Box<Expr>) -> Box<Expr> {
    Box::new(Expr{ty:None, u: Uxpr::Drop(e)})
}

// Type utilities

pub fn match_parameters(formals:&Vec<Type>, actuals:&Vec<Box<Expr>>) -> bool {
    if actuals.len() != formals.len() {
        return false;
    }
    for i in 0..actuals.len() {
        if !is_same_type(Some(formals[i]), actuals[i].ty) {
            return false;
        }
    }
    true
}

pub fn is_same_type(t1:Option<Type>, t2:Option<Type>) -> bool {
    match (t1, t2) {
        (None, None) => true,
        (None, _)    => false,
        (_, None)    => false,
        (Some(t1), Some(t2)) => t1 == t2
    }
}

pub fn is_int_type(t1:Option<Type>) -> bool {
    match t1 {
        Some(Type::I32) | Some(Type::I64) => true,
        _ => false
    }
}

pub fn is_i32_type(t1:Option<Type>) -> bool {
    is_same_type(t1, Some(Type::I32))
}

pub fn is_float_type(t1:Option<Type>) -> bool {
    match t1 {
        Some(Type::F32) | Some(Type::F64) => true,
        _ => false
    }
}

pub fn is_num_type(t1:Option<Type>) -> bool {
    is_int_type(t1) || is_float_type(t1)
}

pub fn is_value_type(t1:Option<Type>) -> bool {
    !t1.is_none()
}

pub fn is_ref_type(t1:Option<Type>) -> bool {
    match t1 {
        Some(Type::AnyRef) => true,
        _ => false
    }
}
