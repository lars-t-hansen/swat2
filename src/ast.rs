/* -*- fill-column: 80 -*- */

use std::fmt;
use std::cell::RefCell;
use std::collections::HashMap;

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
    pub name:  Id,
    pub items: Vec<ModItem>
}

#[derive(Debug)]
pub enum ModItem {
    Var(Box<GlobalDef>),
    Fn(Box<FunctionDef>),
    Struct(Box<StructDef>)
}

#[derive(Debug)]
pub struct GlobalDef {
    pub mutable:  bool,
    pub imported: bool,
    pub exported: bool,
    pub name:     Id,
    pub ty:       Type,
    pub init:     Box<Expr>
}

#[derive(Debug)]
pub struct FunctionDef {
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
    pub ty:   Type,
    pub init: Box<Expr>
}

// It's important that Id is Copy so that Type can be Copy.  To that end, we
// store a name index in the Id, so as not to carry an RC value around and
// require it to be cloned or expensively copied everywhere.
//
// As a side effect, environments can map Ids directly, not having to go via
// strings, and Id comparisons are generally cheaper.

thread_local! {
    // IdTable maps strings to IDs
    static IdTable: RefCell<HashMap<String,usize>> = RefCell::new(HashMap::new());

    // IdNames maps IDs to strings
    static IdNames: RefCell<Vec<String>> = RefCell::new(vec![]);

    // GensymCounter is used to generate unique names
    static GensymCounter: RefCell<usize> = RefCell::new(1000);
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Id
{
    name: usize                 // Reference into a string table
}

impl Id {
    pub fn intern(name:&str) -> Id {
        IdTable.with(|tbl| {
            if let Some(k) = tbl.borrow().get(name) {
                return Id { name: *k };
            }

            IdNames.with(|names| {
                let mut names = names.borrow_mut();
                let k = names.len();
                names.push(name.to_string());
                tbl.borrow_mut().insert(name.to_string(), k);
                Id { name: k }
            })
        })
    }

    pub fn gensym(tag:&str) -> Id {
        GensymCounter.with(|counter| {
            let mut counter = counter.borrow_mut();
            let k = *counter;
            *counter += 1;
            Id::intern(&format!("_{}_{}", tag, k))
        })
    }

    pub fn is(&self, x:&str) -> bool {
        *self == Id::intern(x)
    }

    pub fn name(&self) -> String {
        IdNames.with(|names| { names.borrow()[self.name].clone() })
    }
}

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

// Not PartialEq so that we can avoid accidentally comparing types with `==`.

#[derive(Copy, Clone, Debug)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    AnyRef,
    // RawRef comes from the parser, Id could be anything; it is eliminated by
    // the type checker, and type comparison algorithms will abort on
    // encountering it.
    RawRef(Id),
    // CookedRef is produced by the type checker, Id is known to reference a
    // global type defn that is not shadowed by a local binding, and type
    // comparison need not consider environments.
    CookedRef(Id)
}

pub fn fmt_type(t:Option<Type>) -> String
{
    match t {
        None => "void".to_string(),
        Some(x) => {
            match x {
                Type::I32 => "I32".to_string(),
                Type::I64 => "I64".to_string(),
                Type::F32 => "F32".to_string(),
                Type::F64 => "F64".to_string(),
                Type::AnyRef => "anyref".to_string(),
                Type::RawRef(n) => format!("(rawref {})", n),
                Type::CookedRef(n) => format!("(ref {})", n)
            }
        }
    }
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
    // Surface syntax that's removed by desugaring
    Neg,
    Not,
    BitNot,

    // Operators that are introduced by desugaring intrinsic calls or operators
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
    IsNull,
    I32ToI64,
    U32ToI64,
    I64ToI32,
}

#[derive(Clone, Copy, Debug)]
pub enum Typeop {
    Is,
    As,
}

#[derive(Debug)]
pub struct Expr {
    // `ty` is stable after type checking.
    pub ty: Option<Type>,
    pub u:  Uxpr
}

#[derive(Debug)]
pub enum Uxpr {
    // `While` and `Loop` and `TypeOp` are removed by desugaring.
    // `Id`, `Assign`, `Block`, and `Deref` are removed by flattening.
    Void,
    NullLit{ty: Type},
    NumLit{value: Number},
    Id{name: Id},
    Deref{base: Box<Expr>, field: Id},
    New{ty_name: Id, values: Vec<(Id,Box<Expr>)>}, // "initializers" would be better than "values"
    If{test: Box<Expr>, consequent: Box<Block>, alternate: Box<Block>},
    While{test: Box<Expr>, body: Box<Block>},
    Loop{break_label: Id, body: Box<Block>},
    Break{label: Id},
    Binop{op: Binop, lhs: Box<Expr>, rhs: Box<Expr>},
    Unop{op: Unop, opd: Box<Expr>},
    Typeop{op: Typeop, lhs: Box<Expr>, rhs: Type},
    Assign{lhs: LValue, rhs: Box<Expr>},
    Call{name: Id, actuals: Vec<Box<Expr>>},

    // Introduced by desugaring.
    Block{block: Block},
    Iterate{break_label: Id, continue_label: Id, body: Box<Block>},
    ExactFallibleUnboxAnyRef{to: Type, value: Box<Expr>},
    DowncastFailed,

    // Introduced by flattening.
    GetLocal{name: Id},
    GetGlobal{name: Id},
    SetLocal{name: Id, value: Box<Expr>},
    SetGlobal{name: Id, value: Box<Expr>},
    GetField{base: Box<Expr>, field: Id},
    SetField{base: Box<Expr>, field: Id, value: Box<Expr>},
    Sequence{ty: Option<Type>, body: Vec<Box<Expr>>},
    Drop{value: Box<Expr>}
}

#[derive(Debug)]
pub enum LValue {
    Id{name:Id},
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

pub fn box_let(name: &Id, ty: Type, init: Box<Expr>) -> Box<LetDefn> {
    Box::new(LetDefn { name: *name, ty, init })
}

pub fn box_block(exprs:Vec<Box<Expr>>) -> Box<Block> {
    Box::new(Block{ ty: None, items: exprs.into_iter().map(|e| BlockItem::Expr(e)).collect() })
}


pub fn box_void() -> Box<Expr> {
    Box::new(Expr{ ty: None, u: Uxpr::Void })
}

pub fn box_id(ty:Option<Type>, name:&Id) -> Box<Expr> {
    Box::new(Expr{ ty, u: Uxpr::Id{name: *name} })
}

pub fn box_unop(ty:Option<Type>, op:Unop, opd:Box<Expr>) -> Box<Expr> {
    Box::new(Expr{ ty, u: Uxpr::Unop{ op, opd } })
}

pub fn box_binop(ty:Option<Type>, op:Binop, lhs:Box<Expr>, rhs:Box<Expr>) -> Box<Expr> {
    Box::new(Expr{ ty, u: Uxpr::Binop{ op, lhs, rhs } })
}

pub fn box_typeop(ty:Option<Type>, op:Typeop, lhs:Box<Expr>, rhs:Type) -> Box<Expr> {
    Box::new(Expr{ ty, u: Uxpr::Typeop{ op, lhs, rhs } })
}

pub fn box_downcast(ty:Option<Type>, to:Type, value:Box<Expr>) -> Box<Expr> {
    Box::new(Expr { ty, u: Uxpr::ExactFallibleUnboxAnyRef{ to, value } })
}

pub fn box_downcast_failed() -> Box<Expr> {
    Box::new(Expr { ty: None, u:  Uxpr::DowncastFailed })
}

pub fn box_new(ty:Option<Type>, ty_name:&Id, values: Vec<(Id, Box<Expr>)>) -> Box<Expr> {
    Box::new(Expr { ty, u: Uxpr::New{ ty_name: *ty_name, values } })
}

pub fn box_block_expr(ty: Option<Type>, items: Vec<BlockItem>) -> Box<Expr> {
    Box::new(Expr { ty, u: Uxpr::Block{block: Block{ ty, items }}})
}

pub fn box_if(test:Box<Expr>, consequent:Box<Block>, alternate:Box<Block>) -> Box<Expr> {
    Box::new(Expr{ ty: None, u:  Uxpr::If{ test, consequent, alternate } })
}

pub fn box_empty_sequence() -> Box<Expr> {
    Box::new(Expr{ty: None, u:  Uxpr::Sequence{ty:None, body:vec![]}})
}

pub fn box_sequence(ty:Option<Type>, body:Vec<Box<Expr>>) -> Box<Expr> {
    Box::new(Expr{ ty: ty.clone(), u: Uxpr::Sequence{ty, body}})
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
    Box::new(Expr{ ty, u: Uxpr::GetLocal{name: *name} })
}

pub fn box_get_global(ty:Option<Type>, name:&Id) -> Box<Expr> {
    Box::new(Expr{ ty, u: Uxpr::GetGlobal{name: *name} })
}

pub fn box_set_local(name:&Id, value:Box<Expr>) -> Box<Expr> {
    Box::new(Expr{ ty: None,
                   u:  Uxpr::SetLocal{name: *name, value} })
}

pub fn box_set_global(name:&Id, value:Box<Expr>) -> Box<Expr> {
    Box::new(Expr{ ty: None,
                   u:  Uxpr::SetGlobal{name: *name, value} })
}

pub fn box_intlit(n:i64, ty:Type) -> Box<Expr> {
    match ty {
        Type::I32 => Box::new(Expr{ ty: Some(ty), u: Uxpr::NumLit{value: Number::I32(n as i32)}}),
        Type::I64 => Box::new(Expr{ ty: Some(ty), u: Uxpr::NumLit{value: Number::I64(n)}}),
        Type::F32 => Box::new(Expr{ ty: Some(ty), u: Uxpr::NumLit{value: Number::F32(n as f32)}}),
        Type::F64 => Box::new(Expr{ ty: Some(ty), u: Uxpr::NumLit{value: Number::F64(n as f64)}}),
        _         => panic!("Can't happen")
    }
}

pub fn box_drop(e:Box<Expr>) -> Box<Expr> {
    Box::new(Expr{ty:None, u: Uxpr::Drop{value: e}})
}

// Type utilities

pub fn match_parameters(formals:&Vec<Type>, actuals:&Vec<Box<Expr>>) -> bool {
    if actuals.len() != formals.len() {
        return false;
    }
    for i in 0..actuals.len() {
        if !is_assignable_type(Some(formals[i]), actuals[i].ty) {
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
        (Some(Type::I32), Some(Type::I32)) => true,
        (Some(Type::I64), Some(Type::I64)) => true,
        (Some(Type::F32), Some(Type::F32)) => true,
        (Some(Type::F64), Some(Type::F64)) => true,
        (Some(Type::AnyRef), Some(Type::AnyRef)) => true,
        (Some(Type::CookedRef(name1)), Some(Type::CookedRef(name2))) => name1 == name2,
        (Some(Type::RawRef(_)), Some(_)) => unreachable!(),
        (Some(_), Some(Type::RawRef(_))) => unreachable!(),
        (_, _) => false
    }
}

// t2 assignable to t1 with implicit upcast (not symmetric)

pub fn is_assignable_type(t1:Option<Type>, t2:Option<Type>) -> bool {
    match (t1, t2) {
        (None, None) => true,
        (None, _)    => false,
        (_, None)    => false,
        (Some(Type::I32), Some(Type::I32)) => true,
        (Some(Type::I64), Some(Type::I64)) => true,
        (Some(Type::F32), Some(Type::F32)) => true,
        (Some(Type::F64), Some(Type::F64)) => true,
        (Some(Type::AnyRef), Some(Type::AnyRef)) => true,
        (Some(Type::AnyRef), Some(Type::CookedRef(_))) => true,
        (Some(Type::CookedRef(name1)), Some(Type::CookedRef(name2))) => name1 == name2,
        (Some(Type::RawRef(_)), Some(_)) => unreachable!(),
        (Some(_), Some(Type::RawRef(_))) => unreachable!(),
        (_, _) => false
    }
}

// t1 and t2 are compatible (symmetric)

pub fn is_compatible_type(t1:Option<Type>, t2:Option<Type>) -> bool {
    match (t1, t2) {
        (None, None) => true,
        (None, _)    => false,
        (_, None)    => false,
        (Some(Type::I32), Some(Type::I32)) => true,
        (Some(Type::I64), Some(Type::I64)) => true,
        (Some(Type::F32), Some(Type::F32)) => true,
        (Some(Type::F64), Some(Type::F64)) => true,
        (Some(Type::AnyRef), Some(Type::AnyRef)) => true,
        (Some(Type::AnyRef), Some(Type::CookedRef(_))) => true,
        (Some(Type::CookedRef(_)), Some(Type::AnyRef)) => true,
        (Some(Type::CookedRef(name1)), Some(Type::CookedRef(name2))) => name1 == name2,
        (Some(Type::RawRef(_)), Some(_)) => unreachable!(),
        (Some(_), Some(Type::RawRef(_))) => unreachable!(),
        (_, _) => false
    }
}

pub fn merge_compatible_types(t1:Option<Type>, t2:Option<Type>) -> Option<Type> {
    match (t1, t2) {
        (None, None) => t1,
        (Some(Type::I32), Some(Type::I32)) => t1,
        (Some(Type::I64), Some(Type::I64)) => t1,
        (Some(Type::F32), Some(Type::F32)) => t1,
        (Some(Type::F64), Some(Type::F64)) => t1,
        (Some(Type::AnyRef), Some(Type::AnyRef)) => t1,
        (Some(Type::AnyRef), Some(Type::CookedRef(_))) => t1,
        (Some(Type::CookedRef(_)), Some(Type::AnyRef)) => t2,
        (Some(Type::CookedRef(_)), Some(Type::CookedRef(_))) => t1,
        (Some(Type::RawRef(_)), Some(_)) => unreachable!(),
        (Some(_), Some(Type::RawRef(_))) => unreachable!(),
        (_, _) => unreachable!()
    }
}

pub fn is_int_type(t1:Option<Type>) -> bool {
    match t1 {
        None => false,
        Some(Type::I32) | Some(Type::I64) => true,
        Some(Type::F32) | Some(Type::F64) => false,
        Some(Type::AnyRef) => false,
        Some(Type::CookedRef(_)) => false,
        Some(Type::RawRef(_)) => unreachable!()
    }
}

pub fn is_i32_type(t1:Option<Type>) -> bool {
    match t1 {
        None => false,
        Some(Type::I32) => true,
        Some(Type::I64) => false,
        Some(Type::F32) | Some(Type::F64) => false,
        Some(Type::AnyRef) => false,
        Some(Type::CookedRef(_)) => false,
        Some(Type::RawRef(_)) => unreachable!()
    }
}

pub fn is_float_type(t1:Option<Type>) -> bool {
    match t1 {
        None => false,
        Some(Type::I32) | Some(Type::I64) => false,
        Some(Type::F32) | Some(Type::F64) => true,
        Some(Type::AnyRef) => false,
        Some(Type::CookedRef(_)) => false,
        Some(Type::RawRef(_)) => unreachable!()
    }
}

pub fn is_num_type(t1:Option<Type>) -> bool {
    is_int_type(t1) || is_float_type(t1)
}

pub fn is_value_type(t1:Option<Type>) -> bool {
    !t1.is_none()
}

pub fn is_ref_or_anyref_type(t1:Option<Type>) -> bool {
    match t1 {
        None => false,
        Some(Type::I32) | Some(Type::I64) => false,
        Some(Type::F32) | Some(Type::F64) => false,
        Some(Type::AnyRef) => true,
        Some(Type::CookedRef(_)) => true,
        Some(Type::RawRef(_)) => unreachable!()
    }
}

pub fn is_ref_type(t1:Option<Type>) -> bool {
    match t1 {
        None => false,
        Some(Type::I32) | Some(Type::I64) => false,
        Some(Type::F32) | Some(Type::F64) => false,
        Some(Type::AnyRef) => false,
        Some(Type::CookedRef(_)) => true,
        Some(Type::RawRef(_)) => unreachable!()
    }
}
