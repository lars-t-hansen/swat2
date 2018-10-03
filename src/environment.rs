// -*- fill-column: 80 -*-

use ast::{Binop, FunctionDef, GlobalDef, Id, ModItem, StructDef, Type, Unop};
use std::clone::Clone;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::rc::Rc;

pub type Signature = (Vec<Type>, Option<Type>);
pub type Struct = (Id, Vec<(Id,Type)>);
pub type Intrinsic = Vec<Rc<Signature>>;

#[derive(Clone, Copy)]
pub enum Intrin {
    Unop(Unop),
    Binop(Binop)
}

#[derive(Clone)]
pub enum Binding<T : Clone> {
    Global(bool, Type),
    Function(Rc<Signature>),
    Struct(Rc<Struct>),
    Intrinsic(Rc<Intrinsic>, Intrin),
    Local(T),
    Label
}

pub struct IntrinsicEnv<T : Clone>
{
    intrinsics:      HashMap<Id, (Rc<Intrinsic>, Intrin)>,
    binding_payload: PhantomData<T>
}

impl<T> IntrinsicEnv<T>
    where T : Clone
{
    pub fn new() -> IntrinsicEnv<T> {
        let mut intrinsics = HashMap::new();

        // FIXME: max, min, abs are floating intrinsics but not integer intrinsics...
        // FIXME: extendK instructions not reflected here...
        // FIXME: many more numeric conversions

        let i_to_i = Rc::new(vec![Rc::new((vec![Type::I32], Some(Type::I32))),
                                  Rc::new((vec![Type::I64], Some(Type::I64)))]);
        let i_to_l = Rc::new(vec![Rc::new((vec![Type::I32], Some(Type::I64)))]);
        let l_to_i = Rc::new(vec![Rc::new((vec![Type::I64], Some(Type::I32)))]);
        let ii_to_i = Rc::new(vec![Rc::new((vec![Type::I32, Type::I32], Some(Type::I32))),
                                   Rc::new((vec![Type::I64, Type::I64], Some(Type::I64)))]);
        let f_to_f = Rc::new(vec![Rc::new((vec![Type::F32], Some(Type::F32))),
                                  Rc::new((vec![Type::F64], Some(Type::F64)))]);
        let ff_to_f = Rc::new(vec![Rc::new((vec![Type::F32, Type::F32], Some(Type::F32))),
                                   Rc::new((vec![Type::F64, Type::F64], Some(Type::F64)))]);

        intrinsics.insert(Id::intern("clz"),    (i_to_i.clone(), Intrin::Unop(Unop::Clz)));
        intrinsics.insert(Id::intern("ctz"),    (i_to_i.clone(), Intrin::Unop(Unop::Ctz)));
        intrinsics.insert(Id::intern("popcnt"), (i_to_i.clone(), Intrin::Unop(Unop::Popcnt)));
        intrinsics.insert(Id::intern("eqz"),    (i_to_i.clone(), Intrin::Unop(Unop::Eqz)));

        intrinsics.insert(Id::intern("sqrt"),    (f_to_f.clone(), Intrin::Unop(Unop::Sqrt)));
        intrinsics.insert(Id::intern("ceil"),    (f_to_f.clone(), Intrin::Unop(Unop::Ceil)));
        intrinsics.insert(Id::intern("floor"),   (f_to_f.clone(), Intrin::Unop(Unop::Floor)));
        intrinsics.insert(Id::intern("nearest"), (f_to_f.clone(), Intrin::Unop(Unop::Nearest)));
        intrinsics.insert(Id::intern("trunc"),   (f_to_f.clone(), Intrin::Unop(Unop::Trunc)));

        intrinsics.insert(Id::intern("i32_to_i64"), (i_to_l.clone(), Intrin::Unop(Unop::I32ToI64)));
        intrinsics.insert(Id::intern("u32_to_i64"), (i_to_l.clone(), Intrin::Unop(Unop::U32ToI64)));
        intrinsics.insert(Id::intern("i64_to_i32"), (l_to_i.clone(), Intrin::Unop(Unop::I64ToI32)));

        intrinsics.insert(Id::intern("rotl"), (ii_to_i.clone(), Intrin::Binop(Binop::RotLeft)));
        intrinsics.insert(Id::intern("rotr"), (ii_to_i.clone(), Intrin::Binop(Binop::RotRight)));

        intrinsics.insert(Id::intern("copysign"), (ff_to_f.clone(), Intrin::Binop(Binop::Copysign)));

        IntrinsicEnv {
            intrinsics,
            binding_payload: PhantomData
        }
    }

    pub fn lookup(&self, name:Id) -> Option<Binding<T>> {
        match self.intrinsics.get(&name) {
            Some((b, op)) => Some(Binding::Intrinsic(b.clone(), *op)),
            None          => None
        }
    }
}

pub struct ToplevelEnv<T : Clone>
{
    env: HashMap<Id, Binding<T>>
}

impl<T> ToplevelEnv<T>
    where T : Clone
{
    pub fn new() -> ToplevelEnv<T> {
        ToplevelEnv { env: HashMap::new() }
    }

    pub fn probe(&self, name:Id) -> bool {
        self.env.contains_key(&name)
    }

    pub fn lookup(&self, name:Id) -> Option<Binding<T>> {
        match self.env.get(&name) {
            Some(b) => Some(b.clone()),
            None    => None
        }
    }

    pub fn remove(&mut self, name:Id) {
        self.env.remove(&name);
    }

    pub fn insert_function(&mut self, name:Id, param_types:Vec<Type>, retn:Option<Type>) {
        self.env.insert(name, Binding::Function(Rc::new((param_types, retn))));
    }

    pub fn is_function(&self, name:Id) -> bool {
        match self.env.get(&name) {
            Some(Binding::Function(_)) => true,
            _ => false
        }
    }

    pub fn insert_global(&mut self, name:Id, mutable:bool, ty:Type) {
        self.env.insert(name, Binding::Global(mutable, ty));
    }

    pub fn is_global(&self, name:Id) -> bool {
        match self.env.get(&name) {
            Some(Binding::Global(_,_)) => true,
            _ => false
        }
    }

    pub fn insert_struct(&mut self, name:Id, fields:Vec<(Id,Type)>) {
        self.env.insert(name, Binding::Struct(Rc::new((name.clone(),fields))));
    }

    pub fn is_struct(&self, name:Id) -> bool {
        match self.env.get(&name) {
            Some(Binding::Struct(_)) => true,
            _ => false
        }
    }
}

pub struct LocalEnv<T : Clone>
{
    // The Binding is a Label or a Local
    locals: Vec<Vec<(Id, Binding<T>)>>
}

impl<T> LocalEnv<T>
    where T : Clone
{
    pub fn new() -> LocalEnv<T> {
        LocalEnv {
            locals: vec![],
        }
    }

    pub fn push_rib(&mut self) {
        self.locals.push(vec![]);
    }

    pub fn pop_rib(&mut self) {
        self.locals.pop().unwrap();
    }

    pub fn add_param(&mut self, param_name: Id, param_attrib: T) {
        let last = self.locals.len()-1;
        self.locals[last].push((param_name, Binding::Local(param_attrib)));
    }

    pub fn add_local(&mut self, local_name: Id, local_attrib: T) {
        let last = self.locals.len()-1;
        self.locals[last].push((local_name, Binding::Local(local_attrib)));
    }

    pub fn add_label(&mut self, label: Id) {
        let last = self.locals.len()-1;
        self.locals[last].push((label, Binding::Label));
    }

    pub fn lookup(&self, id:Id) -> Option<Binding<T>> {
        let mut ribno = self.locals.len() as i32 - 1;
        while ribno >= 0 {
            let rib = &self.locals[ribno as usize];
            ribno -= 1;
            let mut locno = rib.len() as i32 - 1;
            while locno >= 0 {
                let item = &rib[locno as usize];
                locno -= 1;
                if id == item.0 {
                    return Some(item.1.clone());
                }
            }
        }
        None
    }
}

// Transparent bundle of the several environment aspects, along with common
// methods.

pub struct Env<T : Clone>
{
    pub intrinsics: IntrinsicEnv<T>,
    pub toplevel:   ToplevelEnv<T>,
    pub locals:     LocalEnv<T>
}

impl<T> Env<T>
    where T : Clone
{
    pub fn new() -> Env<T> {
        Env {
            intrinsics: IntrinsicEnv::new(),
            toplevel:   ToplevelEnv::new(),
            locals:     LocalEnv::new()
        }
    }

    pub fn lookup(&self, id:Id) -> Option<Binding<T>> {
        if let Some(b) = self.locals.lookup(id) {
            Some(b)
        } else if let Some(b) = self.toplevel.lookup(id) {
            Some(b)
        } else if let Some(b) = self.intrinsics.lookup(id) {
            Some(b)
        } else {
            None
        }
    }

    pub fn predefine_global(&mut self, g:&GlobalDef) {
        assert!(!self.toplevel.probe(g.name));
        self.toplevel.insert_global(g.name, false, Type::I32);
    }

    pub fn elaborate_global(&mut self, g:&GlobalDef) {
        assert!(self.toplevel.is_global(g.name));
        self.toplevel.remove(g.name);
        self.define_global(g);
    }

    pub fn predefine_function(&mut self, f:&FunctionDef) {
        assert!(!self.toplevel.probe(f.name));
        self.toplevel.insert_function(f.name, vec![], None);
    }

    pub fn elaborate_function(&mut self, f:&FunctionDef) {
        assert!(self.toplevel.is_function(f.name));
        self.toplevel.remove(f.name);
        self.define_function(f);
    }

    pub fn predefine_struct(&mut self, s:&StructDef) {
        assert!(!self.toplevel.probe(s.name));
        self.toplevel.insert_struct(s.name, vec![]);
    }

    pub fn elaborate_struct(&mut self, s:&StructDef) {
        assert!(self.toplevel.is_struct(s.name));
        self.toplevel.remove(s.name);
        self.define_struct(s);
    }

    pub fn define_global(&mut self, g:&GlobalDef) {
        assert!(!self.toplevel.probe(g.name));
        self.toplevel.insert_global(g.name, g.mutable, g.ty);
    }

    pub fn define_function(&mut self, f:&FunctionDef) {
        assert!(!self.toplevel.probe(f.name));
        let param_types = (&f.formals).into_iter().map(|(_,ty)| *ty).collect();
        self.toplevel.insert_function(f.name, param_types, f.retn);
    }

    pub fn define_struct(&mut self, g:&StructDef) {
        assert!(!self.toplevel.probe(g.name));
        self.toplevel.insert_struct(g.name, g.fields.clone());
    }
    
    pub fn define_toplevel(&mut self, item:&ModItem) {
        match item {
            ModItem::Var(v)    => { self.define_global(v) }
            ModItem::Fn(f)     => { self.define_function(f); }
            ModItem::Struct(s) => { self.define_struct(s); }
        }
    }

    pub fn get_struct_def(&self, name:Id) -> Rc<Struct> {
        match self.toplevel.lookup(name) {
            Some(Binding::Struct(s)) => s,
            _ => unreachable!()
        }
    }
}
