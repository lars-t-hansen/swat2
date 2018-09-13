// -*- fill-column: 80 -*-

// Transforms a Program into a new Program with the following invariants:
//
// - no BlockItem::Let nodes, they have been rewritten as locals + assignment
// - no Expr::Id nodes, they have been rewritten either as Expr::Local or
//   Expr::Global nodes
// - calls to intrinsics have been rewritten as unop/binop/convop nodes
// - type checking has been done in functions and on global initializers
// - every expr and block node has a type
// - the locals array of a function is populated

use ast::*;
use std::collections::{HashMap, HashSet};
use std::iter::Iterator;
use std::rc::Rc;

pub struct Xform {
    toplevel: HashMap<String, ToplevelItem>,
    locals: Vec<Vec<LocalItem>>,
    intrinsics1: HashMap<String, Unop>,
    intrinsics2: HashMap<String, Binop>,
    gensym: i32
}

type Signature = (Vec<Type>, Option<Type>);

enum ToplevelItem {
    Global(Type),
    Function(Rc<Signature>)
}

enum CallTarget {
    Error,
    Intrinsic1(Unop),
    Intrinsic2(Binop),
    Function(Rc<Signature>)
}

enum Binding {
    GlobalVar(Type),
    GlobalFun(Rc<Signature>),
    Local(Id, Type)
}

impl Xform {
    pub fn new() -> Xform {
        let mut intrinsics1 = HashMap::new();
        intrinsics1.insert("ceil".to_string(), Unop::Ceil);
        intrinsics1.insert("clz".to_string(), Unop::Clz);
        intrinsics1.insert("ctz".to_string(), Unop::Ctz);
        intrinsics1.insert("extend8".to_string(), Unop::Extend8);
        intrinsics1.insert("extend16".to_string(), Unop::Extend16);
        intrinsics1.insert("extend32".to_string(), Unop::Extend32);
        intrinsics1.insert("floor".to_string(), Unop::Floor);
        intrinsics1.insert("nearest".to_string(), Unop::Nearest);
        intrinsics1.insert("popcnt".to_string(), Unop::Popcnt);
        intrinsics1.insert("sqrt".to_string(), Unop::Sqrt);
        intrinsics1.insert("trunc".to_string(), Unop::Trunc);

        let mut intrinsics2 = HashMap::new();
        intrinsics2.insert("copysign".to_string(), Binop::Copysign);
        intrinsics2.insert("rotl".to_string(), Binop::RotLeft);
        intrinsics2.insert("rotr".to_string(), Binop::RotRight);

        Xform {
            toplevel: HashMap::new(),
            locals:   vec![],
            intrinsics1,
            intrinsics2,
            gensym:   0
        }
    }

    // Toplevel environment.

    fn toplevel_define(&mut self, name: &Id, t: ToplevelItem) {
        self.toplevel.insert(name.name.clone(), t);
    }

    fn is_toplevel_defined(&self, name: &Id) -> bool {
        self.toplevel.contains_key(&name.name)
    }

    fn find_global_binding(&self, name: &Id) -> Option<Binding> {
        match self.toplevel.get(&name.name) {
            Some(ToplevelItem::Global(t)) => Some(Binding::GlobalVar(*t)),
            Some(ToplevelItem::Function(sig)) => Some(Binding::GlobalFun(sig.clone())),
            None => None
        }
    }
    
    // Local environment.
    // TODO: reuse locals

    fn push_rib(&mut self) {
        self.locals.push(vec![]);
    }

    fn pop_rib(&mut self) -> Vec<LocalItem> {
        self.locals.pop().unwrap()
    }

    fn add_param(&mut self, param_name: &Id, param_type: Type) {
        let last = self.locals.len()-1;
        self.locals[last].push(LocalItem { name: param_name.clone(), aka: param_name.clone(), ty: param_type });
    }

    fn add_local(&mut self, local_name: &Id, local_type: Type) -> Id {
        let k = self.gensym;
        self.gensym += 1;
        let aka = format!("{}_{}", k, local_name.name);
        let last = self.locals.len()-1;
        let aka = Id { name: aka.clone() };
        self.locals[last].push(LocalItem { name: local_name.clone(), aka: aka.clone(), ty: local_type });
        aka
    }

    fn is_locally_defined(&self, id:&Id) -> bool {
        for rib in &self.locals {
            for item in rib {
                if id == &item.name {
                    return true;
                }
            }
        }
        false
    }

    // BUG: must search from the end of each rib to handle shadowing properly.

    fn find_local_binding(&self, id:&Id) -> Option<Binding> {
        for rib in &self.locals {
            for item in rib {
                if id == &item.name {
                    return Some(Binding::Local(item.aka.clone(), item.ty));
                }
            }
        }
        None
    }

    // Environment accessors, generally

    fn find_call_target(&mut self, id:&Id) -> CallTarget {
        if self.is_locally_defined(id) {
            CallTarget::Error
        } else if let Some(b) = self.find_global_binding(id) {
            match b {
                Binding::GlobalFun(sig) => CallTarget::Function(sig.clone()),
                _                       => CallTarget::Error
            }
        } else if let Some(v) = self.intrinsics1.get(&id.name) {
            CallTarget::Intrinsic1(*v)
        } else if let Some(v) = self.intrinsics2.get(&id.name) {
            CallTarget::Intrinsic2(*v)
        } else {
            CallTarget::Error
        }
    }

    fn find_binding(&mut self, id:&Id) -> Option<Binding> {
        if let Some(b) = self.find_local_binding(id) {
            Some(b)
        } else if let Some(b) = self.find_global_binding(id) {
            Some(b)
        } else {
            None
        }
    }
    
    // Transformers consume the Program and return a transformed one.

    pub fn xform_program(&mut self, p: Program) -> Program {
        let new_items = p.items.into_iter().map(|item| self.xform_top_item(item)).collect();
        Program { items: new_items }
    }
    
    fn xform_top_item(&mut self, i: TopItem) -> TopItem {
        match i {
            TopItem::Mod(m) => TopItem::Mod(self.xform_module(m)),
            TopItem::Js(s) => TopItem::Js(s)
        }
    }

    // TODO: Must process the module top level once first, to define top-level
    // phrases, so that we can forward-reference.

    fn xform_module(&mut self, m: Module) -> Module {
        let new_items = m.items.into_iter().map(|item| self.xform_mod_item(item)).collect();
        Module { name: m.name, items: new_items }
    }

    fn xform_mod_item(&mut self, i: ModItem) -> ModItem {
        match i {
            ModItem::Var(v) => ModItem::Var(Box::new(self.xform_global(*v))),
            ModItem::Fn(f) => ModItem::Fn(Box::new(self.xform_function(*f)))
        }
    }

    fn xform_global(&mut self, g: GlobalVar) -> GlobalVar {
        if self.is_toplevel_defined(&g.name) {
            panic!("Multiply defined top-level name {}", g.name.name);
        }
        self.toplevel_define(&g.name, ToplevelItem::Global(g.ty));
        let new_init =
            if !g.imported {
                let new_init = self.xform_const_expr(*g.init);
                if !same_type(Some(g.ty), new_init.ty) {
                    panic!("Init expression type mismatch");
                }
                new_init
            } else {
                *g.init
            };
        GlobalVar { mutable: g.mutable, imported: g.imported, exported: g.exported,
                    name: g.name, ty: g.ty, init: Box::new(new_init) }
    }

    fn xform_function(&mut self, f: FnDef) -> FnDef {
        if self.is_toplevel_defined(&f.name) {
            panic!("Multiply defined top-level name {}", f.name.name);
        }

        // TODO: obviously a map of some kind but can't make the types work out
        let mut param_types = vec![];
        for (_,ty) in &f.formals {
            param_types.push(*ty);
        }

        self.toplevel_define(&f.name, ToplevelItem::Function(Rc::new((param_types, f.retn))));

        self.push_rib();

        let mut param_names = HashSet::<String>::new();
        for (param_name, param_type) in &f.formals {
            if param_names.contains(&param_name.name) {
                panic!("Duplicate parameter name {}", param_name.name);
            }
            param_names.insert(param_name.name.clone());
            self.add_param(param_name, *param_type);
        }

        let new_body = 
            if !f.imported {
                let new_body = self.xform_block(*f.body);
                if !same_type(f.retn, new_body.ty) {
                    panic!("Return type / body type mismatch");
                }
                new_body
            } else {
                *f.body
            };

        let locals = self.pop_rib();

        FnDef { imported: f.imported, exported: f.exported, name: f.name,
                formals: f.formals, retn: f.retn, locals, body: Box::new(new_body) }
    }

    fn xform_const_expr(&mut self, e: Expr) -> Expr {
        match e.u {
            Uxpr::NumLit(n) => {
                match n {
                    Number::I32(_) => Expr{ty: Some(Type::I32), u:e.u},
                    Number::I64(_) => Expr{ty: Some(Type::I64), u:e.u},
                    Number::F32(_) => Expr{ty: Some(Type::F32), u:e.u},
                    Number::F64(_) => Expr{ty: Some(Type::F64), u:e.u}
                }
            }
            Uxpr::Void => {
                Expr{ty:None, u:Uxpr::Void}
            }
            _ => {
                panic!("Not a constant expression")
            }
        }
    }

    fn xform_block(&mut self, b: Block) -> Block {
        let mut new_items : Vec<BlockItem> = vec![];
        let mut last_type = None;
        for item in b.items {
            match item {
                BlockItem::Let(l) => {
                    let aka = self.add_local(&l.name, l.ty);
                    // TODO: This rewrite won't work unless Id(aka) is also in the
                    // environment, and currently it's not...  Can't we just use l.name?  Shadowing should work
                    let new_expr = self.xform_expr(Expr{ty:None, u:Uxpr::Assign{lhs:LValue::Id(aka), rhs:l.init}});
                    last_type = new_expr.ty;
                    new_items.push(BlockItem::Expr(Box::new(new_expr)));
                }
                BlockItem::Expr(e) => {
                    let new_expr = self.xform_expr(*e);
                    last_type = new_expr.ty;
                    new_items.push(BlockItem::Expr(Box::new(new_expr)));
                }
            }
        }
        Block { ty: last_type, items: new_items }
    }

    fn xform_expr(&mut self, b: Expr) -> Expr {
        match b.u {
            Uxpr::Void => b,
            Uxpr::NumLit(_) => b,
            Uxpr::If{test, consequent, alternate} => {
                let test = self.xform_expr(*test);
                let consequent = self.xform_block(*consequent);
                let alternate = self.xform_block(*alternate);
                if !same_type(test.ty, Some(Type::I32)) {
                    panic!("Test type must be i32");
                }
                if !same_type(consequent.ty, alternate.ty) {
                    panic!("Arms of 'if' must have same type");
                }
                Expr{ty: consequent.ty, u:Uxpr::If{test:Box::new(test),
                                                   consequent:Box::new(consequent),
                                                   alternate:Box::new(alternate)}}
            }
            Uxpr::While{test, body} => {
                let test = self.xform_expr(*test);
                let body = self.xform_block(*body);
                if !same_type(test.ty, Some(Type::I32)) {
                    panic!("Test type must be i32");
                }
                Expr{ty: None, u:Uxpr::While{test:Box::new(test),
                                              body:Box::new(body)}}
            }
            Uxpr::Binop{op, lhs, rhs} => {
                let lhs = self.xform_expr(*lhs);
                let rhs = self.xform_expr(*rhs);
                if !same_type(lhs.ty, rhs.ty) {
                    panic!("Binop requires equal types");
                }
                match op {
                    Binop::UDiv | Binop::URem |
                    Binop::ShiftLeft | Binop::ShiftRight | Binop::UShiftRight |
                    Binop::BitAnd | Binop::BitOr | Binop::BitXor |
                    Binop::ULess | Binop::ULessOrEqual | Binop::UGreater | Binop::UGreaterOrEqual |
                    Binop::RotLeft | Binop::RotRight => {
                        if !int_type(lhs.ty) {
                            panic!("Integer type required");
                        }
                    }
                    Binop::Copysign => {
                        if !float_type(lhs.ty) {
                            panic!("Floating type required");
                        }
                    }
                    Binop::Add | Binop::Sub | Binop::Mul | Binop::Div | Binop::Rem |
                    Binop::Less | Binop::LessOrEqual | Binop::Greater | Binop::GreaterOrEqual => {
                        if !num_type(lhs.ty) {
                            panic!("Numeric type required");
                        }
                    }
                    Binop::Equal | Binop::NotEqual => {
                        if !value_type(lhs.ty) {
                            panic!("Non-void type required");
                        }
                    }
                }
                Expr{ty: lhs.ty, u:Uxpr::Binop{op,
                                                lhs:Box::new(lhs),
                                                rhs:Box::new(rhs)}}
            }
            Uxpr::Unop{op, e} => {
                let e = self.xform_expr(*e);
                // TODO: check that operator applies to type in question
                Expr{ty: e.ty, u:Uxpr::Unop{op, e:Box::new(e)}}
            }
            Uxpr::Call{name, actuals} => {
                match self.find_call_target(&name) {
                    CallTarget::Function(sig) => {
                        let (formals, ret) = &*sig;
                        if actuals.len() != formals.len() {
                            panic!("Mismatch in number of arguments");
                        }
                        let mut new_actuals = vec![];
                        for e in actuals {
                            new_actuals.push(Box::new(self.xform_expr(*e)));
                        }
                        for i in 0..new_actuals.len() {
                            if !same_type(Some(formals[i]), new_actuals[i].ty) {
                                panic!("Mismatch in argument type");
                            }
                        }
                        Expr{ty: *ret, u:Uxpr::Call{name, actuals:new_actuals}}
                    }
                    CallTarget::Intrinsic1(v) => {
                        // TODO: implement
                        panic!("Not implemented");
                    }
                    CallTarget::Intrinsic2(v) => {
                        // TODO: implement
                        panic!("Not implemented");
                    }
                    CallTarget::Error => {
                        panic!("Call does not reference function or intrinsic: {}", name.name);
                    }
                }
            }
            Uxpr::Id(id) => {
                match self.find_binding(&id) {
                    Some(Binding::Local(aka, t)) => {
                        Expr{ty: Some(t), u:Uxpr::Local(aka)}
                    }
                    Some(Binding::GlobalVar(t)) => {
                        Expr{ty: Some(t), u:Uxpr::Global(id)}
                    }
                    Some(Binding::GlobalFun(_)) => {
                        panic!("No first-class functions");
                    }
                    None => {
                        panic!("Reference to unknown variable {}", id.name)
                    }
                }
            }
            Uxpr::Assign{lhs, rhs} => {
                // TODO
                panic!("'assign' NYI")
            }
            Uxpr::Local(_) => {
                panic!("'local' node should not appear here");
            }
            Uxpr::Global(_) => {
                panic!("'global' node should not appear here");
            }
        }
    }
}

// Types - maybe belong elsewhere?

fn same_type(t1:Option<Type>, t2:Option<Type>) -> bool {
    match (t1, t2) {
        (None, None) => true,
        (None, _)    => false,
        (_, None)    => false,
        (Some(t1), Some(t2)) => t1 == t2
    }
}

fn int_type(t1:Option<Type>) -> bool {
    match t1 {
        Some(Type::I32) | Some(Type::I64) => true,
        _ => false
    }
}

fn float_type(t1:Option<Type>) -> bool {
    match t1 {
        Some(Type::F32) | Some(Type::F64) => true,
        _ => false
    }
}

fn num_type(t1:Option<Type>) -> bool {
    match t1 {
        Some(Type::I32) | Some(Type::I64) => true,
        Some(Type::F32) | Some(Type::F64) => true,
        _ => false
    }
}

fn value_type(t1:Option<Type>) -> bool {
    match t1 {
        Some(Type::I32) | Some(Type::I64) => true,
        Some(Type::F32) | Some(Type::F64) => true,
        Some(Type::AnyRef) => true,
        _ => false
    }
}
