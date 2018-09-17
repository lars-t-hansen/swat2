// The type checker validates the input program and rewrites it
// slightly in place.
//
// It checks that:
// - no global names are multiply defined
// - no parameter names are multiply defined
// - globals are initialized by constant expressions
// - expressions have appropriate types for their context, and types
//   are commensurable where they meet
// - every identifier references an appropriate bound name
// - every operator is applied to appropriate types
// - every call references a function or intrinsic
// - calls pass the right number of arguments and receive the right
//   number of results (0 or 1, for now)
//
// It computes types for all expr and block nodes and records those in
// the nodes.
//
// In the future, it may also insert explicit casts where they are
// implicit, rewriting the expression tree as required.

use ast::*;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

struct Check {
    intrinsics: HashMap<String, Rc<Vec<Rc<Signature>>>>,
    toplevel:   HashMap<String, ToplevelItem>,
    locals:     Vec<Vec<LocalItem>>,
    gensym:     i32
}

type Signature = (Vec<Type>, Option<Type>);

enum ToplevelItem {
    Global(bool, Type),
    Function(Rc<Signature>)
}

enum Binding {
    GlobalVar(bool, Type),
    GlobalFun(Rc<Signature>),
    Intrinsic(Rc<Vec<Rc<Signature>>>),
    Local(Id, Type)
}

pub fn check(p:&mut Program) {
    let mut chk = Check::new();
    chk.check_program(p);
}

impl Check
{
    fn new() -> Check {
        let mut intrinsics = HashMap::new();

        let ints = Rc::new(vec![Rc::new((vec![Type::I32], Some(Type::I32))),
                                Rc::new((vec![Type::I64], Some(Type::I64)))]);
        let floats = Rc::new(vec![Rc::new((vec![Type::F32], Some(Type::F32))),
                                  Rc::new((vec![Type::F64], Some(Type::F64)))]);
        intrinsics.insert("clz".to_string(), ints.clone());
        intrinsics.insert("ctz".to_string(), ints.clone());
        intrinsics.insert("popcnt".to_string(), ints.clone());
        intrinsics.insert("eqz".to_string(), ints.clone());
        intrinsics.insert("sqrt".to_string(), floats.clone());
        intrinsics.insert("ceil".to_string(), floats.clone());
        intrinsics.insert("floor".to_string(), floats.clone());
        intrinsics.insert("nearest".to_string(), floats.clone());
        intrinsics.insert("trunc".to_string(), floats.clone());

        Check {
            intrinsics,
            toplevel:   HashMap::new(),
            locals:     vec![],
            gensym:     0
        }
    }

    // Toplevel and intrinsic environment.

    fn toplevel_define(&mut self, name: &Id, t:ToplevelItem) {
        self.toplevel.insert(name.name.clone(), t);
    }
    
    fn is_toplevel_defined(&mut self, name: &Id) -> bool {
        self.toplevel.contains_key(&name.name)
    }

    fn find_global_binding(&self, name: &Id) -> Option<Binding> {
        match self.toplevel.get(&name.name) {
            Some(ToplevelItem::Global(mutable, t)) => Some(Binding::GlobalVar(*mutable, *t)),
            Some(ToplevelItem::Function(sig)) => Some(Binding::GlobalFun(sig.clone())),
            None => None
        }
    }

    fn find_intrinsic_binding(&self, name: &Id) -> Option<Binding> {
        match self.intrinsics.get(&name.name) {
            Some(b) => Some(Binding::Intrinsic(b.clone())),
            None => None
        }
    }

    // Local environment.

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
        let aka = format!("{}_{}", k, local_name);
        let last = self.locals.len()-1;
        let aka = Id { name: aka.clone() };
        self.locals[last].push(LocalItem { name: local_name.clone(), aka: aka.clone(), ty: local_type });
        aka
    }

    fn find_local_binding(&self, id:&Id) -> Option<Binding> {
        let mut ribno = self.locals.len() as i32 - 1;
        while ribno >= 0 {
            let rib = &self.locals[ribno as usize];
            ribno -= 1;
            let mut locno = rib.len() as i32 - 1;
            while locno >= 0 {
                let item = &rib[locno as usize];
                locno -= 1;
                if id == &item.name {
                    return Some(Binding::Local(item.aka.clone(), item.ty));
                }
            }
        }
        None
    }

    // Environment accessors, generally

    fn find_binding(&mut self, id:&Id) -> Option<Binding> {
        if let Some(b) = self.find_local_binding(id) {
            Some(b)
        } else if let Some(b) = self.find_global_binding(id) {
            Some(b)
        } else if let Some(b) = self.find_intrinsic_binding(id) {
            Some(b)
        } else {
            None
        }
    }
    
    // Type checker

    fn check_program(&mut self, p:&mut Program) {
        for item in &mut p.items {
            self.check_top_item(item);
        }
    }

    fn check_top_item(&mut self, item:&mut TopItem) {
        match item {
            TopItem::Mod(m) => { self.check_module(m) }
            TopItem::Js(_) => { }
        }
    }

    fn check_module(&mut self, m:&mut Module) {
        for item in &mut m.items {
            self.check_mod_item(item);
        }
    }

    fn check_mod_item(&mut self, item:&mut ModItem) {
        match item {
            ModItem::Var(v) => { self.check_global(v); }
            ModItem::Fn(f)  => { self.check_function(f); }
        }
    }

    fn check_global(&mut self, g:&mut GlobalVar) {
        if self.is_toplevel_defined(&g.name) {
            panic!("Multiply defined top-level name {}", g.name);
        }
        self.toplevel_define(&g.name, ToplevelItem::Global(g.mutable, g.ty));
        if !g.imported {
            self.check_const_expr(&mut g.init);
            if !same_type(Some(g.ty), g.init.ty) {
                panic!("Init expression type mismatch");
            }
        }
    }

    fn check_function(&mut self, f:&mut FnDef) {
        if self.is_toplevel_defined(&f.name) {
            panic!("Multiply defined top-level name {}", f.name);
        }

        let mut param_types = vec![];
        for (_,ty) in &f.formals {
            param_types.push(*ty);
        }

        self.toplevel_define(&f.name, ToplevelItem::Function(Rc::new((param_types, f.retn))));

        self.push_rib();

        let mut param_names = HashSet::<String>::new();
        for (param_name, param_type) in &f.formals {
            if param_names.contains(&param_name.name) {
                panic!("Duplicate parameter name {}", param_name);
            }
            param_names.insert(param_name.name.clone());
            self.add_param(param_name, *param_type);
        }

        if !f.imported {
            self.check_block(&mut f.body);
            if !same_type(f.retn, f.body.ty) {
                panic!("Return type / body type mismatch");
            }
        }

        self.pop_rib();
    }

    fn check_block(&mut self, b:&mut Block) {
        let mut last_type = None;
        for item in &mut b.items {
            match item {
                BlockItem::Let(l) => {
                    self.check_expr(&mut l.init);
                    if !same_type(l.init.ty, Some(l.ty)) {
                        panic!("Initializer does not have same type as variable {}", &l.name);
                    }
                    self.add_local(&l.name, l.ty);
                    last_type = l.init.ty;
                }
                BlockItem::Expr(e) => {
                    self.check_expr(e);
                    last_type = e.ty;
                }
            }
        }
        b.ty = last_type;
    }

    fn check_const_expr(&mut self, e:&mut Expr) {
        match e.u {
            Uxpr::NumLit(_n) => { /* Carries correct type */ }
            Uxpr::Void       => { /* Carries correct type */ }
            _                => { panic!("Not a constant expression"); }
        }
    }

    fn check_expr(&mut self, expr:&mut Expr) {
        match &mut expr.u {
            Uxpr::Void => {
                // Carries correct type (void)
            }
            Uxpr::NumLit(_) => {
                // Carries correct type (of the number)
            }
            Uxpr::If{test, consequent, alternate} => {
                self.check_expr(test);
                self.check_block(consequent);
                self.check_block(alternate);
                if !same_type(test.ty, Some(Type::I32)) {
                    panic!("Test type must be i32");
                }
                if !same_type(consequent.ty, alternate.ty) {
                    panic!("Arms of 'if' must have same type");
                }
                expr.ty = consequent.ty;
            }
            Uxpr::While{test, body} => {
                self.check_expr(test);
                self.check_block(body);
                if !i32_type(test.ty) {
                    panic!("Test type must be i32");
                }
                // Carries correct type (void)
            }
            Uxpr::Binop{op, lhs, rhs} => {
                self.check_expr(lhs);
                self.check_expr(rhs);
                if !same_type(lhs.ty, rhs.ty) {
                    panic!("Binop requires equal types");
                }
                match op {
                    Binop::UDiv | Binop::URem |
                    Binop::ShiftLeft | Binop::ShiftRight | Binop::UShiftRight |
                    Binop::BitAnd | Binop::BitOr | Binop::BitXor |
                    Binop::ULess | Binop::ULessOrEqual | Binop::UGreater | Binop::UGreaterOrEqual |
                    Binop::RotLeft | Binop::RotRight =>
                    {
                        if !int_type(lhs.ty) {
                            panic!("Integer type required");
                        }
                    }
                    Binop::Copysign =>
                    {
                        if !float_type(lhs.ty) {
                            panic!("Floating type required");
                        }
                    }
                    Binop::Add | Binop::Sub | Binop::Mul | Binop::Div | Binop::Rem |
                    Binop::Less | Binop::LessOrEqual | Binop::Greater | Binop::GreaterOrEqual =>
                    {
                        if !num_type(lhs.ty) {
                            panic!("Numeric type required");
                        }
                    }
                    Binop::Equal | Binop::NotEqual =>
                    {
                        if !value_type(lhs.ty) {
                            panic!("Non-void type required");
                        }
                    }
                }
                expr.ty = lhs.ty;
            }
            Uxpr::Unop{op, e} => {
                self.check_expr(e);
                match op {
                    Unop::Neg =>
                    {
                        if !num_type(e.ty) {
                            panic!("Numeric type required for negation");
                        }
                    }
                    Unop::Not =>
                    {
                        if !i32_type(e.ty) {
                            panic!("i32 type required for boolean 'not'");
                        }
                    }
                    Unop::BitNot =>
                    {
                        if !int_type(e.ty) {
                            panic!("integer type required for bitwise 'not'");
                        }
                    }
                    Unop::Clz | Unop::Ctz | Unop::Popcnt | Unop::Eqz |
                    Unop::Extend8 | Unop::Extend16 | Unop::Extend32 |
                    Unop::Sqrt | Unop::Ceil | Unop::Floor | Unop::Nearest | Unop::Trunc =>
                    {
                        panic!("Unary operator should not be present at this stage");
                    }
                }
                expr.ty = e.ty;
            }
            Uxpr::Call{name, actuals} => {
                for actual in &mut *actuals {
                    self.check_expr(actual);
                }
                if let Some(b) = self.find_binding(&name) {
                    match b {
                        Binding::GlobalFun(sig) => {
                            let (formals, ret) = &*sig;
                            if actuals.len() != formals.len() {
                                panic!("Mismatch in number of arguments");
                            }
                            for i in 0..actuals.len() {
                                if !same_type(Some(formals[i]), actuals[i].ty) {
                                    panic!("Mismatch in argument type");
                                }
                            }
                            expr.ty = *ret;
                        }
                        Binding::Intrinsic(sigs) => {
                            let mut found = false;
                          'sigloop:
                            for sig in &*sigs {
                                let (formals, ret) = &**sig;
                                if actuals.len() != formals.len() {
                                    continue 'sigloop;
                                }
                                for i in 0..actuals.len() {
                                    if !same_type(Some(formals[i]), actuals[i].ty) {
                                        continue 'sigloop;
                                    }
                                }
                                found = true;
                                expr.ty = *ret;
                                break 'sigloop;
                            }
                            if !found {
                                panic!("No signature match for intrinsic {}", &name);
                            }
                        }
                        _ => {
                            panic!("Call to non-function: {}", &name);
                        }
                    }
                } else {
                    panic!("Call to unbound name: {}", &name);
                }
            }
            Uxpr::Id(id) => {
                match self.find_binding(&id) {
                    Some(Binding::Local(_aka, t)) => {
                        expr.ty = Some(t);
                    }
                    Some(Binding::GlobalVar(_mutable, t)) => {
                        expr.ty = Some(t);
                    }
                    Some(Binding::GlobalFun(_)) | Some(Binding::Intrinsic(_)) => {
                        panic!("No first-class functions");
                    }
                    None => {
                        panic!("Reference to unknown variable {}", id)
                    }
                }
            }
            Uxpr::Assign{lhs, rhs} => {
                self.check_expr(rhs);
                match lhs {
                    LValue::Id(id) => {
                        let t = match self.find_binding(&id) {
                            Some(Binding::Local(_, t)) => {
                                t
                            }
                            Some(Binding::GlobalVar(mutable, t)) => {
                                if !mutable {
                                    panic!("Can't assign to constant");
                                }
                                t
                            }
                            _ => {
                                panic!("Not a reference to a variable: {}", &id);
                            }
                        };
                        if !same_type(Some(t), rhs.ty) {
                            panic!("Type of value being stored does not match variable");
                        }
                    }
                    LValue::Local(_) | LValue::Global(_) => { panic!("Should not happen"); }
                }
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

fn i32_type(t1:Option<Type>) -> bool {
    same_type(t1, Some(Type::I32))
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
