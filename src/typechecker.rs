// -*- fill-column: 80 -*-
//
// The type checker validates the input program and annotates the tree but
// currently does not rewrite it at all.
//
// It checks that:
// - no global names are multiply defined
// - no parameter names are multiply defined
// - globals are initialized by constant expressions
// - expressions have appropriate types for their context, and types are
//   commensurable where they meet
// - every identifier references an appropriate bound name
// - every operator is applied to appropriate types
// - every call references a function or intrinsic
// - calls pass the right number of arguments and receive the right number of
//   results (0 or 1, for now)
//
// It computes types for all expr and block nodes and records those in the
// nodes.  It transforms every RawRef type into a CookedRef type.
//
// In the future, it may also insert explicit casts where they are implicit,
// rewriting the expression tree as required.

use ast::*;
use environment::*;
use std::collections::HashSet;

pub fn check(p:&mut Module) {
    let mut chk = Check::new();
    chk.check_module(p);
}

struct Check
{
    env: Env<Type>
}

impl Check
{
    fn new() -> Check {
        Check {
            env: Env::new()
        }
    }

    fn check_module(&mut self, m:&mut Module) {
        // We must bind struct names first so that every subsequent cooking
        // operation knows whether it's a struct type.  But the structs
        // don't actually need to be bound in a the final env. 

        // We can do imperative stuff (refcell), or we can have a replace step,
        // which also works ok - the data structures are small.
        
        // This will all change actually.  We need to rewrite all types in
        // all toplevel items as we cook them, so having a single "define" thing
        // that's once and for all, is no more.
        //
        // So now we need to rewrite things in data structures we don't own...

        // For structs, pass 1:
        //   - defines the name
        //   - checks that fields do not have duplicate names
        // Then for pass 2
        //   - define the names
        //   - check that function prototypes and global vars
        //     reference meaningful types, and cook those types
        //   - check that struct field names reference meaningful
        //     types, and cook the fields types
        // Then we do pass 3, for globals and functions, to compute
        // expression types
/*
        for item in &mut m.items {
            if let ModItem::Struct(s) = item {
                self.bind_struct(s);
                self.check_unique_names(&s.fields, "field");
            }
        }
*/
        for item in &mut m.items {
            match item {
                ModItem::Var(v)    => { self.bind_global(v); }
                ModItem::Fn(f)     => { self.bind_function(f); }
                ModItem::Struct(s) => { panic!("NYI") }
            }
        }
        for item in &mut m.items {
            match item {
                ModItem::Var(v)    => { self.check_global(v); }
                ModItem::Fn(f)     => { self.check_function(f); }
                ModItem::Struct(s) => { }
            }
        }
    }

    fn check_type(&mut self, ty:&mut Type) {
        let mut replacement_type = None;
        match ty {
            Type::RawRef(name) => {
                match self.env.lookup(&name) {
                    Some(Binding::Struct(_)) => {
                        replacement_type = Some(Type::CookedRef(*name));
                    }
                    _ => {
                        panic!("Type reference does not name a type {}", name)
                    }
                }
            },
            Type::CookedRef(_) => {
                unreachable!();
            }
            _ => { }
        }
        if let Some(r) = replacement_type {
            *ty = r;
        }
    }

    fn check_type_or_void(&mut self, ty:&mut Option<Type>) {
        match ty {
            None => { }
            Some(ty) => { self.check_type(ty); }
        }
    }

    fn check_unique_names<T>(xs:&Vec<T>, val:fn(&T)->Id, context:&str) {
        let mut names = HashSet::<Id>::new();
        for v in xs {
            let name = val(&v);
            if names.contains(&name) {
                panic!("Duplicate {} name {}", context, name);
            }
            names.insert(name);
        }
    }

    fn bind_global(&mut self, g:&mut GlobalVar) {
        if self.env.toplevel.probe(&g.name) {
            panic!("Multiply defined top-level name {}", g.name);
        }
        self.env.define_global(g);
    }

    fn check_global(&mut self, g:&mut GlobalVar) {
        if !g.imported {
            self.check_type(&mut g.ty);

            if (g.exported || g.imported) && is_ref_type(Some(g.ty)) {
                panic!("Non-private global can't be of ref type (yet)");
            }

            self.check_const_expr(&mut g.init);
            if !is_same_type(Some(g.ty), g.init.ty) {
                panic!("Init expression type mismatch");
            }
        }
    }

    fn bind_function(&mut self, f:&mut FnDef) {
        if self.env.toplevel.probe(&f.name) {
            panic!("Multiply defined top-level name {}", f.name);
        }
        self.env.define_function(f);
    }
    
    fn check_function(&mut self, f:&mut FnDef) {
        Check::check_unique_names(&f.formals, |(name,_)| *name, "parameter");

        (&mut f.formals).into_iter().for_each(|(_,ty)| self.check_type(ty));
        self.check_type_or_void(&mut f.retn);

        if (f.exported || f.imported) &&
            (is_ref_type(f.retn) || (&f.formals).into_iter().any(|(_,ty)| is_ref_type(Some(*ty))))
        {
            panic!("Non-private function can't use ref type in signature (yet)");
        }

        self.env.locals.push_rib();
        (&f.formals).into_iter().for_each(|(name,ty)| self.env.locals.add_param(name, *ty));

        if !f.imported {
            self.check_block(&mut f.body);
            if !is_same_type(f.retn, f.body.ty) {
                panic!("Return type / body type mismatch");
            }
        }

        self.env.locals.pop_rib();
    }

    fn check_block(&mut self, b:&mut Block) {
        self.env.locals.push_rib();

        let mut last_type = None;
        for item in &mut b.items {
            match item {
                BlockItem::Let(l) => {
                    self.check_type(&mut l.ty);
                    self.check_expr(&mut l.init);
                    if !is_same_type(l.init.ty, Some(l.ty)) {
                        panic!("Initializer does not have same type as variable {}", &l.name);
                    }
                    self.env.locals.add_local(&l.name, l.ty);
                    last_type = l.init.ty;
                }
                BlockItem::Expr(e) => {
                    self.check_expr(e);
                    last_type = e.ty;
                }
            }
        }
        b.ty = last_type;

        self.env.locals.pop_rib();
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
                assert!(expr.ty.is_none());
            }
            Uxpr::NumLit(_) => {
                assert!(is_num_type(expr.ty));
            }
            Uxpr::NullLit => {
                assert!(is_value_type(expr.ty));
            }
            Uxpr::If{test, consequent, alternate} => {
                self.check_expr(test);
                self.check_block(consequent);
                self.check_block(alternate);
                if !is_same_type(test.ty, Some(Type::I32)) {
                    panic!("Test type must be i32");
                }
                if !is_same_type(consequent.ty, alternate.ty) {
                    panic!("Arms of 'if' must have same type");
                }
                expr.ty = consequent.ty;
            }
            Uxpr::While{test, body} => {
                self.check_expr(test);
                self.check_block(body);
                if !is_i32_type(test.ty) {
                    panic!("Test type must be i32");
                }
                assert!(expr.ty.is_none());
            }
            Uxpr::Loop{break_label, body} => {
                self.env.locals.push_rib();
                self.env.locals.add_label(&break_label);
                self.check_block(body);
                self.env.locals.pop_rib();
                assert!(expr.ty.is_none());
            }
            Uxpr::Break{label} => {
                match self.env.lookup(&label) {
                    Some(Binding::Label) => {}
                    _ => { panic!("Not a reference to a label in scope: {}", &label); }
                }
                assert!(expr.ty.is_none());
            }
            Uxpr::Binop{op, lhs, rhs} => {
                self.check_expr(lhs);
                self.check_expr(rhs);
                if !is_same_type(lhs.ty, rhs.ty) {
                    panic!("Binop requires equal types");
                }
                match op {
                    Binop::UDiv | Binop::URem |
                    Binop::ShiftLeft | Binop::ShiftRight | Binop::UShiftRight |
                    Binop::BitAnd | Binop::BitOr | Binop::BitXor |
                    Binop::ULess | Binop::ULessOrEqual | Binop::UGreater | Binop::UGreaterOrEqual |
                    Binop::RotLeft | Binop::RotRight =>
                    {
                        if !is_int_type(lhs.ty) {
                            panic!("Integer type required");
                        }
                    }
                    Binop::Copysign =>
                    {
                        if !is_float_type(lhs.ty) {
                            panic!("Floating type required");
                        }
                    }
                    Binop::Add | Binop::Sub | Binop::Mul | Binop::Div | Binop::Rem |
                    Binop::Less | Binop::LessOrEqual | Binop::Greater | Binop::GreaterOrEqual =>
                    {
                        if !is_num_type(lhs.ty) {
                            panic!("Numeric type required");
                        }
                    }
                    Binop::Equal | Binop::NotEqual =>
                    {
                        if !is_value_type(lhs.ty) {
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
                        if !is_num_type(e.ty) {
                            panic!("Numeric type required for negation");
                        }
                    }
                    Unop::Not =>
                    {
                        if !is_i32_type(e.ty) {
                            panic!("i32 type required for boolean 'not'");
                        }
                    }
                    Unop::BitNot =>
                    {
                        if !is_int_type(e.ty) {
                            panic!("integer type required for bitwise 'not'");
                        }
                    }
                    Unop::Clz | Unop::Ctz | Unop::Popcnt | Unop::Eqz |
                    Unop::Extend8 | Unop::Extend16 | Unop::Extend32 |
                    Unop::Sqrt | Unop::Ceil | Unop::Floor | Unop::Nearest | Unop::Trunc |
                    Unop::I32ToI64 | Unop::U32ToI64 | Unop::I64ToI32 =>
                    {
                        panic!("Unary operator should not be present at this stage");
                    }
                }
                expr.ty = e.ty;
            }
            Uxpr::Typeop{..} => {
                panic!("NYI");
            }
            Uxpr::Call{name, actuals} => {
                for actual in &mut *actuals {
                    self.check_expr(actual);
                }
                if let Some(b) = self.env.lookup(&name) {
                    match b {
                        Binding::GlobalFun(sig) => {
                            let (formals, ret) = &*sig;
                            if !match_parameters(&formals, actuals) {
                                panic!("Mismatch in function signature");
                            }
                            expr.ty = *ret;
                        }
                        Binding::Intrinsic(sigs, _op) => {
                            let mut found = false;
                            for sig in &*sigs {
                                let (formals, ret) = &**sig;
                                if match_parameters(&formals, actuals) {
                                    found = true;
                                    expr.ty = *ret;
                                    break;
                                }
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
                match self.env.lookup(&id) {
                    Some(Binding::Label) => {
                        panic!("No first-class labels");
                    }
                    Some(Binding::Local(t)) => {
                        expr.ty = Some(t);
                    }
                    Some(Binding::GlobalVar(_mutable, t)) => {
                        expr.ty = Some(t);
                    }
                    Some(Binding::GlobalFun(_)) | Some(Binding::Intrinsic(_, _)) => {
                        panic!("No first-class functions");
                    }
                    Some(Binding::Struct(_)) => {
                        panic!("NYI");
                    }
                    None => {
                        panic!("Reference to unknown variable {}", id)
                    }
                }
            }
            Uxpr::Deref{base, field} => {
                panic!("NYI");
            }
            Uxpr::New{ty_name, values} => {
                panic!("NYI");
            }
            Uxpr::Assign{lhs, rhs} => {
                self.check_expr(rhs);
                match lhs {
                    LValue::Id(id) => {
                        let t = match self.env.lookup(&id) {
                            Some(Binding::Local(t)) =>
                                t,
                            Some(Binding::GlobalVar(mutable, t)) =>
                                if mutable { t } else { panic!("Can't assign to constant"); },
                            _ => 
                                panic!("Not a reference to a variable: {}", &id)
                        };
                        if !is_same_type(Some(t), rhs.ty) {
                            panic!("Type of value being stored does not match variable");
                        }
                    }
                    LValue::Field{base,field} => {
                        panic!("NYI");
                    }
                }
            }
            Uxpr::Block(_) | Uxpr::Iterate{..} | Uxpr::Sequence{..} | Uxpr::Drop(_) |
            Uxpr::Local(_) | Uxpr::Global(_) | Uxpr::SetLocal{..} | Uxpr::SetGlobal{..} => {
                unreachable!();
            }
        }
    }
}
