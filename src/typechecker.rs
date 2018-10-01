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
// In the future, it may also insert explicit casts where they are implicit.

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
        check_unique_names(&m.items,
                           |item| {
                               match item {
                                   ModItem::Var(v) => v.name,
                                   ModItem::Fn(f) => f.name,
                                   ModItem::Struct(s) => s.name
                               }
                           },
                           "toplevel");

        // Introduce names with their roles but null contents; the next step can
        // only use the role information, no details.
        for item in &mut m.items {
            match item {
                ModItem::Var(v)    => { self.env.predefine_global(v); }
                ModItem::Fn(f)     => { self.env.predefine_function(f); }
                ModItem::Struct(s) => { self.env.predefine_struct(s); }
            }
        }

        // Cook the outward visible types and update the environment with the
        // completed information.
        for item in &mut m.items {
            match item {
                ModItem::Struct(s) => {
                    self.cook_struct(s);
                    self.env.elaborate_struct(s);
                }
                ModItem::Var(v) => {
                    self.cook_global(v);
                    self.env.elaborate_global(v);
                }
                ModItem::Fn(f) => {
                    self.cook_function(f);
                    self.env.elaborate_function(f);
                }
            }
        }

        // Check init expressions and function bodies against the global env and
        // declared types.
        for item in &mut m.items {
            match item {
                ModItem::Var(v)     => { self.check_global(v); }
                ModItem::Fn(f)      => { self.check_function(f); }
                ModItem::Struct(_s) => { /* nothing left to do */ }
            }
        }
    }

    fn cook_struct(&mut self, s:&mut StructDef) {
        check_unique_names(&s.fields, |(name,_)| *name, "field");
        (&mut s.fields).into_iter().for_each(|(_,ty)| self.check_type(ty));
    }

    fn cook_global(&mut self, g:&mut GlobalDef) {
        self.check_type(&mut g.ty);
    }
    
    fn check_global(&mut self, g:&mut GlobalDef) {
        if (g.exported || g.imported) && is_ref_type(Some(g.ty)) {
            panic!("Non-private global can't be of ref type (yet)");
        }

        if !g.imported {
            self.check_const_expr(&mut g.init);
            if !is_same_type(Some(g.ty), g.init.ty) {
                panic!("Init expression type mismatch");
            }
        }
    }

    fn cook_function(&mut self, f:&mut FunctionDef) {
        check_unique_names(&f.formals, |(name,_)| *name, "parameter");
        (&mut f.formals).into_iter().for_each(|(_,ty)| self.check_type(ty));
        self.check_type_or_void(&mut f.retn);
    }

    fn check_function(&mut self, f:&mut FunctionDef) {
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
                    panic!("Binop requires equal types {} {}", fmt_type(lhs.ty), fmt_type(rhs.ty));
                }
                match op {
                    Binop::UDiv | Binop::URem |
                    Binop::ShiftLeft | Binop::ShiftRight | Binop::UShiftRight |
                    Binop::BitAnd | Binop::BitOr | Binop::BitXor |
                    Binop::ULess | Binop::ULessOrEqual | Binop::UGreater | Binop::UGreaterOrEqual |
                    Binop::RotLeft | Binop::RotRight => {
                        if !is_int_type(lhs.ty) {
                            panic!("Integer type required");
                        }
                    }
                    Binop::Copysign => {
                        if !is_float_type(lhs.ty) {
                            panic!("Floating type required");
                        }
                    }
                    Binop::Add | Binop::Sub | Binop::Mul | Binop::Div | Binop::Rem |
                    Binop::Less | Binop::LessOrEqual | Binop::Greater | Binop::GreaterOrEqual => {
                        if !is_num_type(lhs.ty) {
                            panic!("Numeric type required");
                        }
                    }
                    Binop::Equal | Binop::NotEqual => {
                        if !is_value_type(lhs.ty) {
                            panic!("Non-void type required");
                        }
                    }
                }
                match op {
                    Binop::ULess | Binop::ULessOrEqual | Binop::UGreater | Binop::UGreaterOrEqual |
                    Binop::Less | Binop::LessOrEqual | Binop::Greater | Binop::GreaterOrEqual |
                    Binop::Equal | Binop::NotEqual => {
                        expr.ty = Some(Type::I32);
                    }
                    _ => {
                        expr.ty = lhs.ty;
                    }
                }
            }
            Uxpr::Unop{op, opd} => {
                self.check_expr(opd);
                match op {
                    Unop::Neg => {
                        if !is_num_type(opd.ty) {
                            panic!("Numeric type required for negation");
                        }
                    }
                    Unop::Not => {
                        if !is_i32_type(opd.ty) {
                            panic!("i32 type required for boolean 'not'");
                        }
                    }
                    Unop::BitNot => {
                        if !is_int_type(opd.ty) {
                            panic!("integer type required for bitwise 'not'");
                        }
                    }
                    Unop::Clz | Unop::Ctz | Unop::Popcnt | Unop::Eqz |
                    Unop::Extend8 | Unop::Extend16 | Unop::Extend32 | Unop::IsNull |
                    Unop::Sqrt | Unop::Ceil | Unop::Floor | Unop::Nearest | Unop::Trunc |
                    Unop::I32ToI64 | Unop::U32ToI64 | Unop::I64ToI32 => {
                        unreachable!();
                    }
                }
                expr.ty = opd.ty;
            }
            Uxpr::Typeop{op, lhs, rhs} => {
                self.check_expr(lhs);
                self.check_type(rhs);
                if !is_ref_or_anyref_type(lhs.ty) {
                    panic!("Left hand side of type operator must have reference type");
                }
                if !is_ref_or_anyref_type(Some(*rhs)) {
                    panic!("Right hand side of type operator must have reference type");
                }
                // If both are struct types then they must be the same struct type.
                let rhs = *rhs;
                match (lhs.ty, &rhs) {
                    (Some(Type::CookedRef(lhs_struct)), Type::CookedRef(rhs_struct)) => {
                        if lhs_struct != *rhs_struct {
                            panic!("Incompatible types in type operator");
                        }
                    }
                    _ => { }
                }
                expr.ty = Some(match op {
                    Typeop::Is => { Type::I32 }
                    Typeop::As => { rhs }
                })
            }
            Uxpr::Call{name, actuals} => {
                for actual in &mut *actuals {
                    self.check_expr(actual);
                }
                if let Some(b) = self.env.lookup(&name) {
                    match b {
                        Binding::Function(sig) => {
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
                    Some(Binding::Global(_mutable, t)) => {
                        expr.ty = Some(t);
                    }
                    Some(Binding::Function(_)) | Some(Binding::Intrinsic(_, _)) => {
                        panic!("No first-class functions");
                    }
                    Some(Binding::Struct(_)) => {
                        panic!("No first-class types");
                    }
                    None => {
                        panic!("Reference to unknown variable {}", id)
                    }
                }
            }
            Uxpr::Deref{base, field} => {
                self.check_expr(base);
                let ty = self.check_struct_ref(base, field);
                expr.ty = Some(ty);
            }
            Uxpr::New{ty_name, values} => {
                values.into_iter().for_each(|(_,e)| self.check_expr(e));
                match self.env.lookup(ty_name) {
                    Some(Binding::Struct(s)) => {
                        let (_, fields) = &*s;
                        check_unique_names(values, |(name,_)| *name, "initializer");
                        if values.len() != fields.len() {
                            panic!("Wrong number of initializers");
                        }
                        // Since the initializer names are unique and their
                        // number matches the number of fields, we can verify
                        // that the initializer matches the struct exactly by
                        // checking that every field in the initializer is also
                        // in the struct.
                        for (field, value) in values {
                            if let Some((_, ty)) = fields.into_iter().find(|(n,_)| n == field) {
                                if !is_same_type(value.ty, Some(*ty)) {
                                    panic!("Initializer expression type does not match field type");
                                }
                            } else {
                                panic!("Initializer for undefined field: {}", field);
                            }
                        }
                        expr.ty = Some(Type::CookedRef(*ty_name));
                    }
                    _ => {
                        panic!("Type name in `new` must name a struct type");
                    }
                }
            }
            Uxpr::Assign{lhs, rhs} => {
                self.check_expr(rhs);
                match lhs {
                    LValue::Id(id) => {
                        let t = match self.env.lookup(&id) {
                            Some(Binding::Local(t)) =>
                                t,
                            Some(Binding::Global(mutable, t)) =>
                                if mutable { t } else { panic!("Can't assign to constant"); },
                            _ => 
                                panic!("Not a reference to a variable: {}", &id)
                        };
                        if !is_same_type(Some(t), rhs.ty) {
                            panic!("Type of value being stored does not match variable");
                        }
                    }
                    LValue::Field{base,field} => {
                        self.check_expr(base);
                        let ty = self.check_struct_ref(base, field);
                        if !is_same_type(Some(ty), rhs.ty) {
                            panic!("Type of value being stored does not match field");
                        }
                    }
                }
            }
            Uxpr::Block(_) | Uxpr::Iterate{..} | Uxpr::Sequence{..} | Uxpr::Drop(_) |
            Uxpr::ExactFallibleUnboxAnyRef{..} |
            Uxpr::GetLocal{..} | Uxpr::GetGlobal{..} | Uxpr::SetLocal{..} | Uxpr::SetGlobal{..} |
            Uxpr::GetField{..} | Uxpr::SetField{..} => {
                unreachable!();
            }
        }
    }

    fn check_struct_ref(&mut self, base:&Box<Expr>, field:&Id) -> Type {
        match base.ty {
            Some(Type::CookedRef(s_name)) => {
                let (_,fields) = &*self.env.get_struct_def(&s_name);
                let the_field = fields.into_iter().find(|(name,_)| name == field);
                match the_field {
                    Some((_,ty)) => *ty,
                    None => {
                        panic!("Attempting to access field {} from struct that does not have it {}",
                               field, fmt_type(base.ty));
                    }
                }
            }
            _ => {
                panic!("Attempting to access field {} on non-struct type {}",
                       field, fmt_type(base.ty));
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
