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
use environment::*;
use std::collections::HashSet;

pub fn check(p:&mut Module) {
    let mut chk = Check::new();
    chk.check_module(p);
}

struct Check
{
    intrinsics: IntrinsicEnv<Type>,
    toplevel:   ToplevelEnv<Type>,
    locals:     LocalEnv<Type>
}

impl Check
{
    fn new() -> Check {
        Check {
            intrinsics: IntrinsicEnv::new(),
            toplevel:   ToplevelEnv::new(),
            locals:     LocalEnv::new()
        }
    }

    fn lookup(&mut self, id:&Id) -> Option<Binding<Type>> {
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
    
    fn check_module(&mut self, m:&mut Module) {
        for item in &mut m.items {
            match item {
                ModItem::Var(v) => { self.bind_global(v); }
                ModItem::Fn(f)  => { self.bind_function(f); }
            }
        }
        for item in &mut m.items {
            match item {
                ModItem::Var(v) => { self.check_global(v); }
                ModItem::Fn(f)  => { self.check_function(f); }
            }
        }
    }

    fn bind_global(&mut self, g:&mut GlobalVar) {
        if self.toplevel.probe(&g.name) {
            panic!("Multiply defined top-level name {}", g.name);
        }
        self.toplevel.insert_global(&g.name, g.mutable, g.ty);
    }

    fn check_global(&mut self, g:&mut GlobalVar) {
        if !g.imported {
            self.check_const_expr(&mut g.init);
            if !is_same_type(Some(g.ty), g.init.ty) {
                panic!("Init expression type mismatch");
            }
        }
    }

    fn bind_function(&mut self, f:&mut FnDef) {
        if self.toplevel.probe(&f.name) {
            panic!("Multiply defined top-level name {}", f.name);
        }
        let param_types = (&f.formals).into_iter().map(|(_,ty)| *ty).collect();
        self.toplevel.insert_function(&f.name, param_types, f.retn);
    }
    
    fn check_function(&mut self, f:&mut FnDef) {
        self.locals.push_rib();

        let mut param_names = HashSet::<String>::new();
        for (param_name, param_type) in &f.formals {
            if param_names.contains(&param_name.name) {
                panic!("Duplicate parameter name {}", param_name);
            }
            param_names.insert(param_name.name.clone());
            self.locals.add_param(param_name, *param_type);
        }

        if !f.imported {
            self.check_block(&mut f.body);
            if !is_same_type(f.retn, f.body.ty) {
                panic!("Return type / body type mismatch");
            }
        }

        self.locals.pop_rib();
    }

    fn check_block(&mut self, b:&mut Block) {
        self.locals.push_rib();

        let mut last_type = None;
        for item in &mut b.items {
            match item {
                BlockItem::Let(l) => {
                    self.check_expr(&mut l.init);
                    if !is_same_type(l.init.ty, Some(l.ty)) {
                        panic!("Initializer does not have same type as variable {}", &l.name);
                    }
                    self.locals.add_local(&l.name, l.ty);
                    last_type = l.init.ty;
                }
                BlockItem::Expr(e) => {
                    self.check_expr(e);
                    last_type = e.ty;
                }
            }
        }
        b.ty = last_type;

        self.locals.pop_rib();
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
                self.locals.push_rib();
                self.locals.add_label(&break_label);
                self.check_block(body);
                self.locals.pop_rib();
                assert!(expr.ty.is_none());
            }
            Uxpr::Break{label} => {
                match self.lookup(&label) {
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
                if let Some(b) = self.lookup(&name) {
                    match b {
                        Binding::GlobalFun(sig) => {
                            let (formals, ret) = &*sig;
                            if !match_parameters(&formals, actuals) {
                                panic!("Mismatch in function signature");
                            }
                            expr.ty = *ret;
                        }
                        Binding::Intrinsic(sigs) => {
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
                match self.lookup(&id) {
                    Some(Binding::Label) => {
                        panic!("No first-class labels");
                    }
                    Some(Binding::Local(t)) => {
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
                        let t = match self.lookup(&id) {
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
                }
            }
            Uxpr::Iterate{..} | Uxpr::Block{..} |
            Uxpr::Local(_) | Uxpr::Global(_) | Uxpr::SetLocal{..} | Uxpr::SetGlobal{..} => {
                panic!("Can't happen - introduced later");
            }
        }
    }
}
