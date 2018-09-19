// -*- fill-column: 80 -*-

// The flattener lowers the intermediate wasm format to something very
// close to the final format.  On output:
//
//  - all lets are removed, locals are alpha-converted
//  - all functions carry lists of defined locals
//  - all variable references have become get_local/set_local/get_global/set_global
//    operating on alpha-converted names (except for parameters)
//  - calls to intrinsics have been rewritten as intrinsic ops
//  - operations not directly available in wasm have been rewritten, eg,
//    (bitnot x) => (xor x -1), (neg x) => (- 0 x)
//  - explicit drops have been inserted when needed
//  - remove redundant blocks
//  - resulting Block nodes all have exactly one expression and no let bindings
//  - void expressions have been removed, replaced by empty Block expressions

use ast::*;
use context::Context;
use environment::*;
use std::mem::swap;

pub fn flatten(cx:&mut Context, m:&mut Module) {
    let mut f = Flatten::new(cx);
    f.flatten_module(m);
}

struct Flatten<'a>
{
    // Environments map names to their alpha-renamings, which are identity for
    // globals and parameters.  Intrinsics don't have renamings.

    intrinsics: IntrinsicEnv<Id>,
    toplevel:   ToplevelEnv<Id>,
    locals:     LocalEnv<Id>,
    context:    &'a mut Context
}

impl<'a> Flatten<'a>
{
    fn new(context:&'a mut Context) -> Flatten<'a> {
        Flatten {
            intrinsics: IntrinsicEnv::new(),
            toplevel:   ToplevelEnv::new(),
            locals:     LocalEnv::new(),
            context
        }
    }

    fn lookup(&mut self, id:&Id) -> Option<Binding<Id>> {
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
    
    fn flatten_module(&mut self, m:&mut Module) {
        for item in &mut m.items {
            match item {
                ModItem::Var(v) => { self.flatten_global(v) }
                ModItem::Fn(f)  => { self.flatten_function(f); }
            }
        }
    }

    fn flatten_global(&mut self, g:&mut GlobalVar) {
        self.flatten_expr(&mut g.init);
        self.toplevel.insert_global(&g.name, g.mutable, g.ty);
    }

    fn flatten_function(&mut self, f:&mut FnDef) {
        let param_types = (&f.formals).into_iter().map(|(_,ty)| *ty).collect();
        self.toplevel.insert_function(&f.name, param_types, f.retn);

        if !f.imported {
            self.locals.push_rib();
            (&f.formals).into_iter().for_each(|(name,_ty)| self.locals.add_param(name, name.clone()));

            self.flatten_block(&mut f.body);

            self.locals.pop_rib();
        }

        // FIXME: attach renamed locals
    }

    fn flatten_block(&mut self, b:&mut Block) {
        self.locals.push_rib();

        // FIXME: insert drops where necessary
        // FIXME: block nodes should contain exactly one expression,
        //        which can be a block expression or any other expression;
        //        if it is a block expression it contains zero or more
        //        than one other expressions

        for item in &mut b.items {
            match item {
                BlockItem::Let(l) => {
                    self.flatten_expr(&mut l.init);
                    // FIXME: remove the let, turn this node into a void expr node
                    // or generate a new vector of nodes...
                }
                BlockItem::Expr(e) => {
                    self.flatten_expr(e);
                    // FIXME: if the e after flattening is a Uxpr::Block, inline it
                }
            }
        }

        self.locals.pop_rib();
    }

    fn flatten_expr(&mut self, expr:&mut Expr) {
        let mut replacement_expr = None;
        match &mut expr.u {
            Uxpr::Void => {
                replacement_expr = Some(Expr{ty: None,
                                             u:  Uxpr::Block{ty:None, body:vec![]}});
            }
            Uxpr::NumLit(_) => { }
            Uxpr::If{test, consequent, alternate} => {
                self.flatten_expr(test);
                self.flatten_block(consequent);
                self.flatten_block(alternate);
            }
            Uxpr::Iterate{body, ..} => {
                self.flatten_block(body);
            }
            Uxpr::Break{..} => { }
            Uxpr::Binop{lhs, rhs, ..} => {
                self.flatten_expr(lhs);
                self.flatten_expr(rhs);
            }
            Uxpr::Unop{op, e} => {
                match op {
                    Unop::Neg => {
                        let mut new_e = make_void();
                        swap(e, &mut new_e);

                        new_e = make_binop(new_e.ty, Binop::Sub, make_intlit(0, e.ty.unwrap()), new_e);
                        self.flatten_expr(&mut new_e);
                        replacement_expr = Some(*new_e);
                    }
                    Unop::BitNot => {
                        let mut new_e = make_void();
                        swap(e, &mut new_e);

                        new_e = make_binop(new_e.ty, Binop::BitXor, new_e, make_intlit(-1, e.ty.unwrap()));
                        self.flatten_expr(&mut new_e);
                        replacement_expr = Some(*new_e);
                    }
                    _ => {
                        self.flatten_expr(e);
                    }
                }
            }
            Uxpr::Call{name, actuals} => {
                for actual in &mut *actuals {
                    self.flatten_expr(actual);
                }
                if let Binding::Intrinsic(sigs) = self.lookup(&name).unwrap() {
                    for sig in &*sigs {
                        let (formals, ret) = &**sig;
                        if match_parameters(&formals, actuals) {
                            // FIXME: rewrite as this intrinsic
                            break;
                        }
                    }
                }
            }
            Uxpr::Id(id) => {
                match self.lookup(&id) {
                    Some(Binding::Local(new_name)) => {
                        replacement_expr = Some(Expr{ ty: expr.ty,
                                                      u:  Uxpr::Local(new_name.clone()) });
                    }
                    Some(Binding::GlobalVar(_mutable, _ty)) => {
                        replacement_expr = Some(Expr{ ty: expr.ty,
                                                      u:  Uxpr::Global(id.clone()) });
                    }
                    _ => { panic!("Can't happen") }
                }
            }
            Uxpr::Assign{lhs, rhs} => {
                self.flatten_expr(rhs);
                match lhs {
                    LValue::Id(_id) => { /* FIXME: rewrite as setlocal / setglobal */ }
                }
            }
            Uxpr::While{..} | Uxpr::Loop{..} | Uxpr::Block{..} |
            Uxpr::Local(_) | Uxpr::Global(_) | Uxpr::SetLocal{..} | Uxpr::SetGlobal{..} => {
                panic!("Can't happen - introduced later");
            }
        }
        if let Some(e) = replacement_expr {
            *expr = e;
        }
    }
}

