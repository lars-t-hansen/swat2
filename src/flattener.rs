// -*- fill-column: 80 -*-

// The flattener lowers the intermediate wasm format to something very
// close to the final format.  On output:
//
//  - all lets are removed, locals (though not parameters) are alpha-converted
//  - all functions carry information about defined locals
//  - all variable references have become get_local/set_local/get_global/set_global
//    operating on alpha-converted names as appropriate
//  - calls to intrinsics have been rewritten as intrinsic ops
//  - operations not directly available in wasm have been rewritten, eg,
//    (bitnot x) => (xor x -1), (neg x) => (- 0 x)
//  - explicit drops have been inserted when needed
//  - redundant blocks have been flattened
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
    context:    &'a mut Context,

    // Environments map names to their alpha-renamings, which are identity for
    // globals and parameters.  Intrinsics don't have renamings.

    intrinsics: IntrinsicEnv<Id>,
    toplevel:   ToplevelEnv<Id>,
    locals:     LocalEnv<Id>,

    // Definitons of locals that will go into the flattened functions.
    // The ids are the alpha-renamed names.
    localdefs:  Vec<(Id,Type)>,
}

impl<'a> Flatten<'a>
{
    fn new(context:&'a mut Context) -> Flatten<'a> {
        Flatten {
            context,
            intrinsics: IntrinsicEnv::new(),
            toplevel:   ToplevelEnv::new(),
            locals:     LocalEnv::new(),
            localdefs:  vec![],
        }
    }

    // TODO: factor this, and the one in typechecker.rs, into environment.rs, to operate
    // on a generic Environment package.

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

        f.locals = Some(self.localdefs.split_off(0));
    }

    fn flatten_block(&mut self, b:&mut Block) {
        self.locals.push_rib();

        let mut new_exprs : Vec<Box<Expr>> = vec![];
        let len = b.items.len();
        for i in 0..len {
            let mut item = &mut b.items[i];
            match item {
                BlockItem::Let(l) => {
                    self.flatten_expr(&mut l.init);
                    let new_name = self.context.gensym(&l.name.name); // FIXME: dodgy? what if name is "loop", say?
                    let mut new_init = box_void();
                    swap(&mut l.init, &mut new_init);
                    new_exprs.push(Box::new(Expr{ty: None,
                                                 u:  Uxpr::SetLocal{name: new_name.clone(),
                                                                    e:    new_init}}));
                    self.locals.add_local(&l.name, new_name.clone());
                    self.localdefs.push((new_name, l.ty));
                }
                BlockItem::Expr(e) => {
                    let must_drop = i < len-1 && !e.ty.is_none();
                    self.flatten_expr(e);
                    let mut new_e = box_void();
                    swap(e, &mut new_e);

                    let mut moved = false;
                    if let Expr{u: Uxpr::Block{body, ..}, ..} = &mut *new_e {
                        moved = true;
                        if body.len() > 0 {
                            if must_drop {
                                let last_e = box_drop(body.pop().unwrap());
                                new_exprs.append(body);
                                new_exprs.push(last_e);
                            } else {
                                new_exprs.append(body);
                            }
                        }
                    }
                    if !moved {
                        if must_drop {
                            new_e = box_drop(new_e);
                        }
                        new_exprs.push(new_e);
                    }
                }
            }
        }

        b.items.clear();
        if new_exprs.len() == 1 {
            b.items.push(BlockItem::Expr(new_exprs.remove(0)));
        } else {
            b.items.push(BlockItem::Expr(Box::new(Expr{ty: b.ty,
                                                       u:  Uxpr::Block{ty:   b.ty,
                                                                       body: new_exprs}})));
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
                        let mut new_e = box_void();
                        swap(e, &mut new_e);

                        new_e = box_binop(new_e.ty, Binop::Sub, box_intlit(0, e.ty.unwrap()), new_e);
                        self.flatten_expr(&mut new_e);
                        replacement_expr = Some(*new_e);
                    }
                    Unop::BitNot => {
                        let mut new_e = box_void();
                        swap(e, &mut new_e);

                        new_e = box_binop(new_e.ty, Binop::BitXor, new_e, box_intlit(-1, e.ty.unwrap()));
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
                if let Binding::Intrinsic(sigs, op) = self.lookup(&name).unwrap() {
                    for sig in &*sigs {
                        let (formals, ret) = &**sig;
                        if match_parameters(&formals, actuals) {
                            match op {
                                Intrin::Binop(op) => {
                                    assert!(actuals.len() == 2);
                                    let mut lhs = box_void();
                                    swap(&mut actuals[0], &mut lhs);
                                    let mut rhs = box_void();
                                    swap(&mut actuals[1], &mut rhs);
                                    replacement_expr = Some(*box_binop(*ret, op, lhs, rhs));
                                }
                                Intrin::Unop(op) => {
                                    assert!(actuals.len() == 1);
                                    let mut e = box_void();
                                    swap(&mut actuals[0], &mut e);
                                    replacement_expr = Some(*box_unop(*ret, op, e));
                                }
                            }
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
                    Some(Binding::GlobalVar(_mutable, _)) => {
                        replacement_expr = Some(Expr{ ty: expr.ty,
                                                      u:  Uxpr::Global(id.clone()) });
                    }
                    _ => { panic!("Can't happen") }
                }
            }
            Uxpr::Assign{lhs, rhs} => {
                self.flatten_expr(rhs);
                match lhs {
                    LValue::Id(id) => {
                        let mut new_rhs = box_void();
                        swap(rhs, &mut new_rhs);

                        match self.lookup(&id) {
                            Some(Binding::Local(new_name)) => {
                                replacement_expr = Some(Expr{ ty: None,
                                                              u:  Uxpr::SetLocal{name: new_name.clone(),
                                                                                 e:    new_rhs} });
                            }
                            Some(Binding::GlobalVar(_mutable, _)) => {
                                replacement_expr = Some(Expr{ ty: None,
                                                              u:  Uxpr::SetGlobal{name: id.clone(),
                                                                                  e:    new_rhs} });
                            }
                            _ => { panic!("Can't happen") }
                        }
                    }

                }
            }
            Uxpr::While{..} | Uxpr::Loop{..} | Uxpr::Block{..} | Uxpr::Drop(_) |
            Uxpr::Local(_) | Uxpr::Global(_) | Uxpr::SetLocal{..} | Uxpr::SetGlobal{..} => {
                panic!("Can't happen - introduced later");
            }
        }
        if let Some(e) = replacement_expr {
            *expr = e;
        }
    }
}

