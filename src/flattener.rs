// -*- fill-column: 80 -*-

// The flattener lowers the intermediate wasm format to something very
// close to the final format.  On output:
//
//  - all `let`s are removed, and locals (though not yet parameters) are
//    alpha-converted
//  - all functions carry information about defined locals
//  - all variable references have become get_local/set_local/get_global/
//    set_global operating on alpha-converted names as appropriate
//  - explicit drops have been inserted when needed
//  - redundant blocks have been flattened
//  - Block nodes all have exactly one expression and no let bindings
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
    env:        Env<Id>,

    // Definitons of locals that will go into the flattened functions.
    // The ids are the alpha-renamed names.
    localdefs:  Vec<(Id,Type)>,
}

impl<'a> Flatten<'a>
{
    fn new(context:&'a mut Context) -> Flatten<'a> {
        Flatten {
            context,
            env:       Env::new(),
            localdefs: vec![],
        }
    }

    fn flatten_module(&mut self, m:&mut Module) {
        (&m.items).into_iter().for_each(|item| self.env.define_toplevel(item));
        for item in &mut m.items {
            match item {
                ModItem::Var(v)    => { self.flatten_global(v) }
                ModItem::Fn(f)     => { self.flatten_function(f); }
                ModItem::Struct(s) => { panic!("NYI"); }
            }
        }
    }

    fn flatten_global(&mut self, g:&mut GlobalVar) {
        self.flatten_expr(&mut g.init);
    }

    fn flatten_function(&mut self, f:&mut FnDef) {
        if !f.imported {
            self.env.locals.push_rib();
            (&f.formals).into_iter().for_each(|(name,_ty)| self.env.locals.add_param(name, name.clone()));

            self.flatten_block(&mut f.body);

            self.env.locals.pop_rib();
        }
        f.locals = Some(self.localdefs.split_off(0));
    }

    fn flatten_block(&mut self, b:&mut Block) {
        self.env.locals.push_rib();

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
                    new_exprs.push(box_set_local(&new_name, new_init));
                    self.env.locals.add_local(&l.name, new_name.clone());
                    self.localdefs.push((new_name, l.ty));
                }
                BlockItem::Expr(e) => {
                    let must_drop = i < len-1 && !e.ty.is_none();
                    self.flatten_expr(e);
                    let mut new_e = box_void();
                    swap(e, &mut new_e);

                    let mut moved = false;
                    if let Expr{u: Uxpr::Sequence{body, ..}, ..} = &mut *new_e {
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
            b.items.push(BlockItem::Expr(box_sequence(b.ty, new_exprs)));
        }
        
        self.env.locals.pop_rib();
    }

    fn flatten_expr(&mut self, expr:&mut Expr) {
        let mut replacement_expr = None;
        match &mut expr.u {
            Uxpr::Void => {
                replacement_expr = Some(box_empty_sequence());
            }
            Uxpr::NumLit(_) => { }
            Uxpr::NullLit => { }
            Uxpr::If{test, consequent, alternate} => {
                self.flatten_expr(test);
                self.flatten_block(consequent);
                self.flatten_block(alternate);
            }
            Uxpr::Iterate{body, break_label, continue_label} => {
                self.env.locals.add_label(&break_label);
                self.env.locals.add_label(&continue_label);
                self.flatten_block(body);
            }
            Uxpr::Block(b) => {
                self.flatten_block(b);
                match b.items.len() {
                    0 => {
                        replacement_expr = Some(box_empty_sequence());
                    }
                    1 => {
                        if let BlockItem::Expr(e) = b.items.remove(0) {
                            replacement_expr = Some(e)
                        } else {
                            unreachable!();
                        }
                    }
                    _ => { }
                }
            }
            Uxpr::Break{..} => { }
            Uxpr::Binop{lhs, rhs, ..} => {
                self.flatten_expr(lhs);
                self.flatten_expr(rhs);
            }
            Uxpr::Unop{e, ..} => {
                self.flatten_expr(e);
            }
            Uxpr::Call{actuals, ..} => {
                for actual in &mut *actuals {
                    self.flatten_expr(actual);
                }
            }
            Uxpr::Id(id) => {
                match self.env.lookup(&id) {
                    Some(Binding::Local(new_name)) => {
                        replacement_expr = Some(box_get_local(expr.ty, &new_name));
                    }
                    Some(Binding::GlobalVar(_mutable, _)) => {
                        replacement_expr = Some(box_get_global(expr.ty, &id));
                    }
                    _ => { unreachable!(); }
                }
            }
            Uxpr::Deref{base, field} => {
                panic!("NYI");
            }
            Uxpr::New{ty_name, values} => {
                panic!("NYI");
            }
            Uxpr::Assign{lhs, rhs} => {
                self.flatten_expr(rhs);
                match lhs {
                    LValue::Id(id) => {
                        let mut new_rhs = box_void();
                        swap(rhs, &mut new_rhs);

                        match self.env.lookup(&id) {
                            Some(Binding::Local(new_name)) => {
                                replacement_expr = Some(box_set_local(&new_name, new_rhs));
                            }
                            Some(Binding::GlobalVar(_mutable, _)) => {
                                replacement_expr = Some(box_set_global(&id, new_rhs));
                            }
                            _ => { unreachable!() }
                        }
                    }
                    LValue::Field{base,field} => {
                        panic!("NYI");
                    }
                }
            }
            Uxpr::While{..} | Uxpr::Loop{..} | Uxpr::Sequence{..} | Uxpr::Drop(_) |
            Uxpr::Local(_) | Uxpr::Global(_) | Uxpr::SetLocal{..} | Uxpr::SetGlobal{..} => {
                unreachable!();
            }
        }
        if let Some(e) = replacement_expr {
            *expr = *e;
        }
    }
}
