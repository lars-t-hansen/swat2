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
use environment::*;
use std::mem::swap;

pub fn flatten(m:&mut Module) {
    let mut f = Flatten::new();
    f.flatten_module(m);
}

struct Flatten
{
    // Environments map names to their alpha-renamings, which are identity for
    // globals and parameters.  Intrinsics don't have renamings.
    env:        Env<Id>,

    // Definitons of locals that will go into the flattened functions.
    // The ids are the alpha-renamed names.
    localdefs:  Vec<(Id,Type)>,
}

impl Flatten
{
    fn new() -> Flatten {
        Flatten {
            env:       Env::new(),
            localdefs: vec![],
        }
    }

    fn flatten_module(&mut self, m:&mut Module) {
        (&m.items).into_iter().for_each(|item| self.env.define_toplevel(item));
        for item in &mut m.items {
            match item {
                ModItem::Var(v)     => { self.flatten_global(v) }
                ModItem::Fn(f)      => { self.flatten_function(f); }
                ModItem::Struct(_s) => { /* Nothing to do */ }
            }
        }
    }

    fn flatten_global(&mut self, g:&mut GlobalDef) {
        self.flatten_expr(&mut g.init);
    }

    fn flatten_function(&mut self, f:&mut FunctionDef) {
        if !f.imported {
            self.env.locals.push_rib();
            (&f.formals).into_iter()
                .for_each(|(name,_ty)| self.env.locals.add_param(name, name.clone()));

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
                    // FIXME: Dodgy gensym use?  What if name is "loop", say?
                    let new_name = Id::gensym(&l.name.name());
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
            Uxpr::Unop{opd, ..} => {
                self.flatten_expr(opd);
            }
            Uxpr::ExactFallibleUnboxAnyRef{value, ..} => {
                self.flatten_expr(value);
            }
            Uxpr::DowncastFailed => { }
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
                    Some(Binding::Global(_mutable, _)) => {
                        replacement_expr = Some(box_get_global(expr.ty, &id));
                    }
                    _ => { unreachable!(); }
                }
            }
            Uxpr::Deref{base, field} => {
                // Rewrite this as GetField
                // The node carries the name of the structure
                panic!("NYI");
            }
            Uxpr::New{values, ..} => {
                // Desugaring has already ordered the initializers, the field
                // names are irrelevant at this point.
                values.into_iter().for_each(|(_,e)| self.flatten_expr(e));
            }
            Uxpr::Assign{lhs, rhs} => {
                self.flatten_expr(rhs);
                match lhs {
                    LValue::Id{name} => {
                        let mut new_rhs = box_void();
                        swap(rhs, &mut new_rhs);

                        match self.env.lookup(&name) {
                            Some(Binding::Local(new_name)) => {
                                replacement_expr = Some(box_set_local(&new_name, new_rhs));
                            }
                            Some(Binding::Global(_mutable, _)) => {
                                replacement_expr = Some(box_set_global(&name, new_rhs));
                            }
                            _ => { unreachable!() }
                        }
                    }
                    LValue::Field{base,field} => {
                        // Rewrite this as SetField
                        panic!("NYI");
                    }
                }
            }
            Uxpr::While{..} | Uxpr::Loop{..} | Uxpr::Sequence{..} | Uxpr::Drop(_) | Uxpr::Typeop{..} |
            Uxpr::GetLocal{..} | Uxpr::GetGlobal{..} | Uxpr::SetLocal{..} | Uxpr::SetGlobal{..} |
            Uxpr::GetField{..} | Uxpr::SetField{..} => {
                unreachable!();
            }
        }
        if let Some(e) = replacement_expr {
            *expr = *e;
        }
    }
}
