// -*- fill-column: 100 -*-
//
// Remove high-level syntax and operations that are not defined on the target
// platform.  Currently:
//
// - `while` is rewritten as iterate+break
// - `loop` is rewritten as iterate
// - x % y is expanded to its computation if x and y are floats
// - (not-equal x y) is rewritten as (eqz (equal x y)) if x and y are pointers
// - (bitnot x) is rewritten as (xor x -1)
// - (neg x) is rewritten as (- 0 x)
// - (not x) is rewritten as (eqz x)
// - calls to intrinsics are rewritten as intrinsic ops (some of them have
//   direct mappings, some require expansion)
// - `new` is rewritten with initializers in struct declaration order
//
// The desugarer can insert new blocks and variable bindings, it just can't leave behind new
// instances of any of the forms it is trying to remove.

use ast::*;
use environment::*;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::mem::swap;

pub fn desugar(m:&mut Module) {
    let mut de = Desugarer::new();
    de.desugar_module(m);
}

struct Desugarer {
    // The carried Type value is not used here, we use environments simply to discover whether
    // something is an intrinsic or not.
    env:     Env<Type>
}

impl Desugarer
{
    fn new() -> Desugarer {
        Desugarer {
            env: Env::new()
        }
    }

    fn desugar_module(&mut self, m:&mut Module) {
        (&m.items).into_iter().for_each(|item| self.env.define_toplevel(item));
        for item in &mut m.items {
            match item {
                ModItem::Var(g)     => { self.desugar_global(g); }
                ModItem::Fn(f)      => { self.desugar_function(f); }
                ModItem::Struct(_s) => { }
            }
        }
    }

    fn desugar_global(&mut self, g:&mut GlobalDef) {
        self.desugar_expr(&mut g.init);
    }

    fn desugar_function(&mut self, f:&mut FunctionDef) {
        if !f.imported {
            self.env.locals.push_rib();
            (&f.formals).into_iter().for_each(|(name,ty)| self.env.locals.add_param(name, *ty));
            self.desugar_block(&mut f.body);
            self.env.locals.pop_rib();
        }
    }

    fn desugar_block(&mut self, b:&mut Block) {
        self.env.locals.push_rib();
        for item in &mut b.items {
            match item {
                BlockItem::Let(l) => {
                    self.desugar_expr(&mut l.init);
                    self.env.locals.add_local(&l.name, l.ty);
                }
                BlockItem::Expr(e) => {
                    self.desugar_expr(e);
                }
            }
        }
        self.env.locals.pop_rib();
    }

    fn desugar_expr(&mut self, expr:&mut Expr) {
        let mut replacement_expr = None;
        match &mut expr.u {
            Uxpr::Void => { }
            Uxpr::NumLit(_) => { }
            Uxpr::NullLit => { }
            Uxpr::If{test, consequent, alternate} => {
                self.desugar_expr(test);
                self.desugar_block(consequent);
                self.desugar_block(alternate);
            }
            Uxpr::While{test, body} => {
                let mut new_body = box_block(vec![]);
                swap(body, &mut new_body);

                let mut new_test = box_void();
                swap(test, &mut new_test);

                let break_label = Id::gensym("break");
                let continue_label = Id::gensym("continue");
                let cond_break = box_if(new_test,
                                        box_block(vec![box_void()]),
                                        box_block(vec![box_break(&break_label)]));

                new_body.items.insert(0, BlockItem::Expr(cond_break));
                let mut new_expr = box_iterate(&break_label, &continue_label, new_body);
                self.desugar_expr(&mut new_expr);
                replacement_expr = Some(new_expr);
            }
            Uxpr::Loop{body, break_label} => {
                let mut new_body = box_block(vec![]);
                swap(body, &mut new_body);

                let continue_label = Id::gensym("continue");
                let mut new_expr = box_iterate(&break_label, &continue_label, new_body);
                self.desugar_expr(&mut new_expr);
                replacement_expr = Some(new_expr);
            }
            Uxpr::Iterate{body, break_label, continue_label} => {
                self.env.locals.add_label(&break_label);
                self.env.locals.add_label(&continue_label);
                self.desugar_block(body);
            }
            Uxpr::Break{..} => { }
            Uxpr::Binop{op, lhs, rhs} => {
                match op {
                    Binop::NotEqual if is_ref_or_anyref_type(lhs.ty)  => {
                        let mut new_lhs = box_void();
                        swap(lhs, &mut new_lhs);
                        let mut new_rhs = box_void();
                        swap(rhs, &mut new_rhs);
                        let cmp = box_binop(Some(Type::I32), Binop::Equal, new_lhs, new_rhs);
                        replacement_expr = Some(box_unop(Some(Type::I32), Unop::Eqz, cmp));
                    }
                    Binop::Rem if is_float_type(lhs.ty) => {
                        let tmp_x = Id::gensym("tmp");
                        let tmp_y = Id::gensym("tmp");
                        let ty = lhs.ty;

                        let mut new_lhs = box_void();
                        swap(lhs, &mut new_lhs);

                        let mut new_rhs = box_void();
                        swap(rhs, &mut new_rhs);

                        let mut block_items = vec![];
                        block_items.push(BlockItem::Let(Box::new(LetDefn { name: tmp_x, ty: ty.unwrap(), init: new_lhs })));
                        block_items.push(BlockItem::Let(Box::new(LetDefn { name: tmp_y, ty: ty.unwrap(), init: new_rhs })));
                        let div = box_binop(ty, Binop::Div, box_id(ty, &tmp_x), box_id(ty, &tmp_y));
                        let trunc = box_unop(ty, Unop::Trunc, div);
                        let mul = box_binop(ty, Binop::Mul, trunc, box_id(ty, &tmp_y));
                        let diff = box_binop(ty, Binop::Sub, box_id(ty, &tmp_x), mul);
                        block_items.push(BlockItem::Expr(diff));
                        replacement_expr = Some(Box::new(Expr { ty, u:  Uxpr::Block(Block{ ty, items: block_items}) }));
                    }
                    _ => {
                        self.desugar_expr(lhs);
                        self.desugar_expr(rhs);
                    }
                }
            }
            Uxpr::Unop{op, opd} => {
                self.desugar_expr(opd);
                match op {
                    Unop::BitNot => {
                        let mut new_opd = box_void();
                        swap(opd, &mut new_opd);
                        replacement_expr = Some(box_binop(new_opd.ty,
                                                          Binop::BitXor,
                                                          new_opd,
                                                          box_intlit(-1, opd.ty.unwrap())));
                    }
                    Unop::Not => {
                        let mut new_opd = box_void();
                        swap(opd, &mut new_opd);
                        replacement_expr = Some(box_unop(new_opd.ty, Unop::Eqz, new_opd));
                    }                        
                    Unop::Neg => {
                        let mut new_opd = box_void();
                        swap(opd, &mut new_opd);
                        replacement_expr = Some(box_binop(new_opd.ty,
                                                          Binop::Sub,
                                                          box_intlit(0, opd.ty.unwrap()),
                                                          new_opd));
                    }
                    _ => { }
                }
            }
            Uxpr::Typeop{lhs, ..} => {
                self.desugar_expr(lhs);
            }
            Uxpr::Call{name, actuals} => {
                for actual in &mut *actuals {
                    self.desugar_expr(actual);
                }
                if let Binding::Intrinsic(sigs, op) = self.env.lookup(&name).unwrap() {
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
                                    replacement_expr = Some(box_binop(*ret, op, lhs, rhs));
                                }
                                Intrin::Unop(op) => {
                                    assert!(actuals.len() == 1);
                                    let mut e = box_void();
                                    swap(&mut actuals[0], &mut e);
                                    replacement_expr = Some(box_unop(*ret, op, e));
                                }
                            }
                            break;
                        }
                    }
                }
            }
            Uxpr::Id(_id) => { }
            Uxpr::Deref{base, ..} => {
                self.desugar_expr(base);
            }
            Uxpr::New{ty_name, values} => {
                // Reorganize the values so that they are in the right order for eventual structure
                // construction (and so the names in the initializer list can be ignored).  Preserve
                // the order of side effects.

                let mut block_items = vec![];
                let mut initializers = vec![];

                for (field, mut value) in values.drain(0..) {
                    self.desugar_expr(&mut value);
                    let tmp_name = Id::gensym("tmp");
                    let field_ty = value.ty.unwrap();
                    block_items.push(BlockItem::Let(Box::new(LetDefn { name: tmp_name,
                                                                       ty:   field_ty,
                                                                       init: value })));
                    initializers.push((field, Box::new(Expr { ty: Some(field_ty), u: Uxpr::Id(tmp_name) })))
                }

                let mut indices = HashMap::new();
                match self.env.lookup(ty_name) {
                    Some(Binding::Struct(s)) => {
                        let fields = &s.1;
                        let mut k = 0;
                        fields.into_iter().for_each(|(name,_)| {
                            indices.insert(name.clone(), k);
                            k += 1;
                        });
                    }
                    _ => unreachable!()
                }
                initializers.sort_by(|(a,_),(b,_)| {
                    let a = indices.get(a).unwrap();
                    let b = indices.get(b).unwrap();
                    if a < b { Ordering::Less }
                    else if a > b { Ordering::Greater }
                    else { Ordering::Equal }
                });

                block_items.push(BlockItem::Expr(Box::new(Expr { ty: expr.ty,
                                                                 u:  Uxpr::New{ ty_name: *ty_name,
                                                                                values:  initializers } })));
                replacement_expr = Some(Box::new(Expr { ty: expr.ty,
                                                        u:  Uxpr::Block(Block{ ty:    expr.ty,
                                                                               items: block_items }) }));
            }
            Uxpr::Assign{lhs, rhs} => {
                self.desugar_expr(rhs);
                match lhs {
                    LValue::Id(_id) => { }
                    LValue::Field{base, ..} => {
                        self.desugar_expr(base);
                    }
                }
            }
            Uxpr::Block(_) | Uxpr::Sequence{..} | Uxpr::Drop(_) |
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
