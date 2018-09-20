// -*- fill-column: 80 -*-
//
// Remove high-level syntax and operations that are not defined on the target
// platform.  Currently:
//
// - `while` is rewritten as iterate+break
// - `loop` is rewritten as iterate
// - (bitnot x) is rewritten as (xor x -1)
// - (neg x) is rewritten as (- 0 x)
// - (not x) is rewritten as (eqz x)
//
// - TODO: calls to intrinsics have been rewritten as intrinsic ops (some of
//   them have direct mappings, some require expansion), note this requires
//   maintaining the environment.
// - TODO: (ne x y) is rewritten as (eqz (ref.eq x y)) if x and y are pointers
// - TODO: rewrite x % y as something else if x and y are floats
//
// The desugarer can insert new blocks and variable bindings, it just can't
// leave behind new instances of any of the forms it is trying to remove.

use ast::*;
use context::Context;
use std::mem::swap;

pub fn desugar(context:&mut Context, m:&mut Module) {
    let mut de = Desugarer::new(context);
    de.desugar_module(m);
}

struct Desugarer<'a> {
    context: &'a mut Context
}

impl<'a> Desugarer<'a>
{
    fn new(context: &'a mut Context) -> Desugarer<'a> {
        Desugarer {
            context
        }
    }

    fn desugar_module(&mut self, m:&mut Module) {
        for item in &mut m.items {
            match item {
                ModItem::Var(_v) => {
                    // const exprs don't yet have anything desugarable
                }
                ModItem::Fn(f)  => {
                    if !f.imported {
                        self.desugar_block(&mut f.body);
                    }
                }
            }
        }
    }

    fn desugar_block(&mut self, b:&mut Block) {
        for item in &mut b.items {
            match item {
                BlockItem::Let(l) => {
                    self.desugar_expr(&mut l.init);
                }
                BlockItem::Expr(e) => {
                    self.desugar_expr(e);
                }
            }
        }
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

                let break_label = self.context.gensym("break");
                let continue_label = self.context.gensym("continue");
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

                let continue_label = self.context.gensym("continue");
                let mut new_expr = box_iterate(&break_label, &continue_label, new_body);
                self.desugar_expr(&mut new_expr);
                replacement_expr = Some(new_expr);
            }
            Uxpr::Iterate{body, ..} => {
                self.desugar_block(body);
            }
            Uxpr::Break{..} => { }
            Uxpr::Binop{lhs, rhs, ..} => {
                // TODO: x % y if x and y are float
                // TODO: x != y if x and y are references
                self.desugar_expr(lhs);
                self.desugar_expr(rhs);
            }
            Uxpr::Unop{op, e} => {
                match op {
                    Unop::BitNot => {
                        let mut new_e = box_void();
                        swap(e, &mut new_e);

                        new_e = box_binop(new_e.ty, Binop::BitXor, new_e, box_intlit(-1, e.ty.unwrap()));
                        self.desugar_expr(&mut new_e);
                        replacement_expr = Some(new_e);
                    }
                    Unop::Not => {
                        let mut new_e = box_void();
                        swap(e, &mut new_e);

                        new_e = box_unop(new_e.ty, Unop::Eqz, new_e);
                        self.desugar_expr(&mut new_e);
                        replacement_expr = Some(new_e);
                    }                        
                    Unop::Neg => {
                        let mut new_e = box_void();
                        swap(e, &mut new_e);

                        new_e = box_binop(new_e.ty, Binop::Sub, box_intlit(0, e.ty.unwrap()), new_e);
                        self.desugar_expr(&mut new_e);
                        replacement_expr = Some(new_e);
                    }
                    _ => {
                        self.desugar_expr(e);
                    }
                }
            }
            Uxpr::Call{actuals, ..} => {
                for actual in &mut *actuals {
                    self.desugar_expr(actual);
                }
            }
            Uxpr::Id(_id) => { }
            Uxpr::Assign{lhs, rhs} => {
                self.desugar_expr(rhs);
                match lhs {
                    LValue::Id(_id) => { }
                }
            }
            Uxpr::Block{..} | Uxpr::Drop(_) |
            Uxpr::Local(_) | Uxpr::Global(_) | Uxpr::SetLocal{..} | Uxpr::SetGlobal{..} => {
                panic!("Can't happen - introduced later");
            }
        }
        if let Some(e) = replacement_expr {
            *expr = *e;
        }
    }
}
