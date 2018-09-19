// Remove high-level syntax.  Currently:
//
// - `while` is rewritten as iterate+break
// - `loop` is rewritten as iterate
//
// The desugarer can insert new blocks and let bindings, it just can't
// leave behind new instances of any of the forms it is trying to
// remove.

use ast::*;
use context::Context;
use std::iter::Iterator;
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
    fn new(context: &mut Context) -> Desugarer {
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
            Uxpr::If{test, consequent, alternate} => {
                self.desugar_expr(test);
                self.desugar_block(consequent);
                self.desugar_block(alternate);
            }
            Uxpr::While{test, body} => {
                let mut new_body = make_block(vec![]);
                swap(body, &mut new_body);

                let mut new_test = make_void();
                swap(test, &mut new_test);

                let break_label = self.context.gensym("break");
                let continue_label = self.context.gensym("continue");
                let cond_break = make_if(new_test,
                                         make_block(vec![make_void()]),
                                         make_block(vec![make_break(&break_label)]));

                new_body.items.insert(0, BlockItem::Expr(cond_break));
                let mut new_expr = make_iterate(&break_label, &continue_label, new_body);
                self.desugar_expr(&mut new_expr);
                replacement_expr = Some(new_expr);
            }
            Uxpr::Loop{body, break_label} => {
                let mut new_body = make_block(vec![]);
                swap(body, &mut new_body);

                let continue_label = self.context.gensym("continue");
                let mut new_expr = make_iterate(&break_label, &continue_label, new_body);
                self.desugar_expr(&mut new_expr);
                replacement_expr = Some(new_expr);
            }
            Uxpr::Iterate{body, ..} => {
                self.desugar_block(body);
            }
            Uxpr::Break{..} => { }
            Uxpr::Binop{lhs, rhs, ..} => {
                self.desugar_expr(lhs);
                self.desugar_expr(rhs);
            }
            Uxpr::Unop{e, ..} => {
                self.desugar_expr(e);
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
            Uxpr::Block{..} |
            Uxpr::Local(_) | Uxpr::Global(_) | Uxpr::SetLocal{..} | Uxpr::SetGlobal{..} => {
                panic!("Can't happen - introduced later");
            }
        }
        if let Some(e) = replacement_expr {
            *expr = *e;
        }
    }
}

fn make_block(exprs:Vec<Box<Expr>>) -> Box<Block> {
    Box::new(Block{ ty: None, items: exprs.into_iter().map(|e| BlockItem::Expr(e)).collect() })
}

fn make_if(test:Box<Expr>, consequent:Box<Block>, alternate:Box<Block>) -> Box<Expr> {
    Box::new(Expr{ ty: None, u:  Uxpr::If{ test, consequent, alternate } })
}

fn make_void() -> Box<Expr> {
    Box::new(Expr{ ty: None, u: Uxpr::Void })
}

fn make_break(label:&Id) -> Box<Expr> {
    Box::new(Expr{ ty: None, u: Uxpr::Break{ label: label.clone() } })
}

fn make_iterate(break_label:&Id, continue_label:&Id, body:Box<Block>) -> Box<Expr> {
    Box::new(Expr{ ty:None, u:Uxpr::Iterate{ break_label: break_label.clone(),
                                             continue_label: continue_label.clone(),
                                             body } })
}
