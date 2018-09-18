// Remove high-level syntax:
//
// - `while` is rewritten as loop+break
//
// In the future:
//
// - rewrite many other forms
//
// Notably, the desugarer can insert new blocks and let bindings, it
// just can't insert the forms it is trying to remove.

use ast::*;

pub fn desugar(m:&mut Module) {
    let mut de = Desugarer::new();
    de.desugar_module(m);
}

struct Desugarer {
}

impl Desugarer
{
    fn new() -> Desugarer {
        Desugarer { }
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
                // FIXME
                // This becomes: (loop $l (if (not test) (break $b)) body) with new name $l
                self.desugar_expr(test);
                self.desugar_block(body);
                /*
                let label = ...;
                replacement_expr =
                (None,
                Uxpr::Loop{
                label,
                body: Block {
                ty: None,
                body:
                vec![Expr {
                ty:None
                u:Uxpr::Loop{
                name: loop_name,
                body: Block {
                ty:None,
                vec![Expr{ty:None,
                 */
            }
            Uxpr::Loop{body, ..} => {
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
                    LValue::Local(_) | LValue::Global(_) => { panic!("Can't happen"); }
                }
            }
            Uxpr::Local(_) | Uxpr::Global(_) => { panic!("Can't happen"); }
        }
        if let Some((ty, u)) = replacement_expr {
            expr.ty = ty;
            expr.u = u;
        }
    }
}
