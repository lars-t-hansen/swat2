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

use ast::*;

pub fn flatten(m:&mut Module) {
    let mut f = Flatten::new();
    f.flatten_module(m);
}

struct Flatten {
}

impl Flatten {
    fn new() -> Flatten {
        Flatten {}
    }

    fn flatten_module(&mut self, m:&mut Module) {
        for item in &mut m.items {
            match item {
                ModItem::Var(_v) => { /* const exprs don't yet have anything flattenable */ }
                ModItem::Fn(f)  => { self.flatten_function(f); }
            }
        }
    }

    fn flatten_function(&mut self, f:&mut FnDef) {
        if !f.imported {
            self.flatten_block(&mut f.body);
        }
        // FIXME: attach renamed locals
    }

    fn flatten_block(&mut self, b:&mut Block) {
        // FIXME: flatten blocks within blocks
        // FIXME: insert drops where necessary
        // FIXME: block nodes should contain exactly one expression,
        //        which can be a block expression or any other expression
        for item in &mut b.items {
            match item {
                BlockItem::Let(l) => {
                    self.flatten_expr(&mut l.init);
                    // FIXME: remove the let, turn this node into a void expr node
                    // or generate a new vector of nodes...
                }
                BlockItem::Expr(e) => {
                    self.flatten_expr(e);
                }
            }
        }
    }

    fn flatten_expr(&mut self, expr:&mut Expr) {
        match &mut expr.u {
            Uxpr::Void => { }
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
            Uxpr::Unop{e, ..} => {
                // FIXME: rewrite unops
                self.flatten_expr(e);
            }
            Uxpr::Call{actuals, ..} => {
                // FIXME: rewrite intrinsic calls
                for actual in &mut *actuals {
                    self.flatten_expr(actual);
                }
            }
            Uxpr::Id(_id) => {
                // FIXME: rewrite as getlocal / getglobal
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
    }
}
