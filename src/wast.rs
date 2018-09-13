// Emitter to SpiderMonkey "wast" format.

use ast::*;

pub trait Wast {
    fn gen(&self, em:&mut Emitter);
}

pub struct Emitter {}

impl Emitter {
    pub fn new() -> Emitter {
        Emitter {}
    }

    fn js(&mut self, s:&str) {
        print!("JS: {}", s)
    }

    fn wast(&mut self, s:&str) {
        print!("{}", s)
    }
}

impl Wast for Program {
    fn gen(&self, em:&mut Emitter) {
        for item in &self.items {
            item.gen(em);
        }
    }
}

impl Wast for TopItem {
    fn gen(&self, em:&mut Emitter) {
        match self {
            TopItem::Mod(m) => m.gen(em),
            TopItem::Js(s) => em.js(s)
        }
    }
}

impl Wast for Module {
    fn gen(&self, em:&mut Emitter) {
        em.wast("(module\n");
        for item in &self.items {
            item.gen(em);
        }
        em.wast(")\n");
    }
}

impl Wast for ModItem {
    fn gen(&self, em:&mut Emitter) {
        match self {
            ModItem::Var(v) => v.gen(em),
            ModItem::Fn(f) => f.gen(em)
        }
    }
}

impl Wast for GlobalVar {
    fn gen(&self, em:&mut Emitter) {
        // FIXME
    }
}

impl Wast for FnDef {
    fn gen(&self, em:&mut Emitter) {
        let mut params = "".to_string();
        for (name, ty) in &self.formals {
            params.push_str(&format!("(param ${} {}) ", name.name, render_type(*ty)));
        }
        let result =
            if let Some(ty) = self.retn {
                format!("(result {})", render_type(ty))
            } else {
                "".to_string()
            };
        if self.imported {
            em.wast(&format!("(import \"\" \"{}\" (func {} {}))\n",
                                 self.name.name,
                                 params,
                                 result))
        } else {
            let export_clause =
                if self.exported {
                    format!("(export \"{}\")", self.name.name)
                } else {
                    "".to_string()
                };
            em.wast(&format!("(func ${} {} {} {}\n", self.name.name, &export_clause, params, result));
            self.body.gen(em);
            em.wast(")\n");
        }
    }
}

impl Wast for Block {
    fn gen(&self, em:&mut Emitter) {
        for item in &self.items {
            item.gen(em);
        }
    }
}

impl Wast for BlockItem {
    fn gen(&self, em:&mut Emitter) {
        match self {
            BlockItem::Let(_) => { panic!("'let' should have been removed before now") },
            BlockItem::Expr(e) => e.gen(em)
        }
    }
}

impl Wast for Expr {
    fn gen(&self, em:&mut Emitter) {
        match self {
            Expr::If{test, consequent, alternate} => {
                // here we need to emit the common type for the two arms
            }
            Expr::While{test, body} => {
                // labels for break and continue
            }
            Expr::Binop{op, lhs, rhs} => {
                // here we need to know the operand/result type (always the same?)
            }
            Expr::Unop{op, e} => {
                // here we need to know the operand and result types (may be different)
            }
            Expr::Call{name, actuals} => {
                // we assume special functions have been resolved before this
            }
            Expr::Id(id) => {
                // This must carry info about whether local or global
            }
            Expr::NumLit(id) => {
            }
            Expr::Assign{lhs, rhs} => {
            }
            Expr::Void => {
            }
        }
    }
}

fn render_type(ty:Type) -> String {
    match ty {
        Type::I32 => "i32".to_string(),
        Type::I64 => "i64".to_string(),
        Type::F32 => "f32".to_string(),
        Type::F64 => "f64".to_string(),
        Type::AnyRef => "anyref".to_string()
    }
}

