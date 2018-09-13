// Emitter to SpiderMonkey "wast" format.

use ast::*;

// This trait is probably less interesting than just using functions
// as we do in xform.

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
        let ty = render_type(Some(self.ty));
        let ty = if self.mutable { format!("(mut {})", ty) } else { ty };
        if self.imported {
            em.wast(&format!("(import \"\" \"{}\" (global {}))", self.name.name, ty));
        } else {
            em.wast(&format!("(global ${} {} ", self.name.name, ty));
            self.init.gen(em);
            em.wast(")\n");
        }
    }
}

impl Wast for FnDef {
    fn gen(&self, em:&mut Emitter) {
        let mut params = "".to_string();
        for (name, ty) in &self.formals {
            params.push_str(&format!("(param ${} {}) ", name.name, render_type(Some(*ty))));
        }
        let result =
            if let Some(ty) = self.retn {
                format!("(result {})", render_type(Some(ty)))
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
        if self.items.len() == 1 {
            self.items[0].gen(em);
        } else {
            em.wast(&format!("(block {} ", render_type(self.ty)));
            let limit = self.items.len();
            for idx in 0..limit {
                let item = &self.items[idx];
                if idx < limit-1 {
                    if let BlockItem::Expr(e) = item {
                        if let Some(_) = e.ty {
                            em.wast("drop ");
                        }
                    }
                }
            }
            em.wast(")");
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
        match &self.u {
            Uxpr::If{test, consequent, alternate} => {
                em.wast(&format!("(if {} ", render_type(self.ty)));
                test.gen(em);
                em.wast(" ");
                consequent.gen(em);
                em.wast(" ");
                alternate.gen(em);
                em.wast(")");
            }
            Uxpr::While{test, body} => {
                // TODO
                panic!("NYI");
            }
            Uxpr::Binop{op, lhs, rhs} => {
                em.wast(&format!("({}.{} ", render_type(self.ty), render_binop(*op)));
                lhs.gen(em);
                em.wast(" ");
                rhs.gen(em);
                em.wast(")");
            }
            Uxpr::Unop{op, e} => {
                // TODO
                panic!("NYI");
            }
            Uxpr::Call{name, actuals} => {
                em.wast(&format!("(call ${} ", name.name));
                for arg in actuals {
                    arg.gen(em);
                    em.wast(" ");
                }
                em.wast(")");
            }
            Uxpr::Local(id) => {
                em.wast(&format!("(get_local ${})", id.name));
            }
            Uxpr::Global(id) => {
                em.wast(&format!("(get_global ${})", id.name));
            }
            Uxpr::NumLit(n) => {
                match n {
                    Number::I32(k) => { em.wast(&format!("(i32.const {})", k)) }
                    Number::I64(k) => { em.wast(&format!("(i64.const {})", k)) }
                    Number::F32(k) => { em.wast(&format!("(f32.const {})", k)) }
                    Number::F64(k) => { em.wast(&format!("(f64.const {})", k)) }
                }
            }
            Uxpr::Assign{lhs, rhs} => {
                // TODO
                panic!("NYI");
            }
            Uxpr::Void => {
            }
            Uxpr::Id(_) => {
                panic!("Id should have been removed before now");
            }
        }
    }
}

fn render_type(ty:Option<Type>) -> String {
    match ty {
        Some(Type::I32) => "i32".to_string(),
        Some(Type::I64) => "i64".to_string(),
        Some(Type::F32) => "f32".to_string(),
        Some(Type::F64) => "f64".to_string(),
        Some(Type::AnyRef) => "anyref".to_string(),
        None => "".to_string()
    }
}

fn render_binop(op:Binop) -> String {
    match op {
        // TODO: more
        Binop::Add => "add".to_string(),
        Binop::Sub => "sub".to_string(),
        Binop::Less => "lt_s".to_string(),
        _ => "???".to_string()
    }
}
