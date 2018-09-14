// Emitter to SpiderMonkey "wast" format.

use ast::*;

// This trait is probably less interesting than just using functions
// as we do in xform.

pub trait Wast {
    fn gen(&self, em:&mut Emitter);
}

pub struct Emitter {
    gensym: usize
}

impl Emitter {
    pub fn new() -> Emitter {
        Emitter {
            gensym: 0
        }
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

fn maybe_export(exported: bool, name:&Id) -> String {
    if exported {
        format!("(export \"{}\")", name.name)
    } else {
        "".to_string()
    }
}

impl Wast for GlobalVar {
    fn gen(&self, em:&mut Emitter) {
        let ty = render_type(Some(self.ty));
        let ty = if self.mutable { format!("(mut {})", ty) } else { ty };
        if self.imported {
            em.wast(&format!("(import \"\" \"{}\" (global {}))", self.name.name, ty));
        } else {
            let export_clause = maybe_export(self.exported, &self.name);
            em.wast(&format!("(global ${} {} {} ", self.name.name, &export_clause, ty));
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
            let export_clause = maybe_export(self.exported, &self.name);
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
                let block_id = em.gensym;
                let loop_id = em.gensym + 1;
                em.gensym += 2;
                em.wast(&format!("(block $_blk_{} (loop $_loop_{}\n", block_id, loop_id));
                em.wast(&format!("(br_if $_blk_{} (i32.eqz ", block_id));
                test.gen(em);
                em.wast("))");
                body.gen(em);
                em.wast(&format!("(br {})))\n", loop_id));
            }
            Uxpr::Binop{op, lhs, rhs} => {
                em.wast(&format!("({}.{} ", render_type(self.ty), render_binop(*op)));
                lhs.gen(em);
                em.wast(" ");
                rhs.gen(em);
                em.wast(")");
            }
            Uxpr::Unop{op, e} => {
                match op {
                    Unop::Neg => {
                        em.wast(&format!("{}.xor ", render_type(self.ty)));
                        e.gen(em);
                        em.wast(&format!("({}.const -1)", render_type(self.ty)));
                        em.wast(")");
                    }
                    _ => {
                        em.wast(&format!("({}.{} ", render_type(self.ty), render_unop(*op)));
                        e.gen(em);
                        em.wast(")");
                    }
                }
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
        Binop::Add => "add".to_string(),
        Binop::Sub => "sub".to_string(),
        Binop::Mul => "mul".to_string(),
        Binop::Div => "div_s".to_string(),
        Binop::UDiv => "div_u".to_string(),
        Binop::Rem => "rem_s".to_string(),
        Binop::URem => "rem_u".to_string(),
        Binop::ShiftLeft => "shl".to_string(),
        Binop::ShiftRight => "shr_s".to_string(),
        Binop::UShiftRight => "shr_u".to_string(),
        Binop::BitAnd => "and".to_string(),
        Binop::BitOr => "or".to_string(),
        Binop::BitXor => "xor".to_string(),
        Binop::Less => "lt_s".to_string(),
        Binop::LessOrEqual => "le_s".to_string(),
        Binop::Greater => "gt_s".to_string(),
        Binop::GreaterOrEqual => "ge_s".to_string(),
        Binop::Equal => "eq".to_string(),
        Binop::NotEqual => "ne".to_string(),
        Binop::ULess => "lt_u".to_string(),
        Binop::ULessOrEqual => "le_u".to_string(),
        Binop::UGreater => "gt_u".to_string(),
        Binop::UGreaterOrEqual => "ge_u".to_string(),
        Binop::RotLeft => "rotl".to_string(),
        Binop::RotRight => "rotr".to_string(),
        Binop::Copysign => "copysign".to_string()
    }
}

fn render_unop(op:Unop) -> String {
    match op {
        // TODO: more
        Unop::Not => "eqz".to_string(),
        _ => "???".to_string()
    }
}
