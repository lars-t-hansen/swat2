// -*- fill-column: 80 -*-
//
// Generate SpiderMonkey wast code.
//
// We operate on the flattened form and use symbolic names everywhere,
// hence this pretty much just formats the information that is in the
// module.

use ast::*;
use std::fs::File;
use std::io::Write;

pub fn wast(m:&Module, out:&mut File) {
    let mut w = Waster { out };
    w.wast_module(m);
}

struct Waster<'a> {
    out: &'a mut File
}

impl<'a> Waster<'a>
{
    fn wast_module(&mut self, m:&Module) {
        self.emit("(module\n");
        for item in &m.items {
            match item {
                ModItem::Var(v) => { self.wast_global(&v); }
                ModItem::Fn(f)  => { self.wast_function(&f); }
            }
        }
        self.emit(")\n");
    }

    fn wast_global(&mut self, g:&GlobalVar) {
        let ty = render_type(Some(g.ty));
        let ty = if g.mutable { format!("(mut {})", ty) } else { ty };
        if g.imported {
            self.emit(&format!("(import \"\" \"{}\" (global {}))", &g.name, ty));
        } else {
            let export_clause = maybe_export(g.exported, &g.name);
            self.emit(&format!("(global ${} {} {} ", &g.name, &export_clause, ty));
            self.wast_expr(&g.init);
            self.emit(")\n");
        }
    }

    fn wast_function(&mut self, f:&FnDef) {
        let mut params = "".to_string();
        for (name, ty) in &f.formals {
            params.push_str(&format!("(param ${} {}) ", name, render_type(Some(*ty))));
        }
        let result =
            if let Some(ty) = f.retn {
                format!("(result {})", render_type(Some(ty)))
            } else {
                "".to_string()
            };
        if f.imported {
            self.emit(&format!("(import \"\" \"{}\" (func {} {}))\n",
                               &f.name,
                               &params,
                               &result));
        } else {
            let export_clause = maybe_export(f.exported, &f.name);
            self.emit(&format!("(func ${} {} {} {}\n", &f.name, &export_clause, params, result));
            if let Some(locals) = &f.locals {
                for (name, ty) in locals {
                    self.emit(&format!("(local ${} {}) ", name, render_type(Some(*ty))));
                }
                if locals.len() > 0 {
                    self.emit("\n");
                }
            }
            self.wast_function_body(&f.body);
            self.emit(")\n");
        }
    }

    // At the function body level, we do not need the "block" wrapper we get
    // from the standard tree shape, so we do something special here.

    fn wast_function_body(&mut self, b:&Block) {
        assert!(b.items.len() == 1);
        if let BlockItem::Expr(e) = &b.items[0] {
            // I fought the borrow checker and the borrow checker won.
            if let &Expr{u:Uxpr::Sequence{ref body, ..}, ..} = &**e {
                for expr in body {
                    self.wast_expr(expr);
                }
            } else {
                self.wast_expr(&e);
            }
        } else {
            unreachable!();
        }
    }

    fn wast_block(&mut self, b:&Block) {
        assert!(b.items.len() == 1);
        if let BlockItem::Expr(e) = &b.items[0] {
            self.wast_expr(&e);
        } else {
            unreachable!();
        }
    }

    fn wast_expr(&mut self, e:&Expr) {
        match &e.u {
            Uxpr::Sequence{ty, body} => {
                self.emit(&format!("(block {} ", render_type(*ty)));
                for expr in body {
                    self.wast_expr(expr);
                }
                self.emit(")\n");
            }
            Uxpr::If{test, consequent, alternate} => {
                self.emit(&format!("(if {} ", render_type(e.ty)));
                self.wast_expr(&test);
                self.emit(" ");
                self.wast_block(&consequent);
                self.emit(" ");
                self.wast_block(&alternate);
                self.emit(")");
            }
            Uxpr::Iterate{break_label, continue_label, body} => {
                self.emit(&format!("(block ${} (loop ${}\n", &break_label, &continue_label));
                self.wast_block(&body);
                self.emit(&format!("(br ${})))\n", continue_label));
            }
            Uxpr::Break{label} => {
                self.emit(&format!("(br ${})", &label));
            }
            Uxpr::Binop{op, lhs, rhs} => {
                self.emit(&format!("({}.{} ", render_op_type(e.ty), render_binop(*op, e.ty.unwrap())));
                self.wast_expr(&lhs);
                self.emit(" ");
                self.wast_expr(&rhs);
                self.emit(")");
            }
            Uxpr::Unop{op, e} => {
                self.emit(&format!("({}.{} ", &render_op_type(e.ty), render_unop(*op)));
                self.wast_expr(&e);
                self.emit(")");
            }
            Uxpr::Call{name, actuals} => {
                self.emit(&format!("(call ${} ", &name));
                for arg in actuals {
                    self.wast_expr(arg);
                    self.emit(" ");
                }
                self.emit(")");
            }
            Uxpr::NumLit(n) => {
                match n {
                    Number::I32(k) => { self.emit(&format!("(i32.const {})", k)) }
                    Number::I64(k) => { self.emit(&format!("(i64.const {})", k)) }
                    Number::F32(k) => { self.emit(&format!("(f32.const {})", k)) }
                    Number::F64(k) => { self.emit(&format!("(f64.const {})", k)) }
                }
            }
            Uxpr::NullLit => {
                self.emit("(ref.null anyref)");
            }
            Uxpr::Drop(e) => {
                self.emit("(drop ");
                self.wast_expr(&e);
                self.emit(")\n");
            }
            Uxpr::Local(id) => {
                self.emit(&format!("(get_local ${})", &id));
            }
            Uxpr::Global(id) => {
                self.emit(&format!("(get_global ${})", &id));
            }
            Uxpr::SetLocal{name, e} => {
                self.emit(&format!("(set_local ${} ", &name));
                self.wast_expr(&e);
                self.emit(")");
            }
            Uxpr::SetGlobal{name, e} => {
                self.emit(&format!("(set_global ${} ", &name));
                self.wast_expr(&e);
                self.emit(")");
            }
            Uxpr::Void | Uxpr::While{..} | Uxpr::Loop{..} | Uxpr::Block(_) |
            Uxpr::Assign{..} | Uxpr::Id(_) =>
            {
                unreachable!();
            }
        }
    }

    fn emit(&mut self, s:&str) {
        self.out.write(s.as_bytes()).unwrap();
    }
}

fn maybe_export(exported: bool, name:&Id) -> String {
    if exported {
        format!("(export \"{}\")", name)
    } else {
        "".to_string()
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

fn render_op_type(ty:Option<Type>) -> String {
    match ty {
        Some(Type::I32) => "i32".to_string(),
        Some(Type::I64) => "i64".to_string(),
        Some(Type::F32) => "f32".to_string(),
        Some(Type::F64) => "f64".to_string(),
        Some(Type::AnyRef) => "ref".to_string(),
        None => unreachable!()
    }
}

fn render_binop(op:Binop, ty:Type) -> String {
    match op {
        Binop::Add => "add".to_string(),
        Binop::Sub => "sub".to_string(),
        Binop::Mul => "mul".to_string(),
        Binop::Div => match ty {
            Type::I32 | Type::I64 => "div_s".to_string(),
            Type::F32 | Type::F64 => "div".to_string(),
            _ => unreachable!()
        },
        Binop::UDiv => "div_u".to_string(),
        Binop::Rem => "rem_s".to_string(),
        Binop::URem => "rem_u".to_string(),
        Binop::ShiftLeft => "shl".to_string(),
        Binop::ShiftRight => "shr_s".to_string(),
        Binop::UShiftRight => "shr_u".to_string(),
        Binop::BitAnd => "and".to_string(),
        Binop::BitOr => "or".to_string(),
        Binop::BitXor => "xor".to_string(),
        Binop::Less => match ty {
            Type::I32 | Type::I64 => "lt_s".to_string(),
            Type::F32 | Type::F64 => "lt".to_string(),
            _ => unreachable!()
        },
        Binop::LessOrEqual => match ty {
            Type::I32 | Type::I64 => "le_s".to_string(),
            Type::F32 | Type::F64 => "le".to_string(),
            _ => unreachable!()
        },
        Binop::Greater => match ty {
            Type::I32 | Type::I64 => "gt_s".to_string(),
            Type::F32 | Type::F64 => "gt".to_string(),
            _ => unreachable!()
        },
        Binop::GreaterOrEqual => match ty {
            Type::I32 | Type::I64 => "ge_s".to_string(),
            Type::F32 | Type::F64 => "ge".to_string(),
            _ => unreachable!()
        },
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
        Unop::Clz => "clz".to_string(),
        Unop::Ctz => "ctz".to_string(),
        Unop::Popcnt => "popcnt".to_string(),
        Unop::Extend8 => "extend8_s".to_string(),
        Unop::Extend16 => "extend16_s".to_string(),
        Unop::Extend32 => "extend32_s".to_string(),
        Unop::Sqrt => "sqrt".to_string(),
        Unop::Ceil => "ceil".to_string(),
        Unop::Floor => "floor".to_string(),
        Unop::Nearest => "nearest".to_string(),
        Unop::Trunc => "trunc".to_string(),
        Unop::Eqz => "eqz".to_string(),
        Unop::I32ToI64 => "extend_s/i32".to_string(),
        Unop::Neg | Unop::Not | Unop::BitNot => {
            unreachable!()
        }
    }
}
