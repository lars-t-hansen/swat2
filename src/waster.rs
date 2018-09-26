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
                ModItem::Var(v)    => { self.wast_global(&v); }
                ModItem::Fn(f)     => { self.wast_function(&f); }
                ModItem::Struct(s) => { panic!("NYI"); }
            }
        }
        self.emit(")\n");
    }

    fn wast_global(&mut self, g:&GlobalDef) {
        let ty = render_type(Some(g.ty));
        let ty = if g.mutable { format!("(mut {})", ty) } else { ty.to_string() };
        if g.imported {
            self.emit(&format!("(import \"\" \"{}\" (global {}))", &g.name, ty));
        } else {
            let export_clause = maybe_export(g.exported, &g.name);
            self.emit(&format!("(global ${} {} {} ", &g.name, &export_clause, ty));
            self.wast_expr(&g.init);
            self.emit(")\n");
        }
    }

    fn wast_function(&mut self, f:&FunctionDef) {
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
                self.emit(&format!("({}.{} ",
                                   render_op_type(e.ty),
                                   render_binop(*op, e.ty.unwrap())));
                self.wast_expr(&lhs);
                self.emit(" ");
                self.wast_expr(&rhs);
                self.emit(")");
            }
            Uxpr::Unop{op, opd} => {
                self.emit(&format!("({}.{} ", &render_op_type(opd.ty), render_unop(*op)));
                self.wast_expr(&opd);
                self.emit(")");
            }
            Uxpr::Typeop{..} => {
                panic!("NYI");
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
                // FIXME: Not adequate for structs
                self.emit("(ref.null anyref)");
            }
            Uxpr::Drop(e) => {
                self.emit("(drop ");
                self.wast_expr(&e);
                self.emit(")\n");
            }
            Uxpr::GetLocal{name} => {
                self.emit(&format!("(get_local ${})", &name));
            }
            Uxpr::GetGlobal{name} => {
                self.emit(&format!("(get_global ${})", &name));
            }
            Uxpr::SetLocal{name, value} => {
                self.emit(&format!("(set_local ${} ", &name));
                self.wast_expr(&value);
                self.emit(")");
            }
            Uxpr::SetGlobal{name, value} => {
                self.emit(&format!("(set_global ${} ", &name));
                self.wast_expr(&value);
                self.emit(")");
            }
            Uxpr::GetField{..} => {
                panic!("NYI");
            }
            Uxpr::SetField{..} => {
                panic!("NYI");
            }
            Uxpr::New{ty_name, values} => {
                panic!("NYI");
            }
            Uxpr::Void | Uxpr::While{..} | Uxpr::Loop{..} | Uxpr::Block(_) |
            Uxpr::Assign{..} | Uxpr::Id(_) | Uxpr::Deref{..} =>
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
        Some(Type::CookedRef(id)) => format!("(ref ${})", id),
        Some(Type::RawRef(_)) => unreachable!(),
        None => "".to_string()
    }
}

fn render_op_type(ty:Option<Type>) -> &'static str {
    match ty {
        Some(Type::I32) => "i32",
        Some(Type::I64) => "i64",
        Some(Type::F32) => "f32",
        Some(Type::F64) => "f64",
        Some(Type::AnyRef) => "ref",
        Some(Type::CookedRef(_)) => "ref",
        Some(Type::RawRef(_)) => unreachable!(),
        None => unreachable!()
    }
}

fn render_binop(op:Binop, ty:Type) -> &'static str {
    match op {
        Binop::Add => "add",
        Binop::Sub => "sub",
        Binop::Mul => "mul",
        Binop::Div => match ty {
            Type::I32 | Type::I64 => "div_s",
            Type::F32 | Type::F64 => "div",
            _ => unreachable!()
        },
        Binop::UDiv => "div_u",
        Binop::Rem => "rem_s",
        Binop::URem => "rem_u",
        Binop::ShiftLeft => "shl",
        Binop::ShiftRight => "shr_s",
        Binop::UShiftRight => "shr_u",
        Binop::BitAnd => "and",
        Binop::BitOr => "or",
        Binop::BitXor => "xor",
        Binop::Less => match ty {
            Type::I32 | Type::I64 => "lt_s",
            Type::F32 | Type::F64 => "lt",
            _ => unreachable!()
        },
        Binop::LessOrEqual => match ty {
            Type::I32 | Type::I64 => "le_s",
            Type::F32 | Type::F64 => "le",
            _ => unreachable!()
        },
        Binop::Greater => match ty {
            Type::I32 | Type::I64 => "gt_s",
            Type::F32 | Type::F64 => "gt",
            _ => unreachable!()
        },
        Binop::GreaterOrEqual => match ty {
            Type::I32 | Type::I64 => "ge_s",
            Type::F32 | Type::F64 => "ge",
            _ => unreachable!()
        },
        Binop::Equal => "eq",
        Binop::NotEqual => "ne",
        Binop::ULess => "lt_u",
        Binop::ULessOrEqual => "le_u",
        Binop::UGreater => "gt_u",
        Binop::UGreaterOrEqual => "ge_u",
        Binop::RotLeft => "rotl",
        Binop::RotRight => "rotr",
        Binop::Copysign => "copysign"
    }
}

fn render_unop(op:Unop) -> &'static str {
    match op {
        Unop::Clz => "clz",
        Unop::Ctz => "ctz",
        Unop::Popcnt => "popcnt",
        Unop::Extend8 => "extend8_s",
        Unop::Extend16 => "extend16_s",
        Unop::Extend32 => "extend32_s",
        Unop::Sqrt => "sqrt",
        Unop::Ceil => "ceil",
        Unop::Floor => "floor",
        Unop::Nearest => "nearest",
        Unop::Trunc => "trunc",
        Unop::Eqz => "eqz",
        Unop::I32ToI64 => "extend_s/i32",
        Unop::U32ToI64 => "extend_u/i32",
        Unop::I64ToI32 => "wrap/i64",
        Unop::Neg | Unop::Not | Unop::BitNot => {
            unreachable!()
        }
    }
}
