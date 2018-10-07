// -*- fill-column: 100 -*-
//
// Remove high-level syntax and operations that are not defined on the target
// platform.  Currently:
//
// - `while` is rewritten as iterate+break
// - `loop` is rewritten as iterate
// - `x % y` is expanded to its computation if x and y are floats
// - `x as T` and `x is T` are rewritten in terms of a fallible narrowing
//   operation + explicit traps when required
// - (not-equal x y) is rewritten as (eqz (equal x y)) if x and y are pointers
// - (bitnot x) is rewritten as (xor x -1)
// - (neg x) is rewritten as (- 0 x)
// - (not x) is rewritten as (eqz x)
// - calls to intrinsics are rewritten as intrinsic ops (some of them have
//   direct mappings, some require expansion)
// - `new` is rewritten with initializers in struct declaration order
// - `null` is rewritten to carry the type that the null pointer needs to have
//
// The desugarer can insert new blocks and variable bindings, it just can't leave behind new
// instances of any of the forms it is trying to remove.
//
// The line between desugaring and flattening is a little blurry, but flattening should not
// introduce new names, so anything that needs temps gets put into desugaring.

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
        self.desugar_expr(&mut g.init, Some(g.ty));
    }

    fn desugar_function(&mut self, f:&mut FunctionDef) {
        if !f.imported {
            self.env.locals.push_rib();
            (&f.formals).into_iter().for_each(|(name,ty)| self.env.locals.add_param(*name, *ty));
            self.desugar_block(&mut f.body, f.retn);
            self.env.locals.pop_rib();
        }
    }

    fn desugar_block(&mut self, b:&mut Block, ctx_ty:Option<Type>) {
        self.env.locals.push_rib();
        for item in &mut b.items {
            match item {
                BlockItem::Let(l) => {
                    let ty = l.ty;
                    self.desugar_expr(&mut l.init, Some(ty));
                    self.env.locals.add_local(l.name, ty);
                }
                BlockItem::Expr(e) => {
                    // Not actually right to pass ctx_ty here except for the last expr, but not
                    // currently harmful.
                    self.desugar_expr(e, ctx_ty);
                }
            }
        }
        self.env.locals.pop_rib();
    }

    fn desugar_expr(&mut self, expr:&mut Expr, ctx_ty:Option<Type>) {
        let mut replacement_expr = None;
        match &mut expr.u {
            Uxpr::Void => { }
            Uxpr::NumLit{..} => { }
            Uxpr::NullLit{ty} => {
                assert!(is_same_type(Some(*ty), Some(Type::NullRef)));
                match ctx_ty {
                    None => { }
                    Some(t @ Type::NullRef) => {  *ty = t; }
                    Some(t @ Type::Cooked(_)) => { *ty = t; }
                    _ => {
                        println!("{:?}", ctx_ty);
                        unreachable!();
                    }
                }
            }
            Uxpr::If{test, consequent, alternate} => {
                self.desugar_expr(test, Some(Type::I32));
                self.desugar_block(consequent, expr.ty);
                self.desugar_block(alternate, expr.ty);
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
                                        box_block(vec![box_break(break_label)]));

                new_body.items.insert(0, BlockItem::Expr(cond_break));
                let mut new_expr = box_iterate(break_label, continue_label, new_body);
                self.desugar_expr(&mut new_expr, expr.ty);
                replacement_expr = Some(new_expr);
            }
            Uxpr::Loop{body, break_label} => {
                let mut new_body = box_block(vec![]);
                swap(body, &mut new_body);

                let continue_label = Id::gensym("continue");
                let mut new_expr = box_iterate(*break_label, continue_label, new_body);
                self.desugar_expr(&mut new_expr, expr.ty);
                replacement_expr = Some(new_expr);
            }
            Uxpr::Iterate{body, break_label, continue_label} => {
                self.env.locals.add_label(*break_label);
                self.env.locals.add_label(*continue_label);
                self.desugar_block(body, expr.ty);
            }
            Uxpr::Break{..} => { }
            Uxpr::Binop{op, lhs, rhs} => {
                let new_ty = merge_compatible_types(lhs.ty, rhs.ty);
                self.desugar_expr(lhs, new_ty);
                self.desugar_expr(rhs, new_ty);
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
                        block_items.push(BlockItem::Let(box_let(tmp_x, ty.unwrap(), new_lhs)));
                        block_items.push(BlockItem::Let(box_let(tmp_y, ty.unwrap(), new_rhs)));
                        let div = box_binop(ty, Binop::Div, box_id(ty, tmp_x), box_id(ty, tmp_y));
                        let trunc = box_unop(ty, Unop::Trunc, div);
                        let mul = box_binop(ty, Binop::Mul, trunc, box_id(ty, tmp_y));
                        let diff = box_binop(ty, Binop::Sub, box_id(ty, tmp_x), mul);
                        block_items.push(BlockItem::Expr(diff));
                        replacement_expr = Some(box_block_expr(ty, block_items));
                    }
                    _ => { }
                }
            }
            Uxpr::Unop{op, opd} => {
                self.desugar_expr(opd, expr.ty);
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
            Uxpr::Typeop{op, lhs, rhs} => {
                self.desugar_expr(lhs, expr.ty);

                let mut new_lhs = box_void();
                swap(lhs, &mut new_lhs);

                let rhs = *rhs;

                match (new_lhs.ty, &rhs) {
                    (Some(Type::Cooked(_)), Type::Cooked(_)) |
                    (_, Type::AnyRef) => {
                        match op {
                            Typeop::Is => {
                                replacement_expr = Some(
                                    box_block_expr(Some(Type::I32),
                                                   vec![BlockItem::Expr(new_lhs),
                                                        BlockItem::Expr(box_intlit(1, Type::I32))]));
                            }
                            Typeop::As => {
                                replacement_expr = Some(new_lhs);
                            }
                        }
                    }
                    (Some(Type::AnyRef), Type::Cooked(_)) => {
                        let narrow = box_downcast(expr.ty, rhs, new_lhs);
                        match op {
                            Typeop::Is => {
                                replacement_expr = Some(box_unop(Some(Type::I32), Unop::IsNull, narrow));
                            }
                            Typeop::As => {
                                let tmp_name = Id::gensym("tmp");
                                let test = box_unop(Some(Type::I32), Unop::IsNull, box_id(expr.ty, tmp_name));
                                let consequent = box_block(vec![box_downcast_failed()]);
                                let alternate = box_block(vec![]);
                                let guard = box_if(test, consequent, alternate);
                                replacement_expr = Some(
                                    box_block_expr(expr.ty,
                                                   vec![BlockItem::Let(box_let(tmp_name, expr.ty.unwrap(), narrow)),
                                                        BlockItem::Expr(guard),
                                                        BlockItem::Expr(box_id(expr.ty, tmp_name))]));
                            }
                        }
                    }
                    _ => {
                        unreachable!();
                    }
                }
                
            }
            Uxpr::Call{name, actuals} => {
                for actual in &mut *actuals {
                    let ty = actual.ty;
                    self.desugar_expr(actual, ty);
                }
                if let Binding::Intrinsic(sigs, op) = self.env.lookup(*name).unwrap() {
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
            Uxpr::Id{..} => { }
            Uxpr::Deref{base, ..} => {
                self.desugar_expr(base, expr.ty);
            }
            Uxpr::Aref{..} => {
                panic!("NYI");
            }
            Uxpr::New{ty_name, values} => {
                // Reorganize the values so that they are in the right order for eventual structure
                // construction (and so the names in the initializer list can be ignored).  Preserve
                // the order of side effects.

                let mut block_items = vec![];
                let mut initializers = vec![];

                for (field, mut value) in values.drain(0..) {
                    let (_,fields) = &*self.env.get_struct_def(*ty_name);
                    if let Some((_,field_ty)) = fields.into_iter().find(|(name,_)| *name == field) {
                        self.desugar_expr(&mut value, Some(*field_ty));
                        let tmp_name = Id::gensym("tmp");
                        block_items.push(BlockItem::Let(box_let(tmp_name, *field_ty, value)));
                        initializers.push((field, box_id(Some(*field_ty), tmp_name)));
                    } else {
                        unreachable!();
                    }
                }

                let mut indices = HashMap::new();
                match self.env.lookup(*ty_name) {
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

                block_items.push(BlockItem::Expr(box_new(expr.ty, *ty_name, initializers)));
                replacement_expr = Some(box_block_expr(expr.ty, block_items));
            }
            Uxpr::Assign{lhs, rhs} => {
                match lhs {
                    LValue::Id{ty, ..} => {
                        self.desugar_expr(rhs, *ty);
                    }
                    LValue::Field{ty, base, ..} => {
                        self.desugar_expr(rhs, *ty);
                        let base_ty = base.ty;
                        self.desugar_expr(base, base_ty);
                    }
                    LValue::Element{..} => {
                        panic!("NYI");
                    }
                }
            }
            Uxpr::NewArray{..} => {
                panic!("NYI");
            }
            Uxpr::Block{..} | Uxpr::Sequence{..} | Uxpr::Drop{..} |
            Uxpr::ExactFallibleUnboxAnyRef{..} | Uxpr::DowncastFailed |
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
