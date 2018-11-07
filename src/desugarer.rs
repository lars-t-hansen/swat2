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
// - In progress: desugaring array operations and array types
//   - generate placeholder code
//   - rewrite array operations to use ops on placeholder types
//   - rewrite array types to reference placeholder types
//
// The desugarer can insert new blocks and variable bindings, it just can't leave behind new
// instances of any of the forms it is trying to remove.
//
// The line between desugaring and flattening is a little blurry, but flattening should not
// introduce new names, so anything that needs temps gets put into desugaring.

use ast::*;
use context::Context;
use environment::*;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::mem::swap;

pub fn desugar(ctx:&mut Context, m:&mut Module) {
    let mut de = Desugarer::new(ctx);
    de.desugar_module(m);
}

struct Desugarer<'a> {
    // The carried Type value is not used here, we use environments simply to discover whether
    // something is an intrinsic or not.
    env: Env<Type>,
    ctx: &'a mut Context
}

impl<'a> Desugarer<'a>
{
    fn new(ctx:&'a mut Context) -> Desugarer<'a> {
        Desugarer {
            env: Env::new(),
            ctx: ctx
        }
    }

    fn desugar_module(&mut self, m:&mut Module) {
        self.synthesize_types(m);
        (&m.items).into_iter().for_each(|item| self.env.define_toplevel(item));
        for item in &mut m.items {
            match item {
                ModItem::Var(g)    => { self.desugar_global(g); }
                ModItem::Fn(f)     => { self.desugar_function(f); }
                ModItem::Struct(s) => { self.desugar_struct(s); }
            }
        }
    }

    fn desugar_global(&mut self, g:&mut GlobalDef) {
        self.desugar_type(&mut g.ty);
        self.desugar_expr(&mut g.init, Some(g.ty));
    }

    fn desugar_struct(&mut self, s:&mut StructDef) {
        (&mut s.fields).into_iter().for_each(|(_,ty)| self.desugar_type(ty));
    }

    fn desugar_function(&mut self, f:&mut FunctionDef) {
        (&mut f.formals).into_iter().for_each(|(_,ty)| self.desugar_type(ty));
        self.desugar_maybe_type(&mut f.retn);
        if !f.imported {
            self.env.locals.push_rib();
            (&f.formals).into_iter().for_each(|(name,ty)| self.env.locals.add_param(*name, *ty));
            self.desugar_block(&mut f.body, f.retn);
            self.env.locals.pop_rib();
        }
    }

    // Generate support code for arrays into m.items, one for each array type.  The support code is
    // desugared subsequently and only needs to be well-typed.
    //
    // Generating code this way doesn't scale.  If we were to do code generation routinely, or with
    // more sophistication, we would handle it systematically in a pass before desugaring with more
    // infrastructure and maybe an extra type checking pass.  But this code is going to go away once
    // we have proper array types in the VM.

    fn synthesize_types(&mut self, m:&mut Module) {
        let val_name = Id::intern("val");
        let next_name = Id::intern("next");
        let length_name = Id::intern("length");
        let len_name = Id::intern("len");
        let mem_name = Id::intern("mem");
        let a_name = Id::intern("n");
        let n_name = Id::intern("n");
        
        self.ctx.iter_base_types().for_each(|base_type: &Type| {
            let tag = mangled_array_name(*base_type);
            let array_name = Id::intern(&format!("{}_Array", &tag));
            let arrayref_type = Type::Cooked(Ref::Struct(array_name));
            let node_name = Id::intern(&format!("{}_Node", &tag));
            let noderef_type = Type::Cooked(Ref::Struct(node_name));
            let constructor_name = Id::intern(&format!("{}_make", &tag));
            let getter_name = Id::intern(&format!("{}_get", &tag));
            let setter_name = Id::intern(&format!("{}_set", &tag));

            // struct _XXX_Array {
            //   length: i32,
            //   mem:    _XXX_Node
            // }
            m.items.push(ModItem::Struct(Box::new(StructDef{
                name:   array_name,
                fields: vec![(length_name, Type::I32),
                             (mem_name,    noderef_type)]
            })));

            // struct _XXX_Node {
            //   val:  T,
            //   next: _XXX_Node
            // }
            m.items.push(ModItem::Struct(Box::new(StructDef{
                name:   node_name,
                fields: vec![(val_name,  *base_type),
                             (next_name, noderef_type)]
            })));

            // fn _XXX_make(len: i32) -> _XXX_Array {
            //    let mem: _XXX_Node = null;
            //    let n: i32 = len;
            //    while n > 0 {
            //      mem = new _XXX_Node { val: <default for base_type>, next: mem };
            //      n = n - 1;
            //    }
            //    new _XXX_Array { length: len, mem: mem }
            // }
            m.items.push(ModItem::Fn(Box::new(FunctionDef{
                imported: false,
                exported: false,
                name: constructor_name,
                formals: vec![(len_name, Type::I32)],
                retn: Some(arrayref_type),
                locals: Some(vec![(mem_name, noderef_type),
                                  (n_name,   Type::I32)]),
                body: Box::new(Block{
                    ty: Some(noderef_type),
                    items: vec![
                        BlockItem::Let(
                            box_let(mem_name, noderef_type,
                                    // box_null is slightly wrong here, probably a bad API
                                    Box::new(Expr{ ty: Some(Type::AnyRef), u: Uxpr::NullLit{ ty: Type::NullRef } }))),
                        BlockItem::Let(
                            box_let(n_name, Type::I32, box_id(Some(Type::I32), len_name))),
                        BlockItem::Expr(
                            box_while(box_binop(Some(Type::I32),
                                                Binop::Greater,
                                                box_id(Some(Type::I32), n_name),
                                                box_intlit(0, Type::I32)),
                                      box_block(vec![box_assign(LValue::Id{ty:Some(noderef_type), name:mem_name},
                                                                box_new(Some(noderef_type),
                                                                        node_name,
                                                                        vec![(val_name, default_value(*base_type)),
                                                                             (next_name, box_id(Some(noderef_type), mem_name))])),
                                                     box_assign(LValue::Id{ty:Some(Type::I32), name:n_name},
                                                                box_binop(Some(Type::I32),
                                                                          Binop::Sub,
                                                                          box_id(Some(Type::I32), n_name),
                                                                          box_intlit(1, Type::I32)))]))),
                        BlockItem::Expr(
                            box_new(Some(arrayref_type),
                                    array_name,
                                    vec![(length_name, box_id(Some(Type::I32), len_name)),
                                         (mem_name, box_id(Some(noderef_type), mem_name))]))]
                })
            })));

            // fn _XXX_get(a: _XXX_Array, n: i32) -> base_type {
            //   let mem: _XXX_Node = a.mem;
            //   while n > 0 {
            //     mem = mem.next;
            //     n = n - 1;
            //   }
            //   mem.val
            // }
            m.items.push(ModItem::Fn(Box::new(FunctionDef{
                imported: false,
                exported: false,
                name: getter_name,
                formals: vec![(a_name, arrayref_type),
                              (n_name, Type::I32)],
                retn: Some(*base_type),
                locals: Some(vec![(mem_name, noderef_type)]),
                body: Box::new(Block{
                    ty: Some(*base_type),
                    items: vec![
                        BlockItem::Let(
                            box_let(mem_name, noderef_type,
                                    box_deref(Some(noderef_type),
                                              box_id(Some(arrayref_type), a_name),
                                              mem_name))),
                        BlockItem::Expr(
                            box_while(box_binop(Some(Type::I32),
                                                Binop::Greater,
                                                box_id(Some(Type::I32), n_name),
                                                box_intlit(0, Type::I32)),
                                      box_block(vec![box_assign(LValue::Id{ty:Some(noderef_type), name:mem_name},
                                                                box_deref(Some(noderef_type),
                                                                          box_id(Some(noderef_type), mem_name),
                                                                          next_name)),
                                                     box_assign(LValue::Id{ty:Some(Type::I32), name:n_name},
                                                                box_binop(Some(Type::I32),
                                                                          Binop::Sub,
                                                                          box_id(Some(Type::I32), n_name),
                                                                          box_intlit(1, Type::I32)))]))),
                        BlockItem::Expr(
                            box_deref(Some(*base_type),
                                      box_id(Some(noderef_type), mem_name),
                                      val_name))]
                })
            })));

            // fn _XXX_set(a: _XXX_Array, n: i32, val: base_type) {
            //   let mem: _XXX_Node = a.mem;
            //   while n > 0 {
            //     mem = mem.next;
            //     n = n - 1;
            //   }
            //   mem.val = val;
            // }
            m.items.push(ModItem::Fn(Box::new(FunctionDef{
                imported: false,
                exported: false,
                name: setter_name,
                formals: vec![(a_name, arrayref_type),
                              (n_name, Type::I32),
                              (val_name, *base_type)],
                retn: Some(*base_type),
                locals: Some(vec![(mem_name, noderef_type)]),
                body: Box::new(Block{
                    ty: Some(*base_type),
                    items: vec![
                        BlockItem::Let(
                            box_let(mem_name, noderef_type,
                                    box_deref(Some(noderef_type),
                                              box_id(Some(arrayref_type), a_name),
                                              mem_name))),
                        BlockItem::Expr(
                            box_while(box_binop(Some(Type::I32),
                                                Binop::Greater,
                                                box_id(Some(Type::I32), n_name),
                                                box_intlit(0, Type::I32)),
                                      box_block(vec![box_assign(LValue::Id{ty:Some(noderef_type), name:mem_name},
                                                                box_deref(Some(noderef_type),
                                                                          box_id(Some(noderef_type), mem_name),
                                                                          next_name)),
                                                     box_assign(LValue::Id{ty:Some(Type::I32), name:n_name},
                                                                box_binop(Some(Type::I32),
                                                                          Binop::Sub,
                                                                          box_id(Some(Type::I32), n_name),
                                                                          box_intlit(1, Type::I32)))]))),
                        BlockItem::Expr(
                            box_assign(LValue::Field{ty:    Some(*base_type),
                                                     base:  box_id(Some(noderef_type), mem_name),
                                                     field: val_name},
                                       box_id(Some(*base_type), val_name)))]
                })
            })));
        });
    }

    fn desugar_type(&mut self, t:&mut Type) {
        *t =
            if let Type::Cooked(Ref::Array(ad)) = t {
                Type::Cooked(Ref::Struct(Id::intern(&format!("{}_Array", mangled_array_name(ArrayDef::find_raw(*ad))))))
            } else {
                *t
            };
    }

    fn desugar_maybe_type(&mut self, t:&mut Option<Type>) {
        if let Some(ty) = t {
            self.desugar_type(ty);
        }
    }

    fn desugar_block(&mut self, b:&mut Block, ctx_ty:Option<Type>) {
        self.env.locals.push_rib();
        for item in &mut b.items {
            match item {
                BlockItem::Let(l) => {
                    self.desugar_type(&mut l.ty);
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
        self.desugar_type(&mut expr.ty);
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
                let mut new_ty = merge_compatible_types(lhs.ty, rhs.ty);
                self.desugar_type(&mut new_ty);
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

                // panic!("NYI"): Array types
                //
                // Will this not simply "just work" if types are properly translated?  The object
                // will then be of the correct array type, as expected.

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
                    let mut ty = actual.ty;
                    self.desugar_type(&mut ty);
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
                // (aref base index) => (call _TAG_aref base index) where TAG depends on
                // the specific array type.
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
                        // TODO: desugar type?
                        self.desugar_expr(rhs, *ty);
                    }
                    LValue::Field{ty, base, ..} => {
                        // TODO: desugar type?
                        self.desugar_expr(rhs, *ty);
                        let base_ty = base.ty;
                        self.desugar_expr(base, base_ty);
                    }
                    LValue::Element{..} => {
                        // TODO: desugar type?
                        // (setf (aref base index) value) => (call _TAG_aset base index value) where TAG depends on
                        // the specific array type.
                        panic!("NYI");
                    }
                }
            }
            Uxpr::NewArray{ty, values} => {
                self.desugar_type(ty);
                for value in &mut *values {
                    let ty = value.ty;
                    self.desugar_expr(value, ty);
                }
                // (new-array type length) => (call _TAG_new_array length) where TAG depends on
                // the specific array type.
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

// Here `ty` is the base type of the first level of the array.  We create
// a prefix encoding that bottoms out at non-arrays.

fn mangled_array_name(mut ty:Type) -> String {
    let mut s = "_A".to_string();
    loop {
        ty = match ty {
            Type::I32 => {
                return s + "I";
            }
            Type::I64 => {
                return s + "L";
            }
            Type::F32 => {
                return s + "F";
            }
            Type::F64 => {
                return s + "D";
            }
            Type::Cooked(Ref::Struct(name)) => {
                return s + "S" + &name.name();
            }
            Type::Cooked(Ref::Array(ad)) => {
                s = s + "A";
                ArrayDef::find_raw(ad)
            }
            _ => {
                panic!("Bad type");
            }
        }
    }
}

fn default_value(ty:Type) -> Box<Expr> {
    match ty {
        Type::I32 | Type::I64 | Type::F32 | Type::F64 => box_intlit(0, ty),
        Type::Cooked(_) => box_null(Type::AnyRef),
        _ => { panic!("Bad type") }
    }
}
