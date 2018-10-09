// -*- fill-column: 100 -*-

use ast::*;
use lexer::{Lexer,Tok};

pub fn parse<'a>(l:&'a mut Lexer<'a>) -> Program {
    let mut parser = Parser::new(l);
    parser.parse_program()
}

struct Parser<'a> {
    lexer: &'a mut Lexer<'a>,
    cached: Tok
}

impl<'a> Parser<'a>
{
    fn new(lexer: &'a mut Lexer<'a>) -> Parser<'a> {
        let cached = lexer.lex();
        Parser {
            lexer,
            cached
        }
    }

    fn parse_program(&mut self) -> Program {
        let mut items = vec![];
        while let Some(ti) = self.parse_top_item() {
            items.push(ti);
        }
        self.snarf(Tok::EOI);
        Program { items }
    }

    fn parse_top_item(&mut self) -> Option<TopItem> {
        if self.peek(Tok::Module) {
            Some(TopItem::Mod(self.parse_module()))
        } else {
            None
        }
    }

    fn parse_module(&mut self) -> Module {
        self.snarf(Tok::Module);
        let name = self.parse_id();
        self.snarf(Tok::LBrace);
        let mut items = vec![];
        while let Some(mi) = self.parse_mod_item() {
            items.push(mi);
        }
        self.snarf(Tok::RBrace);
        Module { name, items }
    }

    fn parse_mod_item(&mut self) -> Option<ModItem> {
        let is_extern = self.eat(Tok::Extern);
        let is_pub = self.eat(Tok::Pub);
        if is_extern && is_pub {
            self.error("Can't be both `extern` and `pub`");
        }
        if self.peek(Tok::Var) || self.peek(Tok::Const) {
            Some(ModItem::Var(self.parse_global(is_extern, is_pub)))
        } else if self.peek(Tok::Fn) {
            Some(ModItem::Fn(self.parse_fn(is_extern, is_pub)))
        } else if self.peek(Tok::Struct) {
            if is_extern || is_pub {
                self.error("Structs must be private to a module");
            }
            Some(ModItem::Struct(self.parse_struct()))
        } else {
            None
        }
    }

    fn parse_global(&mut self, is_extern:bool, is_pub:bool) -> Box<GlobalDef> {
        let is_var = self.eat(Tok::Var);
        let is_const = self.eat(Tok::Const);
        if is_var && is_const {
            self.error("Can't be both `var` and `const`");
        }
        let name = self.parse_id();
        self.snarf(Tok::Colon);
        let ty = self.parse_type();
        self.snarf(Tok::Assign);
        let init = self.parse_expr();
        self.snarf(Tok::Semi);
        Box::new(GlobalDef{mutable:is_var, exported:is_pub, imported:is_extern, name, ty, init})
    }

    fn parse_fn(&mut self, is_extern:bool, is_pub:bool) -> Box<FunctionDef> {
        self.snarf(Tok::Fn);
        let name = self.parse_id();
        let formals = self.parse_formals(Tok::LParen, Tok::RParen);
        let retn = if self.eat(Tok::Arrow) { Some(self.parse_type()) } else { None };
        let body = if is_extern {
            self.snarf(Tok::Semi);
            Box::new(Block { ty: None, items: vec![] })
        } else {
            self.parse_block()
        };
        Box::new(FunctionDef{exported:is_pub, imported:is_extern, name, formals, retn, body, locals: None})
    }

    fn parse_struct(&mut self) -> Box<StructDef> {
        self.snarf(Tok::Struct);
        let name = self.parse_id();
        let fields = self.parse_formals(Tok::LBrace, Tok::RBrace);
        Box::new(StructDef{ name, fields })
    }

    fn parse_type(&mut self) -> Type {
        if self.eat(Tok::I32) {
            Type::I32
        } else if self.eat(Tok::I64) {
            Type::I64
        } else if self.eat(Tok::F32) {
            Type::F32
        } else if self.eat(Tok::F64) {
            Type::F64
        } else if self.eat(Tok::AnyRef) {
            Type::AnyRef
        } else if let Some(name) = self.eat_id() {
            Type::Raw(Ref::Struct(name))
        } else if self.eat(Tok::LBracket) {
            let base_ty = self.parse_type();
            self.snarf(Tok::RBracket);
            Type::Raw(Ref::Array(ArrayDef::new_raw(base_ty))) // FIXME now that we can
        } else {
            self.error("Invalid type");
        }
    }

    fn parse_formals(&mut self, open:Tok, close:Tok) -> Vec<(Id,Type)> {
        self.parse_comma_separated(open, close,
                                   |p:&mut Parser| {
                                       let name = p.parse_id();
                                       p.snarf(Tok::Colon);
                                       let ty = p.parse_type();
                                       (name, ty)
                                   })
    }

    fn parse_exprs(&mut self, open:Tok, close:Tok) -> Vec<Box<Expr>> {
        self.parse_comma_separated(open, close, |p:&mut Parser| p.parse_expr())
    }

    fn parse_comma_separated<Itemer, T>(&mut self, open: Tok, close: Tok, itemer: Itemer) -> Vec<T>
        where Itemer: Fn(&mut Parser) -> T
    {
        let mut items = vec![];
        self.snarf(open);
        if !self.peek(close) {
            loop {
                items.push(itemer(self));
                if !self.eat(Tok::Comma) {
                    break;
                }
            }
        }
        self.snarf(close);
        items
    }

    // FIXME: break
    // FIXME: assignments should be valid only at this level

    fn parse_block(&mut self) -> Box<Block> {
        let mut items = vec![];
        let mut forbid_semi = true;
        let mut require_semi = false;
        let mut require_semi_if_not_last = false;
        self.snarf(Tok::LBrace);
        while !self.eat(Tok::RBrace) {
            if self.eat(Tok::Let) {
                if require_semi || require_semi_if_not_last {
                    self.error("Semicolon required here");
                }
                let name = self.parse_id();
                self.snarf(Tok::Colon);
                let ty = self.parse_type();
                self.snarf(Tok::Assign);
                let init = self.parse_expr();
                forbid_semi = false;
                require_semi = true;
                require_semi_if_not_last = true;
                items.push(BlockItem::Let(Box::new(LetDefn{ name, ty, init })));
            } else if self.eat(Tok::Semi) {
                if forbid_semi {
                    self.error("Semicolon not allowed here");
                }
                forbid_semi = true;
                require_semi = false;
                require_semi_if_not_last = false;
            } else {
                if require_semi || require_semi_if_not_last {
                    self.error("Semicolon required here");
                }
                let expr = self.parse_expr();
                forbid_semi = false;
                require_semi = false;
                require_semi_if_not_last = !ends_with_block(&expr);
                items.push(BlockItem::Expr(expr));
            }
        }
        if require_semi {
            self.error("Semicolon required here");
        } else if forbid_semi {
            items.push(BlockItem::Expr(Box::new(Expr{ty:None, u:Uxpr::Void})));
        }
        Box::new(Block{ ty: None, items })
    }

    fn parse_expr(&mut self) -> Box<Expr> {
        self.parse_assignment_expr()
    }

    // Assignment is nonassociative.  Arguably it should be block-level only.

    fn parse_assignment_expr(&mut self) -> Box<Expr> {
        let mut e = self.parse_relational_expr();
        if self.eat(Tok::Assign) {
            let lhs = self.expr_to_lvalue(e);
            let rhs = self.parse_relational_expr();
            e = Box::new(Expr{ ty: None, u: Uxpr::Assign{ lhs, rhs } })
        }
        e
    }

    fn expr_to_lvalue(&mut self, e:Box<Expr>) -> LValue {
        let u = e.u;
        match u {
            Uxpr::Id{name} => LValue::Id{ty: None, name},
            Uxpr::Deref{base, field} => LValue::Field{ty: None, base, field},
            Uxpr::Aref{base, index} => LValue::Element{ty: None, base, index},
            _ => self.error("Invalid left-hand-side of assignment")
        }
    }
    
    fn parse_leftassoc_binop<Nexter, Oper>(&mut self, next: Nexter, get_op: Oper) -> Box<Expr>
        where Nexter: Fn(&mut Parser) -> Box<Expr>,
              Oper:   Fn(&mut Parser) -> Option<Binop>
    {
        let mut e = next(self);
        while let Some(op) = get_op(self) {
            let rhs = next(self);
            e = box_binop(None, op, e, rhs);
        }
        e
    }

    fn parse_relational_expr(&mut self) -> Box<Expr> {
        self.parse_leftassoc_binop(|p:&mut Parser| p.parse_bitor_expr(),
                                   |p:&mut Parser| /**/ if p.eat(Tok::Less) { Some(Binop::Less) }
                                                   else if p.eat(Tok::LessOrEqual) { Some(Binop::LessOrEqual) }
                                                   else if p.eat(Tok::Greater) { Some(Binop::Greater) }
                                                   else if p.eat(Tok::GreaterOrEqual) { Some(Binop::GreaterOrEqual) }
                                                   else if p.eat(Tok::ULess) { Some(Binop::ULess) }
                                                   else if p.eat(Tok::ULessOrEqual) { Some(Binop::ULessOrEqual) }
                                                   else if p.eat(Tok::UGreater) { Some(Binop::UGreater) }
                                                   else if p.eat(Tok::UGreaterOrEqual) { Some(Binop::UGreaterOrEqual) }
                                                   else if p.eat(Tok::Equal) { Some(Binop::Equal) }
                                                   else if p.eat(Tok::NotEqual) { Some(Binop::NotEqual) }
                                                   else { None })
    }
                              
    fn parse_bitor_expr(&mut self) -> Box<Expr> {
        self.parse_leftassoc_binop(|p:&mut Parser| p.parse_bitxor_expr(),
                                   |p:&mut Parser| if p.eat(Tok::BitOr) { Some(Binop::BitOr) }
                                                   else { None })
    }

    fn parse_bitxor_expr(&mut self) -> Box<Expr> {
        self.parse_leftassoc_binop(|p:&mut Parser| p.parse_bitand_expr(),
                                   |p:&mut Parser| if p.eat(Tok::BitXor) { Some(Binop::BitXor) }
                                                   else { None })
    }

    fn parse_bitand_expr(&mut self) -> Box<Expr> {
        self.parse_leftassoc_binop(|p:&mut Parser| p.parse_bitshift_expr(),
                                   |p:&mut Parser| if p.eat(Tok::BitAnd) { Some(Binop::BitAnd) }
                                                   else { None })
    }

    fn parse_bitshift_expr(&mut self) -> Box<Expr> {
        self.parse_leftassoc_binop(|p:&mut Parser| p.parse_add_expr(),
                                   |p:&mut Parser| /**/ if p.eat(Tok::ShiftLeft) { Some(Binop::ShiftLeft) }
                                                   else if p.eat(Tok::ShiftRight) { Some(Binop::ShiftRight) }
                                                   else if p.eat(Tok::UShiftRight) { Some(Binop::UShiftRight) }
                                                   else { None })
    }

    fn parse_add_expr(&mut self) -> Box<Expr> {
        self.parse_leftassoc_binop(|p:&mut Parser| p.parse_mul_expr(),
                                   |p:&mut Parser| /**/ if p.eat(Tok::Add) { Some(Binop::Add) }
                                                   else if p.eat(Tok::Sub) { Some(Binop::Sub) }
                                                   else { None })
    }

    fn parse_mul_expr(&mut self) -> Box<Expr> {
        self.parse_leftassoc_binop(|p:&mut Parser| p.parse_unary_expr(),
                                   |p:&mut Parser| /**/ if p.eat(Tok::Mul) { Some(Binop::Mul) }
                                                   else if p.eat(Tok::Div) { Some(Binop::Div) }
                                                   else if p.eat(Tok::Mod) { Some(Binop::Rem) }
                                                   else if p.eat(Tok::UDiv) { Some(Binop::UDiv) }
                                                   else if p.eat(Tok::UMod) { Some(Binop::URem) }
                                                   else { None })
    }

    fn parse_unary_expr(&mut self) -> Box<Expr> {
        let op = /**/ if self.eat(Tok::BitNot) { Some(Unop::BitNot) }
                 else if self.eat(Tok::Not) { Some(Unop::Not) }
                 else if self.eat(Tok::Sub) { Some(Unop::Neg) }
                 else { None };
        if let Some(op) = op {
            box_unop(None, op, self.parse_unary_expr())
        } else {
            self.parse_type_expr()
        }
    }
    
    fn parse_type_expr(&mut self) -> Box<Expr> {
        let lhs = self.parse_primary_expr();
        if self.eat(Tok::As) {
            box_typeop(None, Typeop::As, lhs, self.parse_type())
        } else if self.eat(Tok::Is) {
            box_typeop(None, Typeop::Is, lhs, self.parse_type())
        } else {
            lhs
        }
    }

    fn parse_primary_expr(&mut self) -> Box<Expr> {
        let mut e =
            if self.eat(Tok::If) {
                let test = self.parse_expr();
                let consequent = self.parse_block();
                let alternate = if self.eat(Tok::Else) {
                    self.parse_block()
                } else {
                    Box::new(Block{ ty: None, items: vec![] })
                };
                box_if(test, consequent, alternate)
            } else if self.eat(Tok::While) {
                let test = self.parse_expr();
                let body = self.parse_block();
                Box::new(Expr{ ty: None, u: Uxpr::While{ test, body } })
            } else if self.eat(Tok::Loop) {
                let break_label = self.parse_id();
                let body = self.parse_block();
                Box::new(Expr{ ty: None, u: Uxpr::Loop{ break_label, body } })
            } else if self.eat(Tok::New) {
                if self.eat(Tok::LBracket) {
                    let ty = self.parse_type();
                    self.snarf(Tok::RBracket);
                    let mut args = self.parse_exprs(Tok::LParen, Tok::RParen);
                    if args.len() != 1 {
                        self.error("Only one argument to array `new`");
                    }
                    let length = args.remove(0);
                    Box::new(Expr{ ty: None, u: Uxpr::NewArray{ ty, length } })
                } else {
                    let ty_name = self.parse_id();
                    let mut values = self.parse_comma_separated(Tok::LBrace, Tok::RBrace,
                                                                |p:&mut Parser| {
                                                                    let field = p.parse_id();
                                                                    p.snarf(Tok::Colon);
                                                                    let value = p.parse_expr();
                                                                    (field, value)
                                                                });
                    Box::new(Expr{ ty: None, u: Uxpr::New{ ty_name, values } })
                }
            } else if self.eat(Tok::LParen) {
                let expr = self.parse_expr();
                self.snarf(Tok::RParen);
                expr
            } else if self.eat(Tok::NullLit) {
                Box::new(Expr{ ty: Some(Type::AnyRef), u: Uxpr::NullLit{ ty: Type::NullRef } })
            } else if let Tok::I32Lit(n) = self.lookahead() {
                self.get();
                Box::new(Expr{ ty:Some(Type::I32), u: Uxpr::NumLit{ value: Number::I32(n) } })
            } else if let Tok::I64Lit(n) = self.lookahead() {
                self.get();
                Box::new(Expr{ ty:Some(Type::I64), u: Uxpr::NumLit{ value: Number::I64(n) } })
            } else if let Tok::F32Lit(n) = self.lookahead() {
                self.get();
                Box::new(Expr{ ty:Some(Type::F32), u: Uxpr::NumLit{ value: Number::F32(n) } })
            } else if let Tok::F64Lit(n) = self.lookahead() {
                self.get();
                Box::new(Expr{ ty:Some(Type::F64), u: Uxpr::NumLit{ value: Number::F64(n) } })
            } else if let Tok::Id(name) = self.lookahead() {
                self.get();
                Box::new(Expr{ ty: None, u: Uxpr::Id{ name } })
            } else {
                self.error("Bad expression");
            };

        // Calls go one level only and currently have to come before derefs in any case.
        if self.peek(Tok::LParen) {
            let tmp = if let Uxpr::Id{name} = &e.u {
                let actuals = self.parse_exprs(Tok::LParen, Tok::RParen);
                Box::new(Expr{ ty: None, u: Uxpr::Call{ name: *name, actuals } })
            } else {
                self.error("Only direct calls are allowed");
            };
            e = tmp;
        }

        // Deref operators are left-associative
        loop {
            if self.eat(Tok::Dot) {
                let field = self.parse_id();
                e = Box::new(Expr{ ty: None, u: Uxpr::Deref{ base: e, field } })
            } else if self.eat(Tok::LBracket) {
                let index = self.parse_expr();
                self.snarf(Tok::RBracket);
                e = Box::new(Expr{ ty: None, u: Uxpr::Aref{ base: e, index } })
            } else {
                break;
            }
        }

        e
    }

    fn parse_id(&mut self) -> Id {
        if let Tok::Id(name) = self.get() {
            name
        } else {
            self.error("Expected name")
        }
    }

    // Token stream abstractions.

    fn snarf(&mut self, t:Tok) {
        if self.get() != t {
            self.error("Wrong token");
        }
    }

    fn peek(&mut self, t:Tok) -> bool {
        if self.lookahead() == t {
            true
        } else {
            false
        }
    }

    fn eat(&mut self, t:Tok) -> bool {
        if self.lookahead() == t {
            self.get();
            true
        } else {
            false
        }
    }

    fn eat_id(&mut self) -> Option<Id> {
        if let Tok::Id(name) = self.lookahead() {
            self.get();
            Some(name)
        } else {
            None
        }
    }

    fn get(&mut self) -> Tok {
        let t = self.cached;
        self.cached = self.lexer.lex();
        t
    }

    fn lookahead(&mut self) -> Tok {
        self.cached
    }

    fn error(&mut self, msg:&str) -> ! {
        panic!("Error on line {}: {}", self.lexer.line(), msg);
    }
}

fn ends_with_block(e:&Box<Expr>) -> bool {
    match &e.u {
        Uxpr::If{..} | Uxpr::While{..} | Uxpr::Loop{..} => true,
        _ => false
    }
}
