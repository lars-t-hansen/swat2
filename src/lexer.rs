use ast;
use std::str::FromStr;

#[derive(Clone, Copy, PartialEq)]
pub enum Tok
{
    // Out-of-band
    EOI,

    // Keywords
    AnyRef,
    As,
    Break,
    Const,
    Else,
    Extern,
    F32,
    F64,
    Fn,
    I32,
    I64,
    If,
    Is,
    Let,
    Loop,
    Module,
    New,
    NullLit,
    Pub,
    Struct,
    Var,
    While,

    // Punctuation
    Colon,
    Semi,
    Comma,
    Dot,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    LParen,
    RParen,
    Assign,
    Arrow,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
    ULess,
    ULessOrEqual,
    UGreater,
    UGreaterOrEqual,
    Equal,
    NotEqual,
    BitOr,
    BitXor,
    BitAnd,
    ShiftLeft,
    ShiftRight,
    UShiftRight,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    UDiv,
    UMod,
    BitNot,
    Not,

    // Other
    F32Lit(f32),
    F64Lit(f64),
    I32Lit(i32),
    I64Lit(i64),
    Id(ast::Id)
}

pub struct Lexer<'a>
{
    input:  &'a [u8],
    pos:    usize,
    len:    usize,
    lineno: usize
}

impl<'a> Lexer<'a> {

    // Input is UTF8 though we just disallow non-ASCII for now
    pub fn new(input: &'a [u8]) -> Lexer<'a> {
        Lexer {
            input,
            pos: 0,
            len: input.len(),
            lineno: 1
        }
    }

    pub fn line(&self) -> usize {
        self.lineno
    }
    
    pub fn lex(&mut self) -> Tok {
        loop {
            let c = self.getc();
            match c {
                '\0' => { return Tok::EOI; }
                ' ' | '\t' => { }
                '\r' => {
                    if self.peekc() == '\n' { self.pos += 1; }
                    self.lineno += 1;
                }
                '\n' => { self.lineno += 1; }
                ':' => { return Tok::Colon; }
                ';' => { return Tok::Semi; }
                '.' => { return Tok::Dot; }
                ',' => { return Tok::Comma; }
                '{' => { return Tok::LBrace; }
                '}' => { return Tok::RBrace; }
                '[' => { return Tok::LBracket; }
                ']' => { return Tok::RBracket; }
                '(' => { return Tok::LParen; }
                ')' => { return Tok::RParen; }
                '=' => { return match self.peekc() {
                    '=' => { self.pos += 1; Tok::Equal }
                    _   => { Tok::Assign }
                }}
                '-' => { return match self.peekc() {
                    '>' => { self.pos += 1; Tok::Arrow }
                    _   => { Tok::Sub }
                }}
                '<' => { return match self.peekc3() {
                    ('<',_,_)   => { self.pos += 1; Tok::ShiftLeft }
                    ('=','u',x) => { self.disallow_subsequent(x); self.pos += 2; Tok::ULessOrEqual }
                    ('=',_,_)   => { self.pos += 1; Tok::LessOrEqual }
                    ('u',x,_)   => { self.disallow_subsequent(x); self.pos += 1; Tok::ULess }
                    (_,_,_)     => { Tok::Less }
                }}
                '>' => { return match self.peekc3() {
                    ('>','u',x) => { self.disallow_subsequent(x); self.pos += 2; Tok::UShiftRight }
                    ('>', _,_)  => { self.pos += 1; Tok::ShiftRight }
                    ('=','u',x) => { self.disallow_subsequent(x); self.pos += 2; Tok::UGreaterOrEqual }
                    ('=',_,_)   => { self.pos += 1; Tok::GreaterOrEqual }
                    ('u',_,_)   => { self.pos += 1; Tok::UGreater }
                    (_,_,_)     => { Tok::Greater }
                }}
                '!' => { return match self.peekc() {
                    '=' => { self.pos += 1; Tok::NotEqual }
                    _   => { Tok::Not }
                }}
                '|' => { return Tok::BitOr; }
                '^' => { return Tok::BitXor; }
                '&' => { return Tok::BitAnd; }
                '+' => { return Tok::Add; }
                '*' => { return Tok::Mul; }
                '/' => {
                    if self.peekc() == '/' {
                        self.line_comment();
                        continue;
                    }
                    return match self.peekc2() {
                        ('u',x) => { self.disallow_subsequent(x); self.pos += 1; Tok::UDiv }
                        (_,_)   => { Tok::Div }
                    }
                }
                '%' => { return match self.peekc2() {
                    ('u',x) => { self.disallow_subsequent(x); self.pos += 1; Tok::UMod }
                    (_,_)   => { Tok::Mod }
                }}
                '~' => { return Tok::BitNot; }
                '0'...'9' => { return self.number(c); }
                'a'...'z' | 'A'...'Z' => { return self.ident_or_keyword(c); }
                _ => { self.error(&format!("Unrecognized character `{}`", c)); }
            }
        }
    }

    fn line_comment(&mut self) {
        loop {
            let c = self.peekc();
            if c == '\n' || c == '\r' || c == '\0' {
                return;
            }
            self.pos += 1;
        }
    }

    fn ident_or_keyword(&mut self, c:char) -> Tok {
        let mut cs:String = String::new();
        cs.push(c);
        while is_subsequent(self.peekc()) {
            cs.push(self.getc());
        }
        match cs.as_ref() {
            "anyref" => Tok::AnyRef,
            "as"     => Tok::As,
            "break"  => Tok::Break,
            "const"  => Tok::Const,
            "else"   => Tok::Else,
            "extern" => Tok::Extern,
            "f32"    => Tok::F32,
            "f64"    => Tok::F64,
            "fn"     => Tok::Fn,
            "i32"    => Tok::I32,
            "i64"    => Tok::I64,
            "if"     => Tok::If,
            "is"     => Tok::Is,
            "let"    => Tok::Let,
            "loop"   => Tok::Loop,
            "module" => Tok::Module,
            "new"    => Tok::New,
            "null"   => Tok::NullLit,
            "pub"    => Tok::Pub,
            "struct" => Tok::Struct,
            "var"    => Tok::Var,
            "while"  => Tok::While,
            _        => Tok::Id(ast::Id::intern(&cs))
        }
    }

    fn number(&mut self, c:char) -> Tok {
        let mut cs:String = String::new();
        let mut is_float = false;

        cs.push(c);
        self.digits(&mut cs);

        if self.peekc() == '.' {
            is_float = true;
            cs.push(self.getc());
            if self.digits(&mut cs) == 0 {
                self.error("Empty fraction");
            }
        }

        if self.peekc() == 'e' || self.peekc() == 'E' {
            is_float = true;
            cs.push(self.getc());
            if self.peekc() == '-' || self.peekc() == '+' {
                cs.push(self.getc());
            }
            if self.digits(&mut cs) == 0 {
                self.error("Empty exponent");
            }
        }

        // TODO: technically: no letter following suffix, and if not a
        // suffix then not a letter.

        match self.peekc() {
            'i' | 'I' => {
                self.pos += 1;
                if is_float {
                    self.error("Integer suffix on floating number");
                }
                match i32::from_str_radix(&cs, 10) {
                    Ok(v)  => Tok::I32Lit(v),
                    Err(_) => self.error(&format!("Not a valid i32 literal {}", cs))
                }
            }
            'l' | 'L' => {
                self.pos += 1;
                if is_float {
                    self.error("Integer suffix on floating number");
                }
                match i64::from_str_radix(&cs, 10) {
                    Ok(v)  => Tok::I64Lit(v),
                    Err(_) => self.error(&format!("Not a valid i64 literal {}", cs))
                }
            }
            'f' | 'F' => {
                self.pos += 1;
                match f32::from_str(&cs) {
                    Ok(v)  => Tok::F32Lit(v),
                    Err(_) => self.error(&format!("Not a valid f32 literal {}", cs))
                }
            }
            'd' | 'D' => {
                self.pos += 1;
                match f64::from_str(&cs) {
                    Ok(v)  => Tok::F64Lit(v),
                    Err(_) => self.error(&format!("Not a valid f64 literal {}", cs)),
                }
            }
            _ => {
                if is_float {
                    match f64::from_str(&cs) {
                        Ok(v)  => Tok::F64Lit(v),
                        Err(_) => self.error(&format!("Not a valid f64 literal {}", cs)),
                    }
                } else {
                    match i32::from_str_radix(&cs, 10) {
                        Ok(v)  => Tok::I32Lit(v),
                        Err(_) => self.error(&format!("Not a valid i32 literal {}", cs))
                    }
                }
            }
        }
    }

    fn digits(&mut self, cs:&mut String) -> usize {
        let start = self.pos;
        loop {
            let d = self.peekc();
            if d < '0' || d > '9' {
                break;
            }
            self.pos += 1;
            cs.push(d);
        }
        self.pos - start
    }

    fn getc(&mut self) -> char {
        if self.pos == self.len {
            '\0'
        } else {
            let c = self.input[self.pos];
            self.pos += 1;
            c as char
        }
    }

    fn peekc(&self) -> char {
        self.peekat(0)
    }

    fn peekc2(&self) -> (char, char) {
        (self.peekat(0), self.peekat(1))
    }

    fn peekc3(&mut self) -> (char, char, char) {
        (self.peekat(0), self.peekat(1), self.peekat(2))
    }

    fn peekat(&self, k: usize) -> char {
        if self.pos + k >= self.len {
            '\0'
        } else {
            self.input[self.pos + k] as char
        }
    }

    fn disallow_subsequent(&mut self, c: char) {
        if is_subsequent(c) {
            self.error("Ambiguous operator");
        }
    }

    fn error(&self, msg:&str) -> ! {
        panic!("Error on line {}: {}", self.lineno, msg);
    }
}

fn is_subsequent(c: char) -> bool {
    c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c == '_'
}
