#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub grammar);

mod ast;
mod xform;
mod wast;

use xform::Xform;
use wast::{Emitter, Wast};

const TEST: &str = "
module FibMod {
  pub var x : i32 = 2;
  pub fn fib(n: i32) -> i32 {  
    if n < x { n } else { fib(n-1) + fib(n-2) }
}
}";

fn main() {
    let prog0 = grammar::ProgramParser::new()
        .parse(TEST)
        .unwrap();

    let mut xform = Xform::new();
    let prog1 = xform.xform_program(prog0);

    let mut e = Emitter::new();
    prog1.gen(&mut e);
}
