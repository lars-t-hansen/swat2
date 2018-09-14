#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub grammar);

mod ast;
mod xform;
mod wast;

use xform::xform;
use wast::{Emitter, Wast};

const TEST: &str = "
module FibMod {
  pub var x : i32 = 2;
  pub fn fib(n: i32) -> i32 {  
    if n < x { n } else { fib(n-1) + fib(n-2) }
}
}";

// TODO: read from input files, write to output files

fn main() {
    let prog0 = grammar::ProgramParser::new()
        .parse(TEST)
        .unwrap();

    let prog1 = xform(prog0);

    let mut e = Emitter::new();
    prog1.gen(&mut e);
}
