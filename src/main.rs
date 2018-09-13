#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub grammar);

mod ast;
mod wast;

use wast::{Emitter, Wast};

const TEST: &str = "
module FibMod {
  pub var x : i32 = 2;
  pub fn fib(n: i32) -> i32 {  
    if n < x { n } else { fib(n-1) + fib(n-2) }
}
}";

fn main() {
    let m = grammar::ProgramParser::new()
        .parse(TEST)
        .unwrap();
    let mut e = Emitter::new();
    m.gen(&mut e);
}
