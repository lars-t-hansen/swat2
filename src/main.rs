#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub grammar);

mod ast;

fn main() {
    let m = grammar::ProgramParser::new()
        .parse("
module MyMod {
  export var x : i32 = 1;
  fn fib(n: i32) -> i32 {  
    if n < 2 { n } else { fib(n-1) + fib(n-2) }
}
}")
        .unwrap();
    println!("{:?}", m);
}
