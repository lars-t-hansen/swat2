#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub grammar);

mod ast;

fn main() {
    let m = grammar::ProgramParser::new()
        .parse("module MyMod { export var x : i32 = 0; }")
        .unwrap();
    println!("{:?}", m);
}
