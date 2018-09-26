// -*- fill-column: 80 -*-

#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub grammar);

mod ast;
mod desugarer;
mod environment;
mod flattener;
#[cfg(test)] mod integration_tests;
mod typechecker;
mod waster;

use std::env;
use std::fs::File;
use std::io::{Read, Write};

fn main()
{
    let args : Vec<String> = env::args().collect();
    let mut numfiles = 0;
    for infilename in args[1..].iter() {
        if infilename.ends_with(".swat") {
            numfiles += 1;
            compile_file(infilename);
        }
    }
    if numfiles == 0 {
        println!("No input files");
    }
}

fn compile_file(infilename:&str)
{
    let basename = infilename.split_at(infilename.rfind(".swat").unwrap()).0.to_owned();

    let source = read_text_file(infilename);
    let mut prog = grammar::ProgramParser::new()
        .parse(&source)
        .unwrap();

    let wastfilename = basename.clone() + ".wast";
    let mut wastfile = File::create(&wastfilename)
        .expect(&format!("{}: could not create", &wastfilename));

    let jsfilename = basename + ".js";
    let mut jsfile = File::create(&jsfilename)
        .expect(&format!("{}: could not create", &jsfilename));

    for item in &mut prog.items {
        match item {
            ast::TopItem::Mod(m) => {
                typechecker::check(m);
                desugarer::desugar(m);
                flattener::flatten(m);
                waster::wast(m, &mut wastfile);
            }
            ast::TopItem::Js(s) => {
                jsfile.write(s.as_bytes())
                    .expect(&format!("{}: failed to write", &jsfilename));
            }
        }
    }
}

fn read_text_file(infilename:&str) -> String {
    let mut source = String::new();
    let mut infile = File::open(&infilename)
        .expect(&format!("{}: file not found", &infilename));
    infile.read_to_string(&mut source)
        .expect(&format!("{}: failed to read", &infilename));
    source
}
