#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub grammar);

mod ast;
mod environment;
mod typecheck;
mod xform;
mod wast;

use std::env;
use std::fs::File;
use std::io::{Read, Write};
use wast::{Emitter, Wast};

// Architectural changes:
//
// - "xform" should become "check", which should type check everything
//   and regularize at a high level and return a different kind of
//   Module that has top-level information collected
//
// - there should be a "lower" before code generation, to rewrite
//   operations that have no analogy in wasm, eg, -x becomes 0-x, ~x
//   becomes x^-1, probably a few others, so that the emitter does not
//   need to do this; also we could transform from named entities to
//   numbered entities here, the output could be some closer-to-wasm
//   form.
//
// - the emitter should not do very much.

fn main()
{
    let args : Vec<String> = env::args().collect();

    let mut numfiles = 0;
    for arg in args[1..].iter() {
        let infilename = arg;
        if !infilename.ends_with(".swat") {
            continue;
        }
        numfiles += 1;

        let mut source = String::new();
        {
            let mut infile = File::open(&infilename).expect(&format!("{}: file not found", &infilename));
            infile.read_to_string(&mut source).expect(&format!("{}: failed to read", &infilename));
        }

        let mut prog0 = grammar::ProgramParser::new()
            .parse(&source)
            .unwrap();

        typecheck::check(&mut prog0);

        let prog1 = xform::xform(prog0);

        let mut e = Emitter::new();
        prog1.gen(&mut e);

        {
            let wastfilename = infilename.split_at(infilename.rfind(".swat").unwrap()).0.to_owned() + ".wast";
            let mut wastfile = File::create(&wastfilename).expect(&format!("{}: could not create", &wastfilename));
            wastfile.write(e.get_wast().as_bytes()).expect(&format!("{}: failed to write", &wastfilename));
        }

        {
            let jsfilename = infilename.split_at(infilename.rfind(".swat").unwrap()).0.to_owned() + ".js";
            let mut jsfile = File::create(&jsfilename).expect(&format!("{}: could not create", &jsfilename));
            jsfile.write(e.get_js().as_bytes()).expect(&format!("{}: failed to write", &jsfilename));
        }
    }

    if numfiles == 0 {
        println!("No input files");
    }
}
