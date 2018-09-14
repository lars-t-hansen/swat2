#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub grammar);

mod ast;
mod xform;
mod wast;

use std::env;
use std::fs::File;
use std::io::{Read, Write};
use xform::xform;
use wast::{Emitter, Wast};

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

        let mut infile = File::open(&infilename).expect(&format!("{}: file not found", &infilename));

        let mut source = String::new();
        infile.read_to_string(&mut source).expect(&format!("{}: failed to read", &infilename));

        let prog0 = grammar::ProgramParser::new()
            .parse(&source)
            .unwrap();

        let prog1 = xform(prog0);

        let mut e = Emitter::new();
        prog1.gen(&mut e);

        let wastfilename = infilename.split_at(infilename.rfind(".swat").unwrap()).0.to_owned() + ".wast";
        let mut wastfile = File::create(&wastfilename).expect(&format!("{}: could not create", &wastfilename));

        wastfile.write(e.get_wast().as_bytes()).expect(&format!("{}: filed to write", &wastfilename));

        let jsfilename = infilename.split_at(infilename.rfind(".swat").unwrap()).0.to_owned() + ".js";
        let mut jsfile = File::create(&jsfilename).expect(&format!("{}: could not create", &jsfilename));

        jsfile.write(e.get_js().as_bytes()).expect(&format!("{}: filed to write", &jsfilename));
    }

    if numfiles == 0 {
        println!("No input files");
    }
}
