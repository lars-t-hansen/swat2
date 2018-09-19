#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub grammar);

mod ast;
mod desugarer;
mod environment;
mod flattener;
mod typecheck;
//mod xform;
mod waster;

use std::env;
use std::fs::File;
use std::io::{Read, Write};

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

        let wastfilename = infilename.split_at(infilename.rfind(".swat").unwrap()).0.to_owned() + ".wast";
        let mut wastfile = File::create(&wastfilename).expect(&format!("{}: could not create", &wastfilename));

        let jsfilename = infilename.split_at(infilename.rfind(".swat").unwrap()).0.to_owned() + ".js";
        let mut jsfile = File::create(&jsfilename).expect(&format!("{}: could not create", &jsfilename));

        for item in &mut prog0.items {
            match item {
                ast::TopItem::Mod(m) => {
                    typecheck::check(m);
                    desugarer::desugar(m);
                    flattener::flatten(m);
                    waster::wast(m, &mut wastfile);
                }
                ast::TopItem::Js(s) => {
                    jsfile.write(s.as_bytes()).expect(&format!("{}: failed to write", &jsfilename));
                }
            }
        }

/*
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
*/
    }

    if numfiles == 0 {
        println!("No input files");
    }
}
