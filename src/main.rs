// -*- fill-column: 80 -*-

#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub grammar);

mod ast;
mod context;
mod desugarer;
mod environment;
mod flattener;
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
    let mut wastfile = File::create(&wastfilename).expect(&format!("{}: could not create", &wastfilename));

    let jsfilename = basename + ".js";
    let mut jsfile = File::create(&jsfilename).expect(&format!("{}: could not create", &jsfilename));

    for item in &mut prog.items {
        match item {
            ast::TopItem::Mod(m) => {
                let mut cx = context::Context::new();
                typechecker::check(m);
                desugarer::desugar(&mut cx, m);
                flattener::flatten(&mut cx, m);
                waster::wast(m, &mut wastfile);
            }
            ast::TopItem::Js(s) => {
                jsfile.write(s.as_bytes()).expect(&format!("{}: failed to write", &jsfilename));
            }
        }
    }
}

fn read_text_file(infilename:&str) -> String {
    let mut source = String::new();
    let mut infile = File::open(&infilename).expect(&format!("{}: file not found", &infilename));
    infile.read_to_string(&mut source).expect(&format!("{}: failed to read", &infilename));
    source
}

#[cfg(test)]
mod integration_tests
{
    use std::env;
    use std::fs::File;
    use std::io::Write;
    use std::process::Command;

    use compile_file;
    use read_text_file;

    #[test]
    fn fib() {
        compile_file("test/fib.swat");
        run_wast(&read_text_file("test/fib.wast"),
                 "assertEq(TEST.exports.fib(10), 55)");
    }

    #[test]
    fn oddeven() {
        compile_file("test/oddeven.swat");
        run_wast(&read_text_file("test/oddeven.wast"),
                 "assertEq(TEST.exports.odd(5), 1); assertEq(TEST.exports.even(5), 0);");                
    }

    // Utility code.

    // We split cmd at space boundaries heuristically:
    // - if the string after the space starts with '-' it is an option
    // - any space after an option splits words
    // - any space before the first option does not split words, ie, the
    //   command can have a path with spaces provided space is not followed
    //   by a hyphen.

    fn split_cmd_and_args(s:String) -> (String, Vec<String>) {
        // Totally bogus, WIP
        (s, vec!["--wasm-gc".to_string()])
    }

    fn run_wast(wast_text:&str, code:&str) {
        match env::var("SWATJS") {
            Err(_)  => {
                panic!("Must have environment variable SWATJS pointing to JS shell")
            }
            Ok(cmd) => {
                let tempfilename = "abracadabra.js";
                {
                    let mut tmp = File::create(tempfilename).expect("Unable to create temp");
                    writeln!(tmp, "// Delete me.
var TEST = new WebAssembly.Instance(new WebAssembly.Module(wasmTextToBinary(`
{}
        `)));
{}",
                             wast_text, code);
                }
                let (prog, mut args) = split_cmd_and_args(cmd);
                args.push(tempfilename.to_string());
                let exit_code = Command::new(prog)
                    .args(args)
                    .status()
                    .expect("Could not run test");
                assert_eq!(exit_code.success(), true);
            }
        }
    }
}
