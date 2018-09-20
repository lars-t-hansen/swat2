#[cfg(test)]

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
