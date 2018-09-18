// The flattener lowers the intermediate wasm format to something very
// close to the final format:
//
//  - all loops are labeled, all break and continue use labels
//  - all lets are removed, variables are alpha-converted
//  - all functions carry lists of defined locals
//  - all variable references have become get_local/set_local/get_global/set_global
//  - calls to intrinsics have been rewritten as intrinsic ops
//  - operations not directly available in wasm have been rewritten, eg,
//    (bitnot x) => (xor x -1), (neg x) => (- 0 x)
//  - explicit drops have been inserted when needed
//  - remove redundant blocks
//  - resulting Block nodes all have exactly one expression and no let bindings

use ast::*;

pub fn flatten(m:&mut Module) {
    let mut f = Flatten::new();
    f.flatten_module(m);
}

struct Flatten {
}

impl Flatten {
    fn new() -> Flatten {
        Flatten {}
    }

    fn flatten_module(&mut self, m:&mut Module) {
    }
}
