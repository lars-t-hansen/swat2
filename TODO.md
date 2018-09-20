For MVP "simple programming language":

Admin:

  - upload to github for safekeeping

Testing with `cargo test`:

  - need tests for lots of things

Lexer / parser:

  - Better lexer - what we have does not work well for strings and other things
  - Propagate errors up the tree, do not panic
  - More operators and precedence levels
  - how do we do conversion - as casts, `i32(x)`, or with an operator, `x as i32`?
    the latter is more rust-like, and reduces confusion about type names vs idents.
  - Comments
  - Probably a "continue" thing
  - Unlabeled break and continue
  - Return stmt
  - true, false literals

Env:

  - More intrinsincs, esp for type conversion

Type checker:

  - Propagate errors up the tree, do not panic
  - Support more intrinsics
  - Support unlabeled break and continue
  - Return stmt
  - implicit widening of literals (only)

Desugarer:

  - Expand true, false
  - Two missing rewrites that are logged
  - Support more intrinsics
  - Support unlabeled break and continue, add labels
  - Return stmt
  - Continue stmt

Flattener:

  - Use better gensym tagging for locals
  - Return stmt
  - Continue stmt

Waster:

  - Support more intrinsics
  - Return stmt
  - Continue stmt