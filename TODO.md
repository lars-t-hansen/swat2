For MVP "simple programming language":

Testing with `cargo test`:

  - ===> need tests for lots of things

Lexer / parser:

  - ===> Better lexer - what we have does not work well for strings and %%JS
  - ===> Comments
  - Propagate errors up the tree, do not panic
  - More operators and precedence levels
  - how do we do conversion - as casts, `i32(x)`, or with an operator, `x as i32`?
    the latter is more rust-like, and reduces confusion about type names vs idents.
  - Probably a "continue" thing
  - Unlabeled break and continue
  - Return stmt
  - true, false literals
  - Block expressions (for scoping)
  - One-armed "if" for void contexts
  - "else if" thing, not currently allowed

Env:

  - More intrinsincs, esp for type conversion

Type checker:

  - Propagate errors up the tree, do not panic
  - Support unlabeled break and continue
  - Return stmt
  - Continue stmt
  - Block expressions
  - ===> implicit widening of literals

Desugarer:

  - ===> Two missing rewrites that are logged in the text
  - Expand true, false
  - Support unlabeled break and continue, add labels
  - Return stmt
  - Continue stmt
  - Block expressions

Flattener:

  - ===> Use better gensym tagging for locals
  - Return stmt
  - Continue stmt

Waster:

  - Support more intrinsics
  - Return stmt
