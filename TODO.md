In progress: structs!

 - we have
    - top-level    struct S { field: type, ... }
    - constructor  new S { field: value, ... }
    - read         ... = s.field
    - write        s.field = ...
    - predicate    x is S
    - downcast     x as S
    - typed null   null [coerces to the required ref type]

 - we want
    - lots of test cases!


// Coding style for enumerations:
//
// - if one arm uses named fields then they all do (except when there are not
//   data, just the tag)
// - by and large, if the arms use unnamed fields then there should be just
//   one field
//
// Violated by the environments, largely.

Other things in flight for MVP "simple programming language":

Testing with `cargo test`:

  - ===> need tests for lots of things

Lexer / parser:

  - ===> Better lexer - what we have does not work well for strings and %%JS
  - ===> Comments
  - use box_whatever abstractions
  - Propagate errors up the tree, do not panic
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

  - Expand true, false
  - Support unlabeled break and continue, add labels
  - Return stmt
  - Continue stmt
  - Block expressions

Optimizer (does not yet exist):

 - basic constant propagation will get rid of misc temporaries introduced by the
   desugaring

Flattener:

  - ===> Use better gensym tagging for locals
  - Return stmt
  - Continue stmt

Waster:

  - Support more intrinsics
  - Return stmt
