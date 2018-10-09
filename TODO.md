Proposal for array syntax:

Type ::= ... | "[" Type "]"

Type calculus: Array(T) <: anyref.
Array(T) is not nullable (discuss).

The length is not part of the type.  Arrays are passed by reference
and carry the length.

Arrays are structurally equivalent: [i32] created one place is
compatible with [i32] created another place, independent of length.

Construction: new [T](length), if there's a default value of the type
              new [T](length, init1, .., initn) where initn will be used for element n and subsequent elements if length > n

Rvalue: arrayexpr[expr]
Lvalue: arrayexpr[expr] = ...

Length rvalue: arrayexpr.length

So:

  fn dot(as:[i32], bs:[i32]) -> i32 {
    let i: i32 = 0;
    let sum: i32 = 0;
    while i < as.length {
      sum = sum + as[i] * bs[i];
      i = i + 1;
    }
    sum
  }

Until we have array support in the engine we can desugar into linked lists or trees of private types.  Consider an [i32]:

(Name mangling is imperfect.  _A_t means array of t.  _AA_t means
array of array of t.  _NA_t is "node for array of t".  So we keep the
prefix structure and should be safe?  Not if _ can be used in names...
So switch to '.' to separate parts of names?  Watch out for field
names.  Any other separator that works with the wast format?)

  struct _NA_i32 {
    value: i32,
    next: _NA_i32
  }
  struct _A_i32 {
    length: i32,
    elements: _NA_i32
  }
  // The creation method initializes an array filled with zeroes;
  // generated code must then fill in the elements with any values
  // that are present syntactically.
  fn _A_i32_create(len:i32) -> _i32_Array {
    let elements: _NA_i32 = null;
    let n = len;
    while n > 0 {
      elements = new _NA_i32 { value: 0, next: elements };
      n = n - 1;
    }
    new _i32_Array { length: len, elements: elements }
  }
  fn _A_i32_get(a:_i32_Array, n:i32) -> i32 {
    let elements: _NA_i32 = a.elements;
    while n > 0 {
      elements = elements.next;
      n = n - 1;
    }
    elements.value
  }
  fn _A_i32_set(a:_i32_Array, n:i32, value:i32) {
    let elements: _NA_i32 = a.elements;
    while n > 0 {
      elements = elements.next;
      n = n - 1;
    }
    elements.value = value;
  }

We create these types on demand, only one per base type.

Other things in flight for MVP "simple programming language":

Testing with `cargo test`:

  - ===> need tests for lots of things
  - ===> need tests for structs in particular

Lexer / parser:

  - ===> Comments
  - use box_whatever abstractions
  - Propagate errors up the tree, do not panic
  - Probably a "continue" thing
  - Unlabeled break and continue
  - Return stmt
  - true, false literals
  - Block expressions (for scoping)
  - ===> "else if" thing, not currently allowed

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

Known bugs:

  - gensym tends to double-tag names


// Coding style for enumerations:
//
// - if one arm uses named fields then they all do (except when there are not
//   data, just the tag)
// - by and large, if the arms use unnamed fields then there should be just
//   one field
//
// Violated by the environments, largely.

