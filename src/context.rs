use ast::{ArrayDef,Type,TypeMap,TypeMapIter};
use std::iter::IntoIterator;

pub struct Context {
    array_types: TypeMap
}

impl Context {
    pub fn new() -> Context {
        Context {
            array_types: TypeMap::new()
        }
    }

    // TODO: "array" vs "base" type
    pub fn intern_array_type(&mut self, ty:Type) -> ArrayDef {
        ArrayDef { idx: self.array_types.intern(ty) }
    }

    pub fn reify_base_type(&mut self, ad:ArrayDef) -> Type {
        self.array_types.reify(ad.idx)
    }

    pub fn iter_base_types(&mut self) -> TypeMapIter {
        self.array_types.iter()
    }
}
