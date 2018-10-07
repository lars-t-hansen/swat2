use ast::{ArrayDef,Type,TypeMap};

pub struct Context {
    array_types: TypeMap
}

impl Context {
    pub fn new() -> Context {
        Context {
            array_types: TypeMap::new()
        }
    }

    pub fn intern_array_type(&mut self, ty:Type) -> ArrayDef {
        ArrayDef { idx: self.array_types.intern(ty) }
    }

    pub fn reify_base_type(&mut self, ad:ArrayDef) -> Type {
        self.array_types.reify(ad.idx)
    }
}
