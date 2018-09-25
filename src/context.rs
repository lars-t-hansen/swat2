// Module translation context

use ast::Id;

// FIXME: remove this, we don't need it.
// Or fill it with something useful, like pre-interned names.

pub struct Context {
}

impl Context
{
    pub fn new() -> Context {
        Context { }
    }

    pub fn gensym(&mut self, tag:&str) -> Id {
        Id::gensym(tag)
    }
}
