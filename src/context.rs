// Module translation context

use ast::Id;

pub struct Context {
    gensym_counter: usize
}

impl Context
{
    pub fn new() -> Context {
        Context {
            gensym_counter: 1000
        }
    }

    pub fn gensym(&mut self, tag:&str) -> Id {
        let k = self.gensym_counter;
        self.gensym_counter += 1;
        Id { name: format!("_{}_{}", tag, k) }
    }
}
