use ast::{Id, LocalItem, Type};
use std::collections::HashMap;
use std::rc::Rc;

pub type Signature = (Vec<Type>, Option<Type>);

pub type Intrinsic = Vec<Rc<Signature>>;

#[derive(Clone)]
pub enum Binding {
    GlobalVar(bool, Type),
    GlobalFun(Rc<Signature>),
    Intrinsic(Rc<Intrinsic>),
    Local(Id, Type)
}

pub struct IntrinsicEnv {
    intrinsics: HashMap<String, Rc<Intrinsic>>
}

impl IntrinsicEnv
{
    pub fn new() -> IntrinsicEnv {
        let mut intrinsics = HashMap::new();

        let ints = Rc::new(vec![Rc::new((vec![Type::I32], Some(Type::I32))),
                                Rc::new((vec![Type::I64], Some(Type::I64)))]);
        let floats = Rc::new(vec![Rc::new((vec![Type::F32], Some(Type::F32))),
                                  Rc::new((vec![Type::F64], Some(Type::F64)))]);
        intrinsics.insert("clz".to_string(), ints.clone());
        intrinsics.insert("ctz".to_string(), ints.clone());
        intrinsics.insert("popcnt".to_string(), ints.clone());
        intrinsics.insert("eqz".to_string(), ints.clone());
        intrinsics.insert("sqrt".to_string(), floats.clone());
        intrinsics.insert("ceil".to_string(), floats.clone());
        intrinsics.insert("floor".to_string(), floats.clone());
        intrinsics.insert("nearest".to_string(), floats.clone());
        intrinsics.insert("trunc".to_string(), floats.clone());

        IntrinsicEnv { intrinsics }
    }

    pub fn lookup(&self, name:&Id) -> Option<Binding> {
        match self.intrinsics.get(&name.name) {
            Some(b) => Some(Binding::Intrinsic(b.clone())),
            None    => None
        }
    }
}

pub struct ToplevelEnv {
    env: HashMap<String, Binding>
}

impl ToplevelEnv
{
    pub fn new() -> ToplevelEnv {
        ToplevelEnv { env: HashMap::new() }
    }

    pub fn probe(&self, name:&Id) -> bool {
        self.env.contains_key(&name.name)
    }

    pub fn lookup(&self, name:&Id) -> Option<Binding> {
        match self.env.get(&name.name) {
            Some(b) => Some(b.clone()),
            None    => None
        }
    }

    pub fn insert_function(&mut self, name:&Id, param_types:Vec<Type>, retn:Option<Type>) {
        self.env.insert(name.name.clone(), Binding::GlobalFun(Rc::new((param_types, retn))));
    }

    pub fn insert_global(&mut self, name:&Id, mutable:bool, ty:Type) {
        self.env.insert(name.name.clone(), Binding::GlobalVar(mutable, ty));
    }
}

pub struct LocalEnv {
    locals: Vec<Vec<LocalItem>>,
    gensym: usize
}

impl LocalEnv {
    pub fn new() -> LocalEnv {
        LocalEnv {
            locals: vec![],
            gensym: 0
        }
    }

    pub fn push_rib(&mut self) {
        self.locals.push(vec![]);
    }

    pub fn pop_rib(&mut self) -> Vec<LocalItem> {
        self.locals.pop().unwrap()
    }

    pub fn add_param(&mut self, param_name: &Id, param_type: Type) {
        let last = self.locals.len()-1;
        self.locals[last].push(LocalItem { name: param_name.clone(), aka: param_name.clone(), ty: param_type });
    }

    pub fn add_local(&mut self, local_name: &Id, local_type: Type) -> Id {
        let k = self.gensym;
        self.gensym += 1;
        let aka = format!("{}_{}", k, local_name);
        let last = self.locals.len()-1;
        let aka = Id { name: aka.clone() };
        self.locals[last].push(LocalItem { name: local_name.clone(), aka: aka.clone(), ty: local_type });
        aka
    }

    pub fn lookup(&self, id:&Id) -> Option<Binding> {
        let mut ribno = self.locals.len() as i32 - 1;
        while ribno >= 0 {
            let rib = &self.locals[ribno as usize];
            ribno -= 1;
            let mut locno = rib.len() as i32 - 1;
            while locno >= 0 {
                let item = &rib[locno as usize];
                locno -= 1;
                if id == &item.name {
                    return Some(Binding::Local(item.aka.clone(), item.ty));
                }
            }
        }
        None
    }
}

