use ast::{Id, Type};
use std::clone::Clone;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::rc::Rc;

pub type Signature = (Vec<Type>, Option<Type>);

pub type Intrinsic = Vec<Rc<Signature>>;

#[derive(Clone)]
pub enum Binding<T : Clone> {
    GlobalVar(bool, Type),
    GlobalFun(Rc<Signature>),
    Intrinsic(Rc<Intrinsic>),
    Local(T),
    Label
}

/*
#[derive(Debug)]
pub struct LocalItem {
    pub name: Id,               // What the source program calls it
    pub aka:  Id,               // What we call it in Local and Global nodes
    pub ty:   Type              // Its type
}
*/

pub struct IntrinsicEnv<T : Clone>
{
    intrinsics:      HashMap<String, Rc<Intrinsic>>,
    binding_payload: PhantomData<T>
}

impl<T> IntrinsicEnv<T>
    where T : Clone
{
    pub fn new() -> IntrinsicEnv<T> {
        let mut intrinsics = HashMap::new();

        let i_to_i = Rc::new(vec![Rc::new((vec![Type::I32], Some(Type::I32))),
                                  Rc::new((vec![Type::I64], Some(Type::I64)))]);
        let ii_to_i = Rc::new(vec![Rc::new((vec![Type::I32, Type::I32], Some(Type::I32))),
                                   Rc::new((vec![Type::I64, Type::I64], Some(Type::I64)))]);
        let f_to_f = Rc::new(vec![Rc::new((vec![Type::F32], Some(Type::F32))),
                                  Rc::new((vec![Type::F64], Some(Type::F64)))]);
        let ff_to_f = Rc::new(vec![Rc::new((vec![Type::F32, Type::F32], Some(Type::F32))),
                                   Rc::new((vec![Type::F64, Type::F64], Some(Type::F64)))]);

        intrinsics.insert("clz".to_string(), i_to_i.clone());
        intrinsics.insert("ctz".to_string(), i_to_i.clone());
        intrinsics.insert("popcnt".to_string(), i_to_i.clone());
        intrinsics.insert("eqz".to_string(), i_to_i.clone());

        intrinsics.insert("sqrt".to_string(), f_to_f.clone());
        intrinsics.insert("ceil".to_string(), f_to_f.clone());
        intrinsics.insert("floor".to_string(), f_to_f.clone());
        intrinsics.insert("nearest".to_string(), f_to_f.clone());
        intrinsics.insert("trunc".to_string(), f_to_f.clone());

        intrinsics.insert("rotl".to_string(), ii_to_i.clone());
        intrinsics.insert("rotr".to_string(), ii_to_i.clone());

        intrinsics.insert("copysign".to_string(), ff_to_f.clone());

        IntrinsicEnv {
            intrinsics,
            binding_payload: PhantomData
        }
    }

    pub fn lookup(&self, name:&Id) -> Option<Binding<T>> {
        match self.intrinsics.get(&name.name) {
            Some(b) => Some(Binding::Intrinsic(b.clone())),
            None    => None
        }
    }
}

pub struct ToplevelEnv<T : Clone>
{
    env: HashMap<String, Binding<T>>
}

impl<T> ToplevelEnv<T>
    where T : Clone
{
    pub fn new() -> ToplevelEnv<T> {
        ToplevelEnv { env: HashMap::new() }
    }

    pub fn probe(&self, name:&Id) -> bool {
        self.env.contains_key(&name.name)
    }

    pub fn lookup(&self, name:&Id) -> Option<Binding<T>> {
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

pub struct LocalEnv<T : Clone>
{
    // The Binding is a Label or a Local
    locals: Vec<Vec<(Id, Binding<T>)>>
}

impl<T> LocalEnv<T>
    where T : Clone
{
    pub fn new() -> LocalEnv<T> {
        LocalEnv {
            locals: vec![],
        }
    }

    pub fn push_rib(&mut self) {
        self.locals.push(vec![]);
    }

    pub fn pop_rib(&mut self) {
        self.locals.pop().unwrap();
    }

    pub fn add_param(&mut self, param_name: &Id, param_attrib: T) {
        let last = self.locals.len()-1;
        self.locals[last].push((param_name.clone(), Binding::Local(param_attrib)));
    }

/*
    pub fn add_local(&mut self, local_name: &Id, local_type: Type) -> Id {
        let k = self.gensym;
        self.gensym += 1;
        let aka = format!("{}_{}", k, local_name);
        let last = self.locals.len()-1;
        let aka = Id { name: aka.clone() };
        self.locals[last].push(LocalItem { name: local_name.clone(), aka: aka.clone(), ty: local_type });
        aka
    }
     */
    
    pub fn add_local(&mut self, local_name: &Id, local_attrib: T) {
        let last = self.locals.len()-1;
        self.locals[last].push((local_name.clone(), Binding::Local(local_attrib)));
    }

    pub fn add_label(&mut self, label: &Id) {
        let last = self.locals.len()-1;
        self.locals[last].push((label.clone(), Binding::Label));
    }

    pub fn lookup(&self, id:&Id) -> Option<Binding<T>> {
        let mut ribno = self.locals.len() as i32 - 1;
        while ribno >= 0 {
            let rib = &self.locals[ribno as usize];
            ribno -= 1;
            let mut locno = rib.len() as i32 - 1;
            while locno >= 0 {
                let item = &rib[locno as usize];
                locno -= 1;
                if id == &item.0 {
                    return Some(item.1.clone());
                }
            }
        }
        None
    }
}

