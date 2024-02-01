use std::collections::HashMap;

use crate::{
    frontend::{
        bcode::Index,
        types::{EInfo, ValueType},
    },
    symbol_info::symbol_info::SymbolTable,
};

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Env {
    // name is the key and the value is a tuple of
    // an index to the type and a value (constant value)
    pub names: HashMap<String, (ValueType, Option<EInfo>)>,
    pub parent: Option<Box<Env>>,
    pub cur_fn_end_index: Option<Index>,
    pub cur_fn_return_ty_index: Option<Index>,
}

#[allow(dead_code)]
pub type EnvIndex = usize;

#[allow(dead_code)]
impl Env {
    pub fn new() -> Self {
        Env {
            names: HashMap::new(),
            parent: None,
            cur_fn_end_index: None,
            cur_fn_return_ty_index: None,
        }
    }

    pub fn to_symbol_table(self) -> SymbolTable {
        let mut st = SymbolTable::new();
        for (name, (ty, _)) in self.names.iter() {
            st.insert(name.clone(), ty.clone());
        }
        st
    }

    pub fn make_child_env(parent: Env) -> Self {
        Env {
            names: HashMap::new(),
            parent: Some(Box::new(parent)),
            cur_fn_end_index: None,
            cur_fn_return_ty_index: None,
        }
    }

    pub fn return_parent_env(self) -> Option<Env> {
        self.parent.map(|x| *x)
    }

    pub fn check_name(&self, name: &str) -> bool {
        self.names.contains_key(name)
    }

    pub fn name_has_value(&self, name: &str) -> bool {
        self.names.get(name).unwrap().1.is_some()
    }

    pub fn get_info_for_name(&self, name: &str) -> Option<EInfo> {
        self.names.get(name).unwrap().1.clone()
    }

    pub fn declare_constant(&mut self, name: String, val_ty: ValueType, info: EInfo) {
        self.names.insert(name, (val_ty, Some(info)));
    }

    pub fn show_env_info(&self) {
        if self.names.is_empty() {
            return;
        }
        println!("Env info:");
        for (name, (ty, info)) in self.names.iter() {
            print!("{}: {:?}", name, ty.tag);
            if let Some(info) = info {
                println!(" {:?}", info);
            } else {
                println!();
            }
        }
    }
}
