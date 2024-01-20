use std::collections::HashMap;

use crate::frontend::types::ValueType;

use super::forge::EInfo;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Env {
    // name is the key and the value is a tuple of 
    // an index to the type and a value (constant value)
    pub names: HashMap<String, (ValueType, Option<EInfo>)>,
    pub parent: Option<Box<Env>>,
}

#[allow(dead_code)]
pub type EnvIndex = usize;

#[allow(dead_code)]
impl Env {
    pub fn new() -> Self {
        Env {
            names: HashMap::new(),
            parent: None,
        }
    }

    pub fn make_child_env(parent: Env) -> Self {
        Env {
            names: HashMap::new(),
            parent: Some(Box::new(parent)),
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
}
