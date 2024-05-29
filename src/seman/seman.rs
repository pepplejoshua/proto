#![allow(unused)]

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{source::source::SourceRef, types::signature::Type};

#[derive(Debug, Clone)]
pub struct NameInfo {
    ty: Type,
    is_constant: bool,
    is_init: bool,
    ref_count: usize,
    references: Vec<SourceRef>,
}

#[derive(Debug, Clone)]
pub struct TypeScope {
    names: HashMap<String, NameInfo>,
    parent: Option<Rc<RefCell<TypeScope>>>,
}

impl TypeScope {
    pub fn new() -> Self {
        TypeScope {
            names: HashMap::new(),
            parent: None,
        }
    }

    pub fn add(&mut self, name: String, info: NameInfo) {
        self.names.insert(name, info);
    }

    pub fn lookup(&self, name: &str) -> Option<NameInfo> {
        self.names.get(name).cloned()
    }

    pub fn with_parent(parent: Rc<RefCell<TypeScope>>) -> Self {
        TypeScope {
            names: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn extend(&self) -> Rc<RefCell<TypeScope>> {
        Rc::new(RefCell::new(TypeScope::with_parent(Rc::new(RefCell::new(
            self.clone(),
        )))))
    }
}
