#![allow(unused)]

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{parser::ast::Ins, source::source::SourceRef};

use super::type_table::TypeId;

#[derive(Debug, Clone)]
pub enum SymbolInfo {
    Resolved {
        ty: TypeId,
        def_loc: Rc<SourceRef>,
        mutable: bool,
    },
    Unresolved {
        ins: Rc<Ins>,
        mutable: bool,
    },
}

#[derive(Debug, Clone)]
pub struct SymbolScope {
    pub parent: Option<Rc<RefCell<SymbolScope>>>,
    pub names: HashMap<Rc<String>, Rc<SymbolInfo>>,
}

impl SymbolScope {
    pub fn new(parent: Option<Rc<RefCell<SymbolScope>>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(SymbolScope {
            parent,
            names: HashMap::new(),
        }))
    }

    pub fn shallow_lookup(&self, name: &String) -> Option<Rc<SymbolInfo>> {
        self.names.get(name).cloned()
    }

    pub fn deep_lookup(&self, name: &String) -> Option<Rc<SymbolInfo>> {
        self.names.get(name).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|p| p.borrow().deep_lookup(name))
        })
    }

    pub fn insert(&mut self, name: Rc<String>, info: Rc<SymbolInfo>) {
        self.names.insert(name, info);
    }

    pub fn create_child(&self) -> Rc<RefCell<SymbolScope>> {
        SymbolScope::new(Some(Rc::new(RefCell::new(self.clone()))))
    }
}
