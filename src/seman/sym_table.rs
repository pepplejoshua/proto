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
    pub scope_type_id: Option<TypeId>,
}

impl SymbolScope {
    pub fn new(parent: Option<Rc<RefCell<SymbolScope>>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(SymbolScope {
            parent,
            names: HashMap::new(),
            scope_type_id: None,
        }))
    }

    pub fn new_type_scope(
        parent: Option<Rc<RefCell<SymbolScope>>>,
        src_type_id: TypeId,
    ) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(SymbolScope {
            parent,
            names: HashMap::new(),
            scope_type_id: Some(src_type_id),
        }))
    }

    pub fn insert(&mut self, name: Rc<String>, info: Rc<SymbolInfo>) {
        self.names.insert(name, info);
    }

    pub fn create_child(&self) -> Rc<RefCell<SymbolScope>> {
        SymbolScope::new(Some(Rc::new(RefCell::new(self.clone()))))
    }
}
