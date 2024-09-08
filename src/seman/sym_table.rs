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
        ins: Ins,
        mutable: bool,
    },
}

#[derive(Debug, Clone)]
pub struct SymbolScope {
    pub parent: Option<Rc<RefCell<SymbolScope>>>,
    pub names: HashMap<Rc<String>, SymbolInfo>,
}

impl SymbolScope {
    pub fn new(parent: Option<Rc<RefCell<SymbolScope>>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(SymbolScope {
            parent,
            names: HashMap::new(),
        }))
    }

    pub fn shallow_lookup(&self, name: &String) -> Option<SymbolInfo> {
        self.names.get(name).cloned()
    }

    pub fn deep_lookup(&self, name: &String) -> Option<SymbolInfo> {
        self.names.get(name).cloned().or_else(|| {
            self.parent
                .as_ref()
                .and_then(|p| p.borrow().deep_lookup(name))
        })
    }

    pub fn insert(&mut self, name: Rc<String>, info: SymbolInfo) {
        self.names.insert(name, info);
    }

    pub fn create_child(&self) -> Rc<RefCell<SymbolScope>> {
        SymbolScope::new(Some(Rc::new(RefCell::new(self.clone()))))
    }
}

pub struct SymbolTable {
    pub current_scope: Rc<RefCell<SymbolScope>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            current_scope: SymbolScope::new(None),
        }
    }

    pub fn enter_scope(&mut self) {
        let new_scope = self.current_scope.borrow().create_child();
        self.current_scope = new_scope;
    }

    pub fn exit_scope(&mut self) {
        if let Some(parent) = self.current_scope.clone().borrow().parent.clone() {
            self.current_scope = parent;
        } else {
            // Handle error: trying to exit the global scope
            panic!("Attempting to exit the global scope");
        }
    }

    pub fn shallow_find_symbol_info(&self, name: &String) -> Option<SymbolInfo> {
        self.current_scope.borrow().shallow_lookup(name)
    }

    pub fn deep_find_symbol_info(&self, name: &String) -> Option<SymbolInfo> {
        self.current_scope.borrow().deep_lookup(name)
    }
}
