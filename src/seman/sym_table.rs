#![allow(unused)]

use std::{collections::HashMap, rc::Rc};

use crate::source::source::SourceRef;

use super::type_table::TypeId;

pub struct SymbolInfo {
    name: Rc<String>,
    ty: TypeId,
    def_loc: Rc<SourceRef>,
    mutable: bool,
}

pub struct SymbolTable {
    pub scopes: Vec<HashMap<Rc<String>, SymbolInfo>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    pub fn shallow_find_symbol_info(&self, name: &String) -> Option<&SymbolInfo> {
        self.scopes.last().unwrap().get(name)
    }

    pub fn find_symbol_info(&self, name: &String) -> Option<&SymbolInfo> {
        for scope in self.scopes.iter().rev() {
            let symbol_info = scope.get(name);
            if symbol_info.is_some() {
                return symbol_info;
            }
        }
        None
    }
}
