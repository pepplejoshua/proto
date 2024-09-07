#![allow(unused)]

use std::{collections::HashMap, rc::Rc};

use crate::{parser::ast::Ins, source::source::SourceRef};

use super::type_table::TypeId;

pub struct SymbolInfo {
    name: Rc<String>,
    ty: TypeId,
    def_loc: Rc<SourceRef>,
    mutable: bool,
}

pub enum SymbolScope {
    RegularScope {
        names: HashMap<Rc<String>, SymbolInfo>,
    },
    OodScope {
        names: HashMap<Rc<String>, SymbolInfo>,
        unchecked_instructions: HashMap<Rc<String>, Ins>,
    },
}

impl SymbolScope {
    pub fn find(&self, name: &String) -> Option<&SymbolInfo> {
        match self {
            SymbolScope::RegularScope { names } | SymbolScope::OodScope { names, .. } => {
                names.get(name)
            }
        }
    }
}

pub struct SymbolTable {
    pub scopes: Vec<SymbolScope>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            scopes: vec![SymbolScope::OodScope {
                names: HashMap::new(),
                unchecked_instructions: HashMap::new(),
            }],
        }
    }

    pub fn enter_regular_scope(&mut self) {
        self.scopes.push(SymbolScope::RegularScope {
            names: HashMap::new(),
        });
    }

    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    pub fn shallow_find_symbol_info(&self, name: &String) -> Option<&SymbolInfo> {
        self.scopes.last().unwrap().find(name)
    }

    pub fn find_symbol_info(&self, name: &String) -> Option<&SymbolInfo> {
        for scope in self.scopes.iter().rev() {
            let symbol_info = scope.find(name);
            if symbol_info.is_some() {
                return symbol_info;
            }
        }
        None
    }
}
