#![allow(unused)]

use std::{collections::HashMap, rc::Rc};

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
    }
}

#[derive(Debug, Clone)]
pub struct SymbolScope {
    pub parent: Option<Rc<SymbolScope>>,
    pub names: HashMap<Rc<String>, SymbolInfo>,
}

impl SymbolScope {
    pub fn find(&self, name: &String) -> Option<&SymbolInfo> {
        self.names.get(name)
    }

    pub fn enter_scope(&mut self)  -> Rc<SymbolScope> {
        Rc::new(SymbolScope {
            parent: Some(self),
            names: HashMap::new(),
        });
    }
}

pub struct SymbolTable {
    pub cur_scope: Rc<SymbolScope>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            cur_scope: Rc::new(SymbolScope {
                names: HashMap::new(),
                parent: None,
            })
        }
    }

    pub fn ente_ood_scope() -> SymbolTable {
        self.scopes.push(SymbolScope::)
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
