#![allow(dead_code)]
#![allow(unused_variables)]

use std::collections::HashMap;

use crate::frontend::types::ValueType;

// these will store the symbols and their types
pub struct SymbolTable {
    parent: Option<Box<SymbolTable>>,
    // symbols will store symbols and their types. variables, constants
    // functions, etc.
    pub symbols: HashMap<String, ValueType>,
    // scoped_symbols will hold structs, enums, etc. since their scopes are
    // self-contained and if their parent scope is Preserved or SelfContained,
    // then their internals will survive scopes. If the enclosing scope is Locals,
    // then the scoped_symbols will be dropped when the scope ends.
    pub scoped_symbols: HashMap<String, Box<SymbolTable>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            parent: None,
            symbols: HashMap::new(),
            scoped_symbols: HashMap::new(),
        }
    }

    pub fn make_child_env(parent: SymbolTable) -> Self {
        SymbolTable {
            parent: Some(Box::new(parent)),
            symbols: HashMap::new(),
            scoped_symbols: HashMap::new(),
        }
    }

    pub fn return_parent_env(self) -> Option<SymbolTable> {
        self.parent.map(|x| *x)
    }

    pub fn insert(&mut self, name: String, val_ty: ValueType) {
        self.symbols.insert(name, val_ty);
    }

    pub fn get(&self, name: &str) -> Option<&ValueType> {
        self.symbols.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut ValueType> {
        self.symbols.get_mut(name)
    }

    pub fn check_name(&self, name: &str) -> bool {
        self.symbols.contains_key(name)
    }

    pub fn insert_scoped(&mut self, name: String, scope: SymbolTable) {
        self.scoped_symbols.insert(name, Box::new(scope));
    }
}

pub type SymIndex = usize;
