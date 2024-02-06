#![allow(dead_code)]
#![allow(unused_variables)]

use std::collections::HashMap;

use crate::types::signature::Type;

// these will store the symbols and their types
#[derive(Clone)]
pub struct SymbolTable {
    pub parent: Option<Box<SymbolTable>>,
    // symbols will store symbols and their types. variables, constants
    // functions, etc.
    pub symbols: HashMap<String, Type>,

    // sub_tables will hold structs, enums, etc. since their scopes are
    // self-contained and if their parent scope is Preserved or SelfContained,
    // then their internals will survive scopes. If the enclosing scope is Locals,
    // then the sub_tables will be dropped when the scope ends.
    pub sub_tables: HashMap<String, Box<SymbolTable>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            parent: None,
            symbols: HashMap::new(),
            sub_tables: HashMap::new(),
        }
    }

    pub fn make_child_env(parent: SymbolTable) -> Self {
        SymbolTable {
            parent: Some(Box::new(parent)),
            symbols: HashMap::new(),
            sub_tables: HashMap::new(),
        }
    }

    pub fn return_parent_env(self) -> Option<SymbolTable> {
        self.parent.map(|x| *x)
    }

    pub fn register(&mut self, name: String, val_ty: Type) {
        self.symbols.insert(name, val_ty);
    }

    pub fn get(&self, name: &str) -> Option<&Type> {
        self.symbols.get(name)
    }

    pub fn check_name(&self, name: &str) -> bool {
        self.symbols.contains_key(name)
    }

    pub fn insert_scoped(&mut self, name: String, scope: SymbolTable) {
        self.sub_tables.insert(name, Box::new(scope));
    }

    pub fn show_info(&self) {
        println!("\nSymbols:");
        for (name, val_ty) in &self.symbols {
            println!("{}: {:?}", name, val_ty.as_str());
        }

        if self.sub_tables.is_empty() {
            return;
        }
        println!("Subtables:");
        for (name, sub_table) in &self.sub_tables {
            println!("{name}");
            sub_table.show_info();
        }
    }
}

pub type SymIndex = usize;
