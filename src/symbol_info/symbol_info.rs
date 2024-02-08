#![allow(dead_code)]
#![allow(unused_variables)]

use std::collections::HashMap;

use crate::{source::source::SourceRef, types::signature::Type};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolInfo {
    // affects constants
    constant: bool,
    // affects variables
    mutable: bool,
    // affects variables that might be used before being initialized
    // or might be initialized conditionally (like in a loop or if statement)
    // or we have not resolved all the information required to check the name
    pub fully_initialized: bool,
    // affects all
    references_to_sym: Vec<SourceRef>,
    // definition location
    def_location: SourceRef,
}

impl SymbolInfo {
    pub fn new_const_info() -> Self {
        SymbolInfo {
            constant: true,
            mutable: false,
            fully_initialized: true,
            references_to_sym: vec![],
            def_location: SourceRef::dud(),
        }
    }

    pub fn new_var_info() -> Self {
        SymbolInfo {
            constant: false,
            mutable: true,
            fully_initialized: false,
            references_to_sym: vec![],
            def_location: SourceRef::dud(),
        }
    }

    pub fn new_var_info_initialized() -> Self {
        SymbolInfo {
            constant: false,
            mutable: true,
            fully_initialized: true,
            references_to_sym: vec![],
            def_location: SourceRef::dud(),
        }
    }

    pub fn update_uses(&mut self, new_use: SourceRef) {
        self.references_to_sym.push(new_use);
    }

    pub fn is_unused(&self) -> bool {
        self.references_to_sym.is_empty()
    }

    pub fn set_def_location(&mut self, new_def: SourceRef) {
        self.def_location = new_def;
    }

    pub fn is_const(&self) -> bool {
        self.constant
    }

    pub fn is_var(&self) -> bool {
        !self.constant
    }

    pub fn is_uninitialized(&self) -> bool {
        !self.fully_initialized
    }
}

// these will store the symbols and their types
#[derive(Clone)]
pub struct SymbolTable {
    pub parent: Option<Box<SymbolTable>>,
    // symbols will store symbols and their types. variables, constants
    // functions, etc.
    pub symbols: HashMap<String, (Type, SymbolInfo)>,

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

    pub fn register(&mut self, name: String, val_ty: Type, info: SymbolInfo) {
        self.symbols.insert(name, (val_ty, info));
    }

    pub fn get_type(&self, name: &str) -> Option<&Type> {
        let res = self.symbols.get(name);
        match res {
            Some((val_ty, info)) => Some(val_ty),
            None => None,
        }
    }

    pub fn get_info(&self, name: &str) -> Option<&SymbolInfo> {
        let res = self.symbols.get(name);
        match res {
            Some((_, info)) => Some(info),
            None => None,
        }
    }

    pub fn update_uses(&mut self, name: &str, new_use: SourceRef) {
        if let Some((_, info)) = self.symbols.get_mut(name) {
            info.update_uses(new_use);
        }
    }

    pub fn check_name(&self, name: &str) -> bool {
        self.symbols.contains_key(name)
    }

    pub fn insert_scoped(&mut self, name: String, scope: SymbolTable) {
        self.sub_tables.insert(name, Box::new(scope));
    }

    pub fn show_info(&self) {
        println!("\nSymbols:");
        for (name, (val_ty, info)) in &self.symbols {
            println!(
                // "{}: {:?} was used {} times.",
                "{}: {:?}",
                name,
                val_ty.as_str(),
                // info.references_to_sym.len()
            );
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
