#![allow(unused)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::ast::{Expr, Ins};
use crate::parser::type_signature::Ty;
use crate::seman::type_table::TypeTable;
use crate::source::source::{SourceFile, SourceRef};

use super::sym_table::{SymbolInfo, SymbolScope};

pub struct LazyBoi {
    current_scope: Rc<RefCell<SymbolScope>>,
    types: TypeTable,
    ongoing_resolutions_stack: Vec<String>,
    source_file: Rc<SourceFile>,
}

impl LazyBoi {
    pub fn new(source_file: Rc<SourceFile>) -> Self {
        LazyBoi {
            current_scope: SymbolScope::new(None),
            types: TypeTable::new(),
            ongoing_resolutions_stack: vec![],
            source_file,
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

    pub fn shallow_find_symbol_info(&self, name: &String) -> Option<Rc<SymbolInfo>> {
        self.current_scope.borrow().shallow_lookup(name)
    }

    pub fn deep_find_symbol_info(&self, name: &String) -> Option<Rc<SymbolInfo>> {
        self.current_scope.borrow().deep_lookup(name)
    }

    pub fn check_file(&mut self, instructions: Vec<Ins>) {
        todo!()
    }
}

pub fn check_file(src: Rc<SourceFile>, instructions: Vec<Ins>) {
    let mut lazy_boi = LazyBoi::new(src);
    lazy_boi.check_file(instructions)
}
