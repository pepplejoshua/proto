#![allow(dead_code)]
#![allow(unused_variables)]

use crate::{parser::pcode::PCode, symbol_info::symbol_info::SymbolTable};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CheckerScope {
    Global,
    Function,
    Struct,
    Mod,
    Block,
    Loop,
    Directive,
}

#[derive(Clone)]
pub struct Checker {
    pub pcode: PCode, // parsed code
    pub sym_table: SymbolTable,
    scope: CheckerScope,
}

impl Checker {
    pub fn new(pcode: PCode) -> Checker {
        Checker {
            pcode,
            sym_table: SymbolTable::new(),
            scope: CheckerScope::Global,
        }
    }

    pub fn check(self) {
        let pcode = self.pcode.clone();
        todo!()
    }
}
