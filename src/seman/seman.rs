#![allow(unused)]

use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::ast::{Expr, Ins};
use crate::parser::type_signature::Ty;
use crate::seman::type_table::TypeTable;
use crate::source::source::{SourceFile, SourceRef};

use super::sym_table::SymbolTable;

pub struct LazyBoi {
    symbols: SymbolTable,
    types: TypeTable,
    unresolved_names: HashMap<String, Ins>,
    ongoing_resolutions_stack: Vec<String>,
}

impl LazyBoi {
    pub fn new() -> Self {
        LazyBoi {
            symbols: SymbolTable::new(),
            types: TypeTable::new(),
            unresolved_names: todo!(),
            ongoing_resolutions_stack: todo!(),
        }
    }
}

pub fn check_file(src: &SourceFile, instructions: Vec<Ins>) {
    let mut type_table = TypeTable::new();
    let mut sym_table = SymbolTable::new();

    // collect top level names
    for ins in instructions.iter() {
        let ins_id = ins.get_id(src).unwrap();
    }
}

fn check_ins(sym_table: &mut SymbolTable, type_table: &mut TypeTable, ins: &Ins) {
    todo!()
}

fn check_ins_with_name(
    sym_table: &mut SymbolTable,
    type_table: &mut TypeTable,
    instructions: Vec<Ins>,
) {
    todo!()
}

fn check_expr(sym_table: &mut SymbolTable, type_table: &mut TypeTable, expr: &Expr) {
    todo!()
}
