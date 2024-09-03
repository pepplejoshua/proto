#![allow(unused)]

use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::ast::{Expr, Ins};
use crate::parser::type_signature::Ty;
use crate::seman::type_table::TypeTable;
use crate::source::source::SourceRef;

use super::sym_table::SymbolTable;

pub fn check_file(instructions: Vec<Ins>) {
    let mut type_table = TypeTable::new();
    let mut sym_table = todo!();
    todo!()
}

fn check_expr(sym_table: &mut SymbolTable, type_table: &mut TypeTable, expr: &Rc<Expr>) {
    todo!()
}

fn check_ins(sym_table: &mut SymbolTable, type_table: &mut TypeTable, ins: &Rc<Ins>) {
    todo!()
}
