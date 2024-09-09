#![allow(unused)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::ast::{Expr, Ins};
use crate::parser::type_signature::Ty;
use crate::seman::type_table::TypeTable;
use crate::source::errors::SemanError;
use crate::source::source::{SourceFile, SourceRef};

use super::sym_table::{SymbolInfo, SymbolScope};

pub struct CheckerState {
    type_table: TypeTable,
    current_scope: Rc<RefCell<SymbolScope>>,
    ongoing_resolution_stack: Vec<String>,
}

pub fn check_main_file(top_level: Vec<Ins>) -> Result<(), SemanError> {
    let mut state = CheckerState {
        type_table: TypeTable::new(),
        current_scope: SymbolScope::new(None),
        ongoing_resolution_stack: Vec::new(),
    };

    check_ood_scope(&mut state, top_level);
    Ok(())
}

pub fn check_ood_scope(state: &mut CheckerState, instructions: Vec<Ins>) {
    // this will ensure that names are forward declared in this scope even if
    // they have not been checked yet. when we run into an unchecked name, we
    // will want to check it immediately and then replace its entry in the
    // symbol table with the resolved type. we will also need to track the
    // names in the right order
}

pub fn check_ins(state: &mut CheckerState) {}

pub fn check_expr(state: &mut CheckerState) {}

pub fn check_type(state: &mut CheckerState) {}

fn store_typed_ins(state: &mut CheckerState, ins: Ins) {}
