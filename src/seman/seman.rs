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
    scope_stack: Vec<SymbolScope>,
    current_scope: usize,
    ongoing_resolution_stack: Vec<String>,
    files: HashMap<Rc<String>, Rc<SourceFile>>,
}

pub fn check_main_file(top_level: Vec<Ins>, main_file: Rc<SourceFile>) -> Result<(), SemanError> {
    let mut state = CheckerState {
        type_table: TypeTable::new(),
        scope_stack: vec![SymbolScope::new(None, 0)],
        current_scope: 0,
        ongoing_resolution_stack: Vec::new(),
        files: HashMap::from([(main_file.path.clone(), main_file)]),
    };

    let ins_ids = prepare_ood_scope(&mut state, top_level);
    for id in ins_ids {
        match &state.scope_stack[0].names[&id].as_ref() {
            SymbolInfo::Resolved { .. } => continue,
            SymbolInfo::Unresolved { ins, mutable } => {
                // we still have to resolve it, so we can go ahead and do that
                todo!()
            }
        }
    }
    Ok(())
}

pub fn prepare_ood_scope(state: &mut CheckerState, instructions: Vec<Ins>) -> Vec<Rc<String>> {
    let mut ins_ids = vec![];
    for ins in instructions.into_iter() {
        let ins_file = &ins.get_source_ref().file;
        let ins_id = Rc::new(ins.get_id(&state.files[ins_file]).unwrap());
        ins_ids.push(ins_id.clone());
        let is_mutable = matches!(ins, Ins::DeclVar { .. });
        state.scope_stack[state.current_scope].insert(
            ins_id,
            Rc::new(SymbolInfo::Unresolved {
                ins: Rc::new(ins),
                mutable: is_mutable,
            }),
        );
    }
    ins_ids
}

pub fn check_ins(state: &mut CheckerState, ins: &Rc<Ins>) {}

pub fn check_expr(state: &mut CheckerState, expr: &Expr) {}

pub fn check_type(state: &mut CheckerState, ty: &Ty) {}
