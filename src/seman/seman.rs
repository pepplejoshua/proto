#![allow(unused)]

use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::ast::{Expr, Ins};
use crate::source::source::SourceRef;
use crate::types::signature::{Ty, TypeId, TypeTable};

pub struct State {
    global_scope: HashMap<String, SymbolInfo>,
    type_table: TypeTable,
    unresolved_symbols: Vec<String>,
}

struct SymbolInfo {
    name: String,
    ty: Option<TypeId>,
    loc: Rc<SourceRef>,
    declaration: Rc<Ins>,
}

pub fn check_file(instructions: Vec<Ins>) {
    let mut state = State {
        global_scope: HashMap::new(),
        type_table: TypeTable::new(),
        unresolved_symbols: vec![],
    };
    todo!()
}

fn check_expr(state: &mut State, expr: &Rc<Expr>) {
    todo!()
}

fn check_ins(state: &mut State, ins: &Rc<Ins>) {
    todo!()
}
