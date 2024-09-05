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
    let mut sym_table = SymbolTable::new();

    for ins in instructions.iter() {
        forward_declare_names(&mut sym_table, &mut type_table, ins);
    }
}

pub fn forward_declare_names(sym_table: &mut SymbolTable, type_table: &mut TypeTable, ins: &Ins) {
    match ins {
        Ins::DeclConst {
            name,
            ty,
            init_val,
            loc,
        } => {
            if let Some(ty) = ty {
                // intern type in table

                // register type for name into table
            } else {
            }
            todo!()
        }
        Ins::DeclVar {
            name,
            ty,
            init_val,
            loc,
        } => {
            todo!()
        }
        _ => unreachable!("non-var/const instruction in top level."),
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
