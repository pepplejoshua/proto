#![allow(unused)]

use crate::parser::ast::{BinOpType, UnaryOpType};

use super::type_table::TypeId;

#[derive(Debug, Clone)]
pub enum TyExpr {
    Str {
        value: String,
    },
    Char {
        value: char,
    },
    Integer {
        value: isize,
    },
    Decimal {
        value: f64,
    },
    Bool {
        value: bool,
    },
    UnaryOp {
        op: UnaryOpType,
        operand: Box<TyExpr>,
    },
    BinOp {
        op: BinOpType,
        left: Box<TyExpr>,
        right: Box<TyExpr>,
    },
    StaticArray {
        ty: TypeId,
        items: Vec<TyExpr>,
    },
}

#[derive(Debug, Clone)]
pub enum TyIns {}
