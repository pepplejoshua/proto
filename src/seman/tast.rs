#![allow(unused)]

use crate::parser::ast::{BinOpType, UnaryOpType};

use super::type_table::TypeId;

pub type TyExprId = usize;
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
        operand: Box<TyExprId>,
    },
    BinOp {
        op: BinOpType,
        left: Box<TyExprId>,
        right: Box<TyExprId>,
    },
    StaticArray {
        ty: TypeId,
        items: Vec<TyExprId>,
    },
}

pub type TyInsId = usize;

#[derive(Debug, Clone)]
pub enum TyIns {}
