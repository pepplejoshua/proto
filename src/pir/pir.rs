#![allow(dead_code)]
#![allow(unused_variables)]

use std::collections::HashMap;

use crate::frontend::types::TypeSignature;

pub enum Stmt {
    ExprStmt {
        expr: ExprIndex,
    },
    Function {
        name: String,
        args: HashMap<String, TypeSignature>,
        body: Vec<StmtIndex>,
    },
    Block {
        stmts: Vec<StmtIndex>,
    },
    Constant {
        name: String,
        ty: TypeSignature,
        value: ExprIndex,
    },
    TypeAlias {
        name: String,
        ty: TypeSignature,
    },
}

pub type StmtIndex = usize;

pub enum Expr {
    NameRef { name: String },
    String { value: String },
    Char { value: char },
    Bool { value: bool },
    I8 { value: i8 },
    I16 { value: i16 },
    I32 { value: i32 },
    I64 { value: i64 },
    Int { value: isize },
    U8 { value: u8 },
    U16 { value: u16 },
    U32 { value: u32 },
    U64 { value: u64 },
    UInt { value: usize },
}

pub type ExprIndex = usize;
