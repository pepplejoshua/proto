#![allow(unused)]

use std::rc::Rc;

use crate::{
    parser::{
        ast::{BinOpType, FnParam, UnaryOpType},
        type_signature::Ty,
    },
    source::source::SourceRef,
};

pub enum SignedInteger {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    Int(isize),
}

pub enum UnsignedInteger {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    Uint(usize),
}

pub enum TypedExpr {
    UntypedInt {
        value: usize,
        ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    SignedInt {
        value: SignedInteger,
        ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    UnsignedInt {
        value: UnsignedInteger,
        ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    Float {
        value: f64,
        ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    CallFn {
        func: Rc<TypedExpr>,
        args: Vec<Rc<TypedExpr>>,
        ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    UnaryOp {
        op: UnaryOpType,
        expr: Rc<TypedExpr>,
        ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    BinOp {
        op: BinOpType,
        left: Rc<TypedExpr>,
        right: Rc<TypedExpr>,
        ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    Identifier {
        name: String,
        ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    Bool {
        value: bool,
        ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    Str {
        value: String,
        ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    Char {
        value: char,
        ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    Optional {
        value: Option<Rc<TypedExpr>>,
        ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    Error,
}

pub enum TypedIns {
    DeclFunc {
        name: String,
        params: Vec<FnParam>,
        ret_ty: Rc<Ty>,
        body: Rc<TypedIns>,
        loc: Rc<SourceRef>,
    },
    DeclVariable {
        name: String,
        ty: Rc<Ty>,
        init_value: TypedExpr,
        is_mutable: bool,
        loc: Rc<SourceRef>,
    },
    AssignTo {
        target: TypedExpr,
        value: TypedExpr,
        loc: Rc<SourceRef>,
    },
    Block {
        code: Vec<TypedIns>,
        loc: Rc<SourceRef>,
    },
    Error,
}
