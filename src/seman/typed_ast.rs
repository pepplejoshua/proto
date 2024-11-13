#![allow(unused)]

use std::rc::Rc;

use crate::{
    parser::{ast::FnParam, type_signature::Ty},
    source::source::SourceRef,
};

pub enum TypedExpr {
    Integer {
        value: i64,
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
    Block {
        code: Vec<TypedIns>,
        loc: Rc<SourceRef>,
    },
    Error,
}
