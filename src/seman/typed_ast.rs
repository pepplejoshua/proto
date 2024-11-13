#![allow(unused)]

use std::rc::Rc;

use crate::parser::{ast::FnParam, type_signature::Ty};

pub enum TypedExpr {}

pub enum TypedIns {
    DeclFunc {
        name: String,
        params: Vec<FnParam>,
        ret_ty: Rc<Ty>,
        body: Rc<TypedIns>,
    },
    Block {
        code: Vec<TypedIns>,
    },
    ErrorIns,
}
