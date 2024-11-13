#![allow(unused)]

pub enum TypedExpr {}

pub enum TypedIns {
    Block { code: Vec<TypedIns> },
}
