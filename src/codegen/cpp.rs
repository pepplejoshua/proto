#![allow(unused)]

use super::tast::{TyExpr, TyFileModule, TyIns};

pub struct State {
    in_count: usize,
    cpp_code: Vec<String>,
}

impl State {
    pub fn new() -> Self {
        State {
            in_count: 0,
            cpp_code: vec![],
        }
    }

    pub fn indent(&mut self) {
        self.in_count += 4;
    }

    pub fn dedent(&mut self) {
        self.in_count -= 4;
    }

    pub fn get_pad(&self) -> String {
        " ".repeat(self.in_count)
    }
}

pub fn cpp_gen_expr(expr: &TyExpr) -> String {
    match expr {
        TyExpr::Integer { variant, val } => todo!(),
        TyExpr::Str { val } => todo!(),
        TyExpr::Char { val } => todo!(),
        TyExpr::Bool { val } => todo!(),
        TyExpr::Ident { name } => todo!(),
        TyExpr::BinOp { op, lhs, rhs } => todo!(),
        TyExpr::UnaryOp { op, expr } => todo!(),
        TyExpr::CallFn { func, args } => todo!(),
    }
}

pub fn cpp_gen_ins(ins: &TyIns) -> String {
    match ins {
        TyIns::Constant { name, ty, init } => todo!(),
        TyIns::Var { name, ty, init } => todo!(),
        TyIns::Func {
            name,
            params,
            ret_ty,
            body,
        } => todo!(),
        TyIns::Block { code } => todo!(),
        TyIns::ExprIns { expr } => todo!(),
        TyIns::Return { expr } => todo!(),
    }
}

pub fn cpp_gen_top_level(file_mod: &TyFileModule) {
    todo!()
}
