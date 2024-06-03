#![allow(unused)]

use std::collections::HashMap;

use crate::{
    parser::ast::{Expr, Ins},
    types::signature::{Sig, Type},
};

#[derive(Debug, Clone)]
struct TypeEnv {
    vars: HashMap<String, Type>,
}

impl TypeEnv {
    fn new() -> Self {
        TypeEnv {
            vars: HashMap::new(),
        }
    }

    fn extend(&self, var: String, ty: Type) -> Self {
        let mut new_env = self.clone();
        new_env.vars.insert(var, ty);
        new_env
    }

    fn lookup(&self, var: &str) -> Option<&Type> {
        self.vars.get(var)
    }
}

#[derive(Debug, Clone)]
pub struct Side {
    ty: Type,
    is_inferred: bool,
}

#[derive(Debug, Clone)]
pub struct Constraint {
    left: Side,
    right: Side,
}

impl Constraint {
    pub fn as_str(&self) -> String {
        format!("{} = {}", self.left.ty.as_str(), self.right.ty.as_str())
    }
}

#[derive(Debug, Clone)]
pub struct State {
    constraints: Vec<Constraint>,
    env: TypeEnv,
}

impl State {
    pub fn new() -> Self {
        State {
            constraints: vec![],
            env: TypeEnv::new(),
        }
    }
}

pub fn collect_constraints_from_ins(ins: &Ins, state: &mut State) -> Result<(), String> {
    match &ins {
        Ins::DeclConst {
            name,
            ty,
            init_val,
            loc,
        } => todo!(),
        Ins::DeclVar {
            name,
            ty,
            init_val,
            loc,
        } => todo!(),
        Ins::DeclFunc {
            name,
            params,
            ret_type,
            body,
            loc,
        } => todo!(),
        Ins::DeclStruct { name, body, loc } => todo!(),
        Ins::DeclModule { name, body, loc } => todo!(),
        Ins::Block { code, loc } => todo!(),
        Ins::AssignTo { target, value, loc } => todo!(),
        Ins::ExprIns { expr, loc } => todo!(),
        Ins::Return { expr, loc } => todo!(),
        Ins::SingleLineComment { comment, loc } => todo!(),
        Ins::ErrorIns { msg, loc } => todo!(),
    }
}

pub fn collect_constraints_from_expr(expr: &Expr, state: &mut State) -> Result<Type, String> {
    match &expr {
        Expr::Number { val, loc } => todo!(),
        Expr::Str { val, loc } => todo!(),
        Expr::Char { val, loc } => todo!(),
        Expr::Bool { val, loc } => todo!(),
        Expr::Void { loc } => todo!(),
        Expr::Ident { name, loc } => todo!(),
        Expr::BinOp {
            op,
            left,
            right,
            loc,
        } => todo!(),
        Expr::InitStruct {
            struct_name,
            fields,
            loc,
        } => todo!(),
        Expr::CallFn { func, args, loc } => todo!(),
        Expr::UnaryOp { op, expr, loc } => todo!(),
        Expr::ErrorExpr { msg, loc } => todo!(),
    }
}
