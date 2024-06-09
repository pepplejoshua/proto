#![allow(unused)]
use crate::{
    parser::ast::{BinOpType, UnaryOpType},
    types::signature::{Sig, Type},
};

#[derive(Debug, Clone)]
pub enum TyExpr {
    Integer {
        variant: Sig,
        val: String,
    },
    Str {
        val: String,
    },
    Char {
        val: char,
    },
    Bool {
        val: bool,
    },
    Ident {
        name: String,
    },
    BinOp {
        op: BinOpType,
        lhs: Box<TyExpr>,
        rhs: Box<TyExpr>,
    },
    UnaryOp {
        op: UnaryOpType,
        expr: Box<TyExpr>,
    },
    CallFn {
        func: String,
        args: Vec<TyExpr>,
    },
}

impl TyExpr {
    pub fn as_str(&self) -> String {
        match self {
            TyExpr::Integer { variant, val } => format!("{val}#({:?})", variant),
            TyExpr::Str { val } => format!("\"{val}\""),
            TyExpr::Char { val } => format!("'{val}'"),
            TyExpr::Bool { val } => format!(
                "{v}",
                v = if *val {
                    "true".to_string()
                } else {
                    "false".to_string()
                }
            ),
            TyExpr::Ident { name } => name.clone(),
            TyExpr::BinOp { op, lhs, rhs } => {
                format!("[{} {} {}]", lhs.as_str(), op.as_str(), rhs.as_str())
            }
            TyExpr::UnaryOp { op, expr } => format!("{}{}", op.as_str(), expr.as_str()),
            TyExpr::CallFn { func, args } => {
                let args_str = args.iter().map(|arg| arg.as_str()).collect::<Vec<String>>();
                let args_str = args_str.join(", ");
                format!("{}({args_str})", func.as_str())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct TyFnParam {
    pub name: String,
    pub given_ty: Type,
}

#[derive(Debug, Clone)]
pub enum TyIns {
    Constant {
        name: String,
        ty: Type,
        init: TyExpr,
    },
    Var {
        name: String,
        ty: Type,
        init: Option<TyExpr>,
    },
    Func {
        name: String,
        params: Vec<TyFnParam>,
        ret_ty: Type,
        body: Box<TyIns>,
    },
    Block {
        code: Vec<TyIns>,
    },
    ExprIns {
        expr: TyExpr,
    },
    Return {
        expr: Option<TyExpr>,
    },
}
