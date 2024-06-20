#![allow(unused)]
use crate::{
    parser::ast::{BinOpType, UnaryOpType},
    types::signature::{Sig, Type},
};

#[derive(Debug, Clone)]
pub enum TyExpr {
    Integer {
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
    GroupedExpr {
        inner: Box<TyExpr>,
    },
    CallFn {
        func: Box<TyExpr>,
        args: Vec<TyExpr>,
    },
    TernaryConditional {
        cond: Box<TyExpr>,
        then: Box<TyExpr>,
        otherwise: Box<TyExpr>,
    },
    StaticArray {
        vals: Vec<TyExpr>,
    },
    InterpolatedString {
        parts: Vec<TyExpr>,
    },
    MakeSlice {
        target: Box<TyExpr>,
        start: Option<Box<TyExpr>>,
        end_excl: Option<Box<TyExpr>>,
    },
}

impl TyExpr {
    pub fn as_str(&self) -> String {
        match self {
            TyExpr::Integer { val } => val.clone(),
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
            TyExpr::StaticArray { vals } => {
                let items_str = vals
                    .iter()
                    .map(|item| item.as_str())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("[{items_str}]")
            }
            TyExpr::GroupedExpr { inner } => format!("({})", inner.as_str()),
            TyExpr::TernaryConditional {
                cond,
                then,
                otherwise,
            } => format!(
                "{} ? {} : {}",
                cond.as_str(),
                then.as_str(),
                otherwise.as_str()
            ),
            TyExpr::InterpolatedString { parts } => {
                format!(
                    "{}",
                    parts
                        .iter()
                        .map(|part| {
                            match part {
                                TyExpr::Str { val } => val.clone(),
                                _ => part.as_str(),
                            }
                        })
                        .collect::<Vec<String>>()
                        .join("")
                )
            }
            TyExpr::MakeSlice {
                target,
                start,
                end_excl,
            } => todo!(),
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
    IfConditional {
        comb: Vec<(Option<TyExpr>, TyIns)>,
    },
}

impl TyIns {
    pub fn as_str(&self) -> String {
        match self {
            TyIns::Constant { name, ty, init } => {
                format!("{name} : {} : {}", ty.as_str(), init.as_str())
            }
            TyIns::Var { name, ty, init } => {
                if let Some(init) = init {
                    format!("{name} : {} : {};", ty.as_str(), init.as_str())
                } else {
                    format!("{name} : {};", ty.as_str())
                }
            }
            TyIns::Func {
                name,
                params,
                ret_ty,
                body,
            } => {
                let params_str = params
                    .iter()
                    .map(|fn_param| format!("{} {}", fn_param.name, fn_param.given_ty.as_str()))
                    .collect::<Vec<String>>();
                let params_str = params_str.join(", ");
                format!(
                    "fn {name}({params_str}) {}\n{}",
                    ret_ty.as_str(),
                    body.as_str()
                )
            }
            TyIns::Block { code } => {
                let mut buf = String::new();
                for ins in code {
                    buf.push_str(&(ins.as_str() + "\n"))
                }
                buf
            }
            TyIns::ExprIns { expr } => {
                format!("{};", expr.as_str())
            }
            TyIns::Return { expr } => {
                if let Some(expr) = expr {
                    format!("return {}", expr.as_str())
                } else {
                    "return;".to_string()
                }
            }
            TyIns::IfConditional { comb } => {
                let mut buf = String::new();
                let mut seen_if = false;
                for (cond, body) in comb.iter() {
                    match cond {
                        Some(cond) => {
                            if !seen_if {
                                buf.push_str(&format!("if {}:\n{}", cond.as_str(), body.as_str()));
                                seen_if = true;
                            } else {
                                buf.push_str(&format!(
                                    "else if {}:\n{}",
                                    cond.as_str(),
                                    body.as_str()
                                ));
                            }
                        }
                        None => buf.push_str(&format!("else:\n{}", body.as_str())),
                    }
                }
                buf
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct TyFileModule {
    pub top_level: Vec<TyIns>,
    pub src_file: String,
}

impl TyFileModule {
    pub fn new(src_file: String) -> Self {
        TyFileModule {
            top_level: vec![],
            src_file,
        }
    }
}
