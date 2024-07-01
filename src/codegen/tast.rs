#![allow(unused)]
use crate::{
    parser::ast::{BinOpType, UnaryOpType},
    types::signature::Ty,
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
    MultiExpr {
        exprs: Vec<TyExpr>,
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
    InitStruct {
        struct_name: String,
        fields: Vec<(String, TyExpr)>,
    },
    InterpolatedString {
        parts: Vec<TyExpr>,
    },
    MakeSliceFrom {
        target: Box<TyExpr>,
        start: Box<TyExpr>,
    },
    MakeSliceWithEnd {
        target: Box<TyExpr>,
        start: Box<TyExpr>,
        end_excl: Box<TyExpr>,
    },
    IndexInto {
        target: Box<TyExpr>,
        index: Box<TyExpr>,
    },
    AccessMember {
        target: Box<TyExpr>,
        mem: Box<TyExpr>,
    },
    OptionalExpr {
        val: Option<Box<TyExpr>>,
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
                                _ => format!("{{{}}}", part.as_str()),
                            }
                        })
                        .collect::<Vec<String>>()
                        .join("")
                )
            }
            TyExpr::MakeSliceFrom { target, start } => {
                format!("{}[{}:]", target.as_str(), start.as_str())
            }
            TyExpr::MakeSliceWithEnd {
                target,
                start,
                end_excl,
            } => {
                format!(
                    "{}[{}:{}]",
                    target.as_str(),
                    start.as_str(),
                    end_excl.as_str()
                )
            }
            TyExpr::MultiExpr { exprs } => {
                format!(
                    "({})",
                    exprs
                        .iter()
                        .map(|e| { e.as_str() })
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            TyExpr::IndexInto { target, index } => {
                format!("{}[{}]", target.as_str(), index.as_str())
            }
            TyExpr::AccessMember { target, mem } => {
                format!("{}.{}", target.as_str(), mem.as_str())
            }
            TyExpr::OptionalExpr { val } => match val {
                Some(v) => format!("some({})", v.as_str()),
                None => format!("none"),
            },
            TyExpr::InitStruct {
                struct_name,
                fields,
            } => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TyFnParam {
    pub name: String,
    pub given_ty: Ty,
}

#[derive(Debug, Clone)]
pub enum TyIns {
    Constant {
        name: String,
        ty: Ty,
        init: TyExpr,
    },
    Var {
        name: String,
        ty: Ty,
        init: Option<TyExpr>,
    },
    Func {
        name: String,
        params: Vec<TyFnParam>,
        ret_ty: Ty,
        body: Box<TyIns>,
    },
    Struct {
        name: String,
        fields: Vec<TyIns>,
        funcs: Vec<TyIns>,
    },
    Block {
        code: Vec<TyIns>,
    },
    AssignTo {
        target: TyExpr,
        val: TyExpr,
    },
    ExprIns {
        expr: TyExpr,
    },
    Return {
        expr: Option<TyExpr>,
    },
    PrintIns {
        is_println: bool,
        output: TyExpr,
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
            TyIns::Struct {
                name,
                fields,
                funcs,
            } => {
                let mut buf = vec![format!("struct {}", name.as_str())];

                if !fields.is_empty() {
                    buf.push(format!(
                        "{}",
                        fields
                            .iter()
                            .map(|f| f.as_str())
                            .collect::<Vec<String>>()
                            .join("\n")
                    ))
                }
                if !funcs.is_empty() {
                    buf.push(format!(
                        "\n{}",
                        funcs
                            .iter()
                            .map(|f| f.as_str())
                            .collect::<Vec<String>>()
                            .join("\n")
                    ))
                }
                buf.join("\n")
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
            TyIns::PrintIns { is_println, output } => {
                format!(
                    "{}({})",
                    if *is_println { "println" } else { "print" },
                    output.as_str()
                )
            }
            TyIns::AssignTo { target, val } => {
                format!("{} = {};", target.as_str(), val.as_str())
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
