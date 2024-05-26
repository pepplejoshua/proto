#![allow(unused)]
use crate::{source::source::SourceRef, types::signature::Type};

#[derive(Debug, Clone)]
pub enum BinOpType {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    And,
    Or,
    Eq,
    Neq,
    Gt,
    Lt,
    GtEq,
    LtEq,
    AccessMember,
    IndexArray,
}

impl BinOpType {
    pub fn as_str(&self) -> String {
        let op_str = match self {
            BinOpType::Add => "+",
            BinOpType::Sub => "-",
            BinOpType::Mult => "*",
            BinOpType::Div => "/",
            BinOpType::Mod => "%",
            BinOpType::And => "&&",
            BinOpType::Or => "||",
            BinOpType::Eq => "==",
            BinOpType::Neq => "!=",
            BinOpType::Gt => ">",
            BinOpType::Lt => "<",
            BinOpType::GtEq => ">=",
            BinOpType::LtEq => "<=",
            BinOpType::AccessMember => ".",
            BinOpType::IndexArray => "[]",
        };
        op_str.to_string()
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOpType {
    Not,
    Negate,
}

impl UnaryOpType {
    pub fn as_str(&self) -> String {
        let op_str = match self {
            UnaryOpType::Not => "!",
            UnaryOpType::Negate => "-",
        };
        op_str.to_string()
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number {
        val: String,
        loc: SourceRef,
    },
    Str {
        val: String,
        loc: SourceRef,
    },
    Char {
        val: char,
        loc: SourceRef,
    },
    Bool {
        val: bool,
        loc: SourceRef,
    },
    Void {
        loc: SourceRef,
    },
    Ident {
        name: String,
        loc: SourceRef,
    },
    BinOp {
        op: BinOpType,
        left: Box<Expr>,
        right: Box<Expr>,
        loc: SourceRef,
    },
    InitStruct {
        struct_name: Box<Expr>,
        fields: Vec<(Expr, Expr)>,
        loc: SourceRef,
    },
    CallFn {
        func: Box<Expr>,
        args: Vec<Expr>,
        loc: SourceRef,
    },
    UnaryOp {
        op: UnaryOpType,
        expr: Box<Expr>,
        loc: SourceRef,
    },
    ErrorExpr {
        msg: String,
        loc: SourceRef,
    },
}

impl Expr {
    pub fn get_source_ref(&self) -> SourceRef {
        match self {
            Expr::Number { loc, .. }
            | Expr::Str { loc, .. }
            | Expr::Char { loc, .. }
            | Expr::Bool { loc, .. }
            | Expr::Void { loc }
            | Expr::Ident { loc, .. }
            | Expr::BinOp { loc, .. }
            | Expr::InitStruct { loc, .. }
            | Expr::CallFn { loc, .. }
            | Expr::UnaryOp { loc, .. }
            | Expr::ErrorExpr { loc, .. } => loc.clone(),
        }
    }

    pub fn as_str(&self) -> String {
        match self {
            Expr::Number { val, .. } | Expr::Str { val, .. } => val.clone(),
            Expr::Char { val, .. } => val.to_string(),
            Expr::Bool { val, .. } => {
                if *val {
                    "true".to_string()
                } else {
                    "false".to_string()
                }
            }
            Expr::Void { .. } => "void".to_string(),
            Expr::Ident { name, .. } => name.clone(),
            Expr::BinOp {
                op,
                left,
                right,
                loc,
            } => format!("[{} {} {}]", left.as_str(), op.as_str(), right.as_str()),
            Expr::InitStruct {
                struct_name,
                fields,
                loc,
            } => {
                let fields_str: Vec<String> = fields
                    .iter()
                    .map(|(name, init_expr)| format!("{}: {}", name.as_str(), init_expr.as_str()))
                    .collect();
                let fields_str = fields_str.join(", ");
                format!("{}.({fields_str})", struct_name.as_str())
            }
            Expr::CallFn { func, args, .. } => {
                let args_str: Vec<String> = args.iter().map(|arg| arg.as_str()).collect();
                let args_str = args_str.join(", ");
                format!("{}({args_str})", func.as_str())
            }
            Expr::UnaryOp { op, expr, .. } => format!("{}{}", op.as_str(), expr.as_str()),
            Expr::ErrorExpr { msg, .. } => format!("[ErrExpr {msg}]"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FnParam {
    pub name: String,
    pub given_ty: Type,
    pub loc: SourceRef,
}

#[derive(Debug, Clone)]
pub enum Ins {
    DeclConst {
        name: Expr,
        ty: Option<Type>, // might be provided, or not
        init_val: Expr,
        loc: SourceRef,
    },
    DeclVar {
        name: Expr,
        ty: Option<Type>,
        init_val: Option<Expr>,
        loc: SourceRef,
    },
    DeclFunc {
        name: String,
        params: Vec<FnParam>,
        ret_type: Type,
        body: Box<Ins>,
        loc: SourceRef,
    },
    Block {
        code: Vec<Ins>,
        loc: SourceRef,
    },
    AssignTo {
        target: Expr,
        value: Expr,
        loc: SourceRef,
    },
    Return {
        expr: Option<Expr>,
        loc: SourceRef,
    },
    SingleLineComment {
        comment: String,
        loc: SourceRef,
    },
    ErrorIns {
        msg: String,
        loc: SourceRef,
    },
}

impl Ins {
    pub fn get_source_ref(&self) -> SourceRef {
        match self {
            Ins::DeclConst { loc, .. }
            | Ins::DeclVar { loc, .. }
            | Ins::Block { loc, .. }
            | Ins::AssignTo { loc, .. }
            | Ins::Return { loc, .. }
            | Ins::DeclFunc { loc, .. }
            | Ins::SingleLineComment { loc, .. }
            | Ins::ErrorIns { loc, .. } => loc.clone(),
        }
    }

    pub fn as_str(&self) -> String {
        match self {
            Ins::DeclConst {
                name, ty, init_val, ..
            } => {
                if let Some(ty) = ty {
                    format!(
                        "{} : {} : {};",
                        name.as_str(),
                        ty.as_str(),
                        init_val.as_str()
                    )
                } else {
                    format!("{} :: {};", name.as_str(), init_val.as_str())
                }
            }
            Ins::SingleLineComment { comment, .. } => format!("//{comment}"),
            Ins::DeclVar {
                name, ty, init_val, ..
            } => match init_val {
                Some(init_v) => match ty {
                    Some(tyv) => {
                        format!(
                            "{} : {} = {};",
                            name.as_str(),
                            tyv.as_str(),
                            init_v.as_str()
                        )
                    }
                    None => {
                        format!("{} := {};", name.as_str(), init_v.as_str())
                    }
                },
                None => match ty {
                    Some(tyv) => {
                        format!("{} : {}", name.as_str(), tyv.as_str())
                    }
                    None => {
                        unreachable!("ast.rs::[NO TYPE OR INIT VALUE FOR VARIABLE]")
                    }
                },
            },
            Ins::DeclFunc {
                name,
                params,
                ret_type,
                body,
                ..
            } => {
                let params_str: Vec<String> = params
                    .iter()
                    .map(|fn_param| format!("{} {}", fn_param.name, fn_param.given_ty.as_str()))
                    .collect();
                let params_str = params_str.join(", ");
                let mut buf = format!(
                    "fn {}({params_str}) {}\n{}\n",
                    name.as_str(),
                    ret_type.as_str(),
                    body.as_str()
                );
                buf
            }
            Ins::Block { code, .. } => {
                let mut buf = String::new();
                for instruc in code {
                    buf.push_str(&(instruc.as_str() + "\n"));
                }
                buf
            }
            Ins::AssignTo { target, value, .. } => {
                format!("{} = {};", target.as_str(), value.as_str())
            }
            Ins::Return { expr, .. } => match expr {
                Some(val) => {
                    format!("return {};", val.as_str())
                }
                None => {
                    format!("return;")
                }
            },
            Ins::ErrorIns { msg, loc } => format!("[ErrIns {msg}]"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FileModule {
    pub top_level: Vec<Ins>,
}

impl FileModule {
    pub fn new() -> Self {
        FileModule {
            top_level: Vec::new(),
        }
    }

    pub fn add_top_level_ins(&mut self, ins: Ins) {
        self.top_level.push(ins);
    }

    pub fn as_str(&self) -> String {
        let mut buf = String::new();
        for tl_ins in self.top_level.iter() {
            buf.push_str(&tl_ins.as_str());
            buf.push('\n');
        }

        buf
    }
}