#![allow(unused)]
use crate::{source::source::SourceRef, types::signature::Type};

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
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
    StaticArray {
        vals: Vec<Expr>,
        loc: SourceRef,
    },
    GroupedExpr {
        inner: Box<Expr>,
        loc: SourceRef,
    },
    TernaryConditional {
        cond: Box<Expr>,
        then: Box<Expr>,
        otherwise: Box<Expr>,
        loc: SourceRef,
    },
    InterpolatedString {
        parts: Vec<Expr>,
        loc: SourceRef,
    },
    MakeSlice {
        target: Box<Expr>,
        start: Option<Box<Expr>>,
        end_excl: Option<Box<Expr>>,
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
            | Expr::Ident { loc, .. }
            | Expr::BinOp { loc, .. }
            | Expr::InitStruct { loc, .. }
            | Expr::CallFn { loc, .. }
            | Expr::UnaryOp { loc, .. }
            | Expr::StaticArray { loc, .. }
            | Expr::GroupedExpr { loc, .. }
            | Expr::TernaryConditional { loc, .. }
            | Expr::InterpolatedString { loc, .. }
            | Expr::MakeSlice { loc, .. }
            | Expr::ErrorExpr { loc, .. } => loc.clone(),
        }
    }

    pub fn is_literal(&self) -> bool {
        match self {
            Expr::Number { .. }
            | Expr::InterpolatedString { .. }
            | Expr::Str { .. }
            | Expr::Char { .. }
            | Expr::Bool { .. } => true,
            Expr::Ident { .. }
            | Expr::BinOp { .. }
            | Expr::InitStruct { .. }
            | Expr::CallFn { .. }
            | Expr::UnaryOp { .. }
            | Expr::StaticArray { .. }
            | Expr::GroupedExpr { .. }
            | Expr::TernaryConditional { .. }
            | Expr::MakeSlice { .. }
            | Expr::ErrorExpr { .. } => false,
        }
    }

    pub fn get_non_literal_ranking(&self) -> usize {
        match self {
            Expr::Ident { .. } => 4,
            Expr::CallFn { .. }
            | Expr::StaticArray { .. }
            | Expr::MakeSlice { .. }
            | Expr::InitStruct { .. }
            | Expr::GroupedExpr { .. } => 3,
            Expr::BinOp { .. } | Expr::UnaryOp { .. } | Expr::TernaryConditional { .. } => 2,
            Expr::ErrorExpr { .. } | _ => unreachable!(
                "expr::get_non_literal_ranking(): literal found: {:#?}",
                self
            ),
        }
    }

    pub fn as_str(&self) -> String {
        match self {
            Expr::Number { val, .. } => val.clone(),
            Expr::Str { val, .. } => format!("\"{val}\""),
            Expr::Char { val, .. } => format!("'{val}'"),
            Expr::Bool { val, .. } => {
                if *val {
                    "true".to_string()
                } else {
                    "false".to_string()
                }
            }
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
            Expr::StaticArray { vals, .. } => format!(
                "[{items}]",
                items = vals
                    .iter()
                    .map(|item| { item.as_str() })
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Expr::GroupedExpr { inner, .. } => format!("({})", inner.as_str()),
            Expr::TernaryConditional {
                cond,
                then,
                otherwise,
                ..
            } => format!(
                "{} ? {} : {}",
                cond.as_str(),
                then.as_str(),
                otherwise.as_str()
            ),
            Expr::InterpolatedString { parts, .. } => {
                format!(
                    "`{}`",
                    parts
                        .iter()
                        .map(|part| {
                            match part {
                                Expr::Str { val, .. } => val.clone(),
                                _ => format!("{{{}}}", part.as_str()),
                            }
                        })
                        .collect::<Vec<String>>()
                        .join("")
                )
            }
            Expr::MakeSlice {
                target,
                start,
                end_excl,
                ..
            } => match (start, end_excl) {
                (Some(start), Some(end_excl)) => {
                    format!(
                        "{}[{}:{}]",
                        target.as_str(),
                        start.as_str(),
                        end_excl.as_str()
                    )
                }
                (Some(start), None) => {
                    format!("{}[{}:]", target.as_str(), start.as_str(),)
                }
                (None, Some(end_excl)) => {
                    format!("{}[:{}]", target.as_str(), end_excl.as_str(),)
                }
                (None, None) => format!("{}[:]", target.as_str()),
            },
            Expr::ErrorExpr { msg, .. } => format!("[ErrExpr {msg}]"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FnParam {
    pub name: Expr,
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
        name: Expr,
        params: Vec<FnParam>,
        ret_type: Type,
        body: Box<Ins>,
        loc: SourceRef,
    },
    DeclStruct {
        name: Expr,
        body: Box<Ins>,
        loc: SourceRef,
    },
    DeclModule {
        name: Expr,
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
    ExprIns {
        expr: Expr,
        loc: SourceRef,
    },
    IfConditional {
        conds_and_code: Vec<(Option<Expr>, Ins)>,
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
    PrintIns {
        is_println: bool,
        output: Expr,
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
            | Ins::ExprIns { loc, .. }
            | Ins::DeclStruct { loc, .. }
            | Ins::DeclModule { loc, .. }
            | Ins::IfConditional { loc, .. }
            | Ins::PrintIns { loc, .. }
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
            Ins::SingleLineComment { comment, .. } => format!("{comment}"),
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
                        unreachable!("Ast::as_str: no initialization value or type for variable declaration.")
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
                    .map(|fn_param| {
                        format!("{} {}", fn_param.name.as_str(), fn_param.given_ty.as_str())
                    })
                    .collect();
                let params_str = params_str.join(", ");
                let mut buf = format!(
                    "fn {}({params_str}) {}\n{}",
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
            Ins::ExprIns { expr, .. } => {
                format!("{};", expr.as_str())
            }
            Ins::ErrorIns { msg, .. } => format!("[ErrIns {msg}]"),
            Ins::DeclStruct { name, body, .. } => {
                format!("struct {}\n{}\n", name.as_str(), body.as_str())
            }
            Ins::DeclModule { name, body, .. } => {
                format!("mod {}\n{}\n", name.as_str(), body.as_str())
            }
            Ins::IfConditional { conds_and_code, .. } => {
                let mut buf = String::new();
                let mut seen_if = false;
                for (cond, body) in conds_and_code.iter() {
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
            Ins::PrintIns {
                is_println,
                loc,
                output,
            } => {
                format!(
                    "{}({});",
                    if *is_println { "println" } else { "print" },
                    output.as_str()
                )
            }
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
        }

        buf
    }
}
