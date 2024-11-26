#![allow(unused)]

use std::rc::Rc;

use crate::{
    parser::{
        ast::{BinOpType, FnParam, UnaryOpType},
        type_signature::Ty,
    },
    source::source::SourceRef,
};

pub enum SignedInteger {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
}

impl SignedInteger {
    pub fn as_str(&self) -> String {
        match self {
            SignedInteger::I8(val) => format!("{val}"),
            SignedInteger::I16(val) => format!("{val}"),
            SignedInteger::I32(val) => format!("{val}"),
            SignedInteger::I64(val) => format!("{val}"),
        }
    }
}

pub enum UnsignedInteger {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
}

impl UnsignedInteger {
    pub fn as_str(&self) -> String {
        match self {
            UnsignedInteger::U8(val) => format!("{val}"),
            UnsignedInteger::U16(val) => format!("{val}"),
            UnsignedInteger::U32(val) => format!("{val}"),
            UnsignedInteger::U64(val) => format!("{val}"),
        }
    }
}

pub enum TypedExpr {
    UntypedInt {
        value: usize,
        ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    SignedInt {
        value: SignedInteger,
        ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    UnsignedInt {
        value: UnsignedInteger,
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
    UnaryOp {
        op: UnaryOpType,
        expr: Rc<TypedExpr>,
        ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    BinOp {
        op: BinOpType,
        left: Rc<TypedExpr>,
        right: Rc<TypedExpr>,
        ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    ConditionalExpr {
        cond: Rc<TypedExpr>,
        then: Rc<TypedExpr>,
        otherwise: Rc<TypedExpr>,
        ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    GroupedExpr {
        inner: Rc<TypedExpr>,
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
    Error {
        loc: Rc<SourceRef>,
    },
}

impl TypedExpr {
    pub fn is_error_expr(&self) -> bool {
        match self {
            Self::Error { .. } => true,
            _ => false,
        }
    }

    pub fn get_ty(&self) -> Rc<Ty> {
        match self {
            TypedExpr::UntypedInt { ty, .. }
            | TypedExpr::SignedInt { ty, .. }
            | TypedExpr::UnsignedInt { ty, .. }
            | TypedExpr::Float { ty, .. }
            | TypedExpr::CallFn { ty, .. }
            | TypedExpr::UnaryOp { ty, .. }
            | TypedExpr::BinOp { ty, .. }
            | TypedExpr::ConditionalExpr { ty, .. }
            | TypedExpr::Identifier { ty, .. }
            | TypedExpr::Bool { ty, .. }
            | TypedExpr::Str { ty, .. }
            | TypedExpr::Char { ty, .. }
            | TypedExpr::Optional { ty, .. } => ty.clone(),
            TypedExpr::Error { loc } => Rc::new(Ty::ErrorType { loc: loc.clone() }),
            TypedExpr::GroupedExpr { inner, .. } => inner.get_ty(),
        }
    }

    pub fn as_str(&self) -> String {
        match self {
            TypedExpr::UntypedInt { value, .. } => {
                format!("{value}")
            }
            TypedExpr::SignedInt { value, .. } => {
                format!("{}", value.as_str())
            }
            TypedExpr::UnsignedInt { value, .. } => {
                format!("{}", value.as_str())
            }
            TypedExpr::Float { value, .. } => {
                format!("{value}")
            }
            TypedExpr::CallFn { func, args, .. } => {
                let args_str: Vec<String> = args.iter().map(|arg| arg.as_str()).collect();
                let args_str = args_str.join(", ");
                format!("{}({args_str})", func.as_str())
            }
            TypedExpr::UnaryOp { op, expr, .. } => format!("{}{}", op.as_str(), expr.as_str()),
            TypedExpr::BinOp {
                op, left, right, ..
            } => format!("[{} {} {}]", left.as_str(), op.as_str(), right.as_str()),
            TypedExpr::ConditionalExpr {
                cond,
                then,
                otherwise,
                ..
            } => {
                format!(
                    "{} ? {} : {}",
                    cond.as_str(),
                    then.as_str(),
                    otherwise.as_str()
                )
            }
            TypedExpr::GroupedExpr { inner, .. } => format!("({})", inner.as_str()),
            TypedExpr::Identifier { name, .. } => format!("{name}"),
            TypedExpr::Bool { value, .. } => format!("{value}"),
            TypedExpr::Str { value, .. } => format!("\"{value}\""),
            TypedExpr::Char { value, .. } => format!("'{value}'"),
            TypedExpr::Optional { value, .. } => {
                if let Some(value) = value {
                    format!("some {}", value.as_str())
                } else {
                    "none".into()
                }
            }
            TypedExpr::Error { .. } => format!("ErrorExpr"),
        }
    }
}

pub enum TypedIns {
    SingleLineComment {
        content: Rc<String>,
        loc: Rc<SourceRef>,
    },
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
    AssignTo {
        target: TypedExpr,
        value: TypedExpr,
        loc: Rc<SourceRef>,
    },
    ExprIns {
        expr: TypedExpr,
        loc: Rc<SourceRef>,
    },
    IfConditional {
        conds_and_code: Vec<(Option<TypedExpr>, TypedIns)>,
        loc: Rc<SourceRef>,
    },
    Block {
        code: Vec<TypedIns>,
        loc: Rc<SourceRef>,
    },
    Return {
        expr: Option<TypedExpr>,
        loc: Rc<SourceRef>,
    },
    Error,
}

impl TypedIns {
    pub fn as_str(&self) -> String {
        match self {
            TypedIns::SingleLineComment { content, .. } => content.to_string(),
            TypedIns::ExprIns { expr, .. } => {
                format!("{}", expr.as_str())
            }
            TypedIns::DeclFunc {
                name,
                params,
                ret_ty,
                body,
                ..
            } => {
                let params_str: Vec<String> = params
                    .iter()
                    .map(|fn_param| {
                        format!(
                            "{}{} {}",
                            if fn_param.is_comptime {
                                "comptime "
                            } else if fn_param.is_mutable {
                                "var "
                            } else {
                                ""
                            },
                            fn_param.name.as_str(),
                            fn_param.given_ty.as_str()
                        )
                    })
                    .collect();
                let params_str = params_str.join(", ");

                format!(
                    "fn {name}({params_str}) {}\n{}",
                    ret_ty.as_str(),
                    body.as_str()
                )
            }
            TypedIns::DeclVariable {
                name,
                ty,
                init_value,
                is_mutable,
                ..
            } => {
                let mutable = if *is_mutable { "var" } else { "const" };
                let init_val = init_value.as_str();
                let ty_str = ty.as_str();
                format!("{mutable} {name} {ty_str} = {init_val}")
            }
            TypedIns::AssignTo { target, value, .. } => {
                format!("{} = {}", target.as_str(), value.as_str())
            }
            TypedIns::IfConditional { conds_and_code, .. } => {
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
                                    "\nelse if {}:\n{}",
                                    cond.as_str(),
                                    body.as_str()
                                ));
                            }
                        }
                        None => buf.push_str(&format!("\nelse:\n{}", body.as_str())),
                    }
                }
                buf
            }
            TypedIns::Block { code, .. } => code
                .iter()
                .map(|ins| ins.as_str())
                .collect::<Vec<String>>()
                .join("\n"),
            TypedIns::Return { expr, .. } => match expr {
                Some(val) => format!("return {}", val.as_str()),
                None => "return".into(),
            },
            TypedIns::Error => "ErrorIns".into(),
        }
    }
}
