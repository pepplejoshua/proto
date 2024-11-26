#![allow(unused)]
use std::rc::Rc;

use crate::{
    parser::type_signature::Ty,
    source::source::{SourceFile, SourceRef},
};

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
    Integer {
        content: Rc<String>,
        loc: Rc<SourceRef>,
    },
    Float {
        content: Rc<String>,
        loc: Rc<SourceRef>,
    },
    Str {
        content: Rc<String>,
        loc: Rc<SourceRef>,
    },
    Char {
        content: Rc<String>,
        loc: Rc<SourceRef>,
    },
    Bool {
        val: bool,
        loc: Rc<SourceRef>,
    },
    Tuple {
        items: Vec<Expr>,
        loc: Rc<SourceRef>,
    },
    StaticArray {
        ty: Rc<Ty>,
        items: Vec<Expr>,
        loc: Rc<SourceRef>,
    },
    Identifier {
        name: Rc<String>,
        loc: Rc<SourceRef>,
    },
    UnaryOp {
        op: UnaryOpType,
        expr: Box<Expr>,
        loc: Rc<SourceRef>,
    },
    BinOp {
        op: BinOpType,
        left: Box<Expr>,
        right: Box<Expr>,
        loc: Rc<SourceRef>,
    },
    ConditionalExpr {
        cond: Box<Expr>,
        then: Box<Expr>,
        otherwise: Box<Expr>,
        loc: Rc<SourceRef>,
    },
    CallFn {
        func: Box<Expr>,
        args: Vec<Expr>,
        loc: Rc<SourceRef>,
    },
    GroupedExpr {
        inner: Box<Expr>,
        loc: Rc<SourceRef>,
    },
    IndexInto {
        target: Box<Expr>,
        index: Box<Expr>,
        loc: Rc<SourceRef>,
    },
    MakeSlice {
        target: Box<Expr>,
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
        loc: Rc<SourceRef>,
    },
    AccessMember {
        target: Box<Expr>,
        mem: Box<Expr>,
        loc: Rc<SourceRef>,
    },
    OptionalExpr {
        val: Option<Box<Expr>>,
        loc: Rc<SourceRef>,
    },
    ComptimeExpr {
        val: Box<Expr>,
        loc: Rc<SourceRef>,
    },
    Lambda {
        params: Vec<FnParam>,
        ret_type: Rc<Ty>,
        body: Box<Ins>,
        loc: Rc<SourceRef>,
    },
    DerefPtr {
        target: Box<Expr>,
        loc: Rc<SourceRef>,
    },
    MakePtrFromAddrOf {
        target: Box<Expr>,
        loc: Rc<SourceRef>,
    },
    InitializerList {
        target: Option<Rc<Expr>>,
        pairs: Vec<(Box<Expr>, Option<Box<Expr>>)>,
        loc: Rc<SourceRef>,
    },
    ErrorExpr {
        loc: Rc<SourceRef>,
    },
}

impl Expr {
    pub fn get_source_ref(&self) -> Rc<SourceRef> {
        match self {
            Expr::Integer { loc, .. }
            | Expr::Float { loc, .. }
            | Expr::Str { loc, .. }
            | Expr::Char { loc, .. }
            | Expr::Bool { loc, .. }
            | Expr::Tuple { loc, .. }
            | Expr::Identifier { loc, .. }
            | Expr::UnaryOp { loc, .. }
            | Expr::BinOp { loc, .. }
            | Expr::ConditionalExpr { loc, .. }
            | Expr::CallFn { loc, .. }
            | Expr::GroupedExpr { loc, .. }
            | Expr::MakeSlice { loc, .. }
            | Expr::IndexInto { loc, .. }
            | Expr::AccessMember { loc, .. }
            | Expr::ComptimeExpr { loc, .. }
            | Expr::OptionalExpr { loc, .. }
            | Expr::Lambda { loc, .. }
            | Expr::MakePtrFromAddrOf { loc, .. }
            | Expr::DerefPtr { loc, .. }
            | Expr::StaticArray { loc, .. }
            | Expr::InitializerList { loc, .. }
            | Expr::ErrorExpr { loc, .. } => loc.clone(),
        }
    }

    pub fn as_str(&self) -> String {
        match self {
            Expr::Identifier { name, .. } => name.as_ref().clone(),
            Expr::Str { content, .. }
            | Expr::Char { content, .. }
            | Expr::Integer { content, .. }
            | Expr::Float { content, .. } => content.as_ref().clone(),
            Expr::Bool { val, .. } => {
                if *val {
                    "true".to_string()
                } else {
                    "false".to_string()
                }
            }
            Expr::Tuple { items, .. } => {
                format!(
                    "({})",
                    items
                        .iter()
                        .map(|item| { item.as_str() })
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Expr::MakeSlice {
                target, start, end, ..
            } => {
                let start_s = match start {
                    Some(start) => start.as_str(),
                    None => "".to_string(),
                };
                let end_s = match end {
                    Some(end) => end.as_str(),
                    None => "".to_string(),
                };
                format!("{}[{}:{}]", target.as_str(), start_s, end_s)
            }
            Expr::StaticArray { ty, items, .. } => {
                format!(
                    "{}{{{}}}",
                    ty.as_str(),
                    items
                        .iter()
                        .map(|item| { item.as_str() })
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Expr::UnaryOp { op, expr, .. } => format!("{}{}", op.as_str(), expr.as_str()),
            Expr::BinOp {
                op, left, right, ..
            } => format!("[{} {} {}]", left.as_str(), op.as_str(), right.as_str()),
            Expr::ConditionalExpr {
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
            Expr::CallFn { func, args, .. } => {
                let args_str: Vec<String> = args.iter().map(|arg| arg.as_str()).collect();
                let args_str = args_str.join(", ");
                format!("{}({args_str})", func.as_str())
            }
            Expr::GroupedExpr { inner, .. } => format!("({})", inner.as_str()),
            Expr::IndexInto { target, index, .. } => {
                format!("{}[{}]", target.as_str(), index.as_str())
            }
            Expr::AccessMember { target, mem, loc } => {
                format!("{}.{}", target.as_str(), mem.as_str())
            }
            Expr::ComptimeExpr { val, .. } => {
                format!("comptime {}", val.as_str())
            }
            Expr::OptionalExpr { val, .. } => match val {
                Some(v) => format!("some {}", v.as_str()),
                None => format!("none"),
            },
            Expr::InitializerList { target, pairs, .. } => {
                let t_str = match target {
                    Some(targ) => targ.as_str(),
                    None => ".".to_string(),
                };
                let p_str = pairs
                    .iter()
                    .map(|(k, v)| {
                        if let Some(val) = v {
                            format!("{} = {}", k.as_str(), val.as_str())
                        } else {
                            k.as_str()
                        }
                    })
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("{}{{{}}}", t_str, p_str)
            }
            Expr::Lambda {
                params,
                ret_type,
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
                    "\\({}) {}\n{}",
                    params_str,
                    ret_type.as_str(),
                    body.as_str()
                )
            }
            Expr::DerefPtr { target, .. } => format!("*{}", target.as_str()),
            Expr::MakePtrFromAddrOf { target, .. } => format!("&{}", target.as_str()),
            Expr::ErrorExpr { .. } => format!("[ErrExpr]"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FnParam {
    pub name: Expr,
    pub given_ty: Rc<Ty>,
    pub is_mutable: bool,
    pub is_self: bool,
    pub is_comptime: bool,
    pub loc: Rc<SourceRef>,
}

#[derive(Debug, Clone)]
pub enum ModulePathPart {
    Root,
    Parent,
    Name(String),
}

#[derive(Debug, Clone)]
pub struct ModulePath {
    parts: Vec<ModulePathPart>,
    loc: Rc<SourceRef>,
}

#[derive(Debug, Clone)]
pub struct UseDecl {
    path: ModulePath,
    alias: Option<String>,
    loc: Rc<SourceRef>,
}

#[derive(Debug, Clone)]
pub enum Ins {
    DeclModule {
        name: ModulePath,
        loc: Rc<SourceRef>,
    },
    DeclVariable {
        name: Expr,
        ty: Option<Rc<Ty>>, // might be provided, or not
        init_val: Option<Expr>,
        is_mutable: bool,
        loc: Rc<SourceRef>,
        is_public: bool,
    },
    DeclFunc {
        name: Expr,
        params: Vec<FnParam>,
        ret_ty: Rc<Ty>,
        body: Rc<Ins>,
        loc: Rc<SourceRef>,
        is_public: bool,
    },
    DeclTypeAlias {
        name: Expr,
        ty: Rc<Ty>,
        loc: Rc<SourceRef>,
        is_public: bool,
    },
    Defer {
        sub_ins: Box<Ins>,
        loc: Rc<SourceRef>,
    },
    Block {
        code: Vec<Ins>,
        loc: Rc<SourceRef>,
    },
    AssignTo {
        target: Expr,
        value: Expr,
        loc: Rc<SourceRef>,
    },
    ExprIns {
        expr: Expr,
        loc: Rc<SourceRef>,
    },
    IfConditional {
        conds_and_code: Vec<(Option<Expr>, Ins)>,
        loc: Rc<SourceRef>,
    },
    Return {
        expr: Option<Expr>,
        loc: Rc<SourceRef>,
    },
    SingleLineComment {
        content: Rc<String>,
        loc: Rc<SourceRef>,
    },
    PrintIns {
        is_println: bool,
        output: Expr,
        loc: Rc<SourceRef>,
    },
    Break {
        loc: Rc<SourceRef>,
    },
    Continue {
        loc: Rc<SourceRef>,
    },
    ForInLoop {
        loop_var: Expr,
        loop_target: Expr,
        block: Box<Ins>,
        loc: Rc<SourceRef>,
    },
    InfiniteLoop {
        block: Box<Ins>,
        loc: Rc<SourceRef>,
    },
    WhileLoop {
        cond: Expr,
        post_code: Option<Box<Ins>>,
        block: Box<Ins>,
        loc: Rc<SourceRef>,
    },
    RegLoop {
        init: Box<Ins>,
        loop_cond: Expr,
        update: Box<Ins>,
        block: Box<Ins>,
        loc: Rc<SourceRef>,
    },
    ErrorIns {
        loc: Rc<SourceRef>,
    },
}

impl Ins {
    pub fn get_id(&self, src: &SourceFile) -> Option<String> {
        match self {
            Ins::DeclVariable { name, .. }
            | Ins::DeclFunc { name, .. }
            | Ins::DeclTypeAlias { name, .. } => Some(name.as_str()),
            _ => None,
        }
    }

    pub fn get_source_ref(&self) -> Rc<SourceRef> {
        match self {
            Ins::DeclVariable { loc, .. }
            | Ins::DeclFunc { loc, .. }
            | Ins::DeclTypeAlias { loc, .. }
            | Ins::Block { loc, .. }
            | Ins::AssignTo { loc, .. }
            | Ins::Return { loc, .. }
            | Ins::SingleLineComment { loc, .. }
            | Ins::ExprIns { loc, .. }
            | Ins::IfConditional { loc, .. }
            | Ins::PrintIns { loc, .. }
            | Ins::Defer { loc, .. }
            | Ins::ForInLoop { loc, .. }
            | Ins::InfiniteLoop { loc, .. }
            | Ins::WhileLoop { loc, .. }
            | Ins::RegLoop { loc, .. }
            | Ins::Break { loc }
            | Ins::Continue { loc }
            | Ins::ErrorIns { loc, .. } => loc.clone(),
        }
    }

    pub fn make_public(&mut self) -> bool {
        match self {
            Ins::DeclVariable { is_public, .. }
            | Ins::DeclFunc { is_public, .. }
            | Ins::DeclTypeAlias { is_public, .. } => {
                *is_public = true;
                true
            }
            _ => false,
        }
    }

    pub fn as_str(&self) -> String {
        match self {
            Ins::DeclVariable {
                name,
                ty,
                init_val,
                is_mutable,
                is_public,
                ..
            } => {
                let public = if *is_public { "pub " } else { "" };
                let mutable = if *is_mutable { "var" } else { "const" };
                let init_val = if let Some(val) = init_val {
                    val.as_str()
                } else {
                    "_".into()
                };
                if let Some(ty) = ty {
                    format!(
                        "{public}{mutable} {} {} = {init_val}",
                        name.as_str(),
                        ty.as_str(),
                    )
                } else {
                    format!("{public}{mutable} {} = {init_val}", name.as_str(),)
                }
            }
            Ins::DeclTypeAlias {
                name,
                ty,
                is_public,
                ..
            } => {
                format!(
                    "{}type {} = {}",
                    if *is_public { "pub " } else { "" },
                    name.as_str(),
                    ty.as_str()
                )
            }
            Ins::DeclFunc {
                name,
                params,
                ret_ty,
                body,
                is_public,
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
                    "{}fn {}({params_str}) {}\n{}",
                    if *is_public { "pub " } else { "" },
                    name.as_str(),
                    ret_ty.as_str(),
                    body.as_str()
                )
            }
            Ins::SingleLineComment { content, .. } => content.as_ref().clone(),
            Ins::Block { code, .. } => {
                let mut buf = String::new();
                for instruc in code {
                    buf.push_str(&(instruc.as_str() + "\n"));
                }
                format!("{{\n{buf}}}")
            }
            Ins::AssignTo { target, value, .. } => {
                format!("{} = {}", target.as_str(), value.as_str())
            }
            Ins::Return { expr, .. } => match expr {
                Some(val) => {
                    format!("return {}", val.as_str())
                }
                None => {
                    format!("return")
                }
            },
            Ins::ExprIns { expr, .. } => {
                format!("{}", expr.as_str())
            }
            Ins::ErrorIns { .. } => format!("[ErrIns]"),
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
            Ins::PrintIns {
                is_println, output, ..
            } => {
                format!(
                    "{}({})",
                    if *is_println { "println" } else { "print" },
                    output.as_str()
                )
            }
            Ins::Defer { sub_ins, .. } => {
                format!("defer {}", sub_ins.as_str())
            }
            Ins::ForInLoop {
                loop_var,
                loop_target,
                block,
                ..
            } => {
                format!(
                    "for {} in {}\n{}",
                    loop_var.as_str(),
                    loop_target.as_str(),
                    block.as_str()
                )
            }
            Ins::Break { .. } => "break;".into(),
            Ins::Continue { .. } => "continue;".into(),
            Ins::InfiniteLoop { block, .. } => {
                format!("for\n{}", block.as_str())
            }
            Ins::WhileLoop {
                cond,
                block,
                post_code,
                ..
            } => {
                let post_code = if let Some(pcode) = post_code {
                    format!(" : ({})", pcode.as_str())
                } else {
                    "".into()
                };
                format!("for {}{}\n{}", cond.as_str(), post_code, block.as_str())
            }
            Ins::RegLoop {
                init,
                loop_cond,
                update,
                block,
                ..
            } => {
                format!(
                    "for ({}; {}; {})\n{}",
                    init.as_str(),
                    loop_cond.as_str(),
                    update.as_str(),
                    block.as_str()
                )
            }
        }
    }
}
