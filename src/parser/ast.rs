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
    Underscore {
        loc: Rc<SourceRef>,
    },
    Integer {
        loc: Rc<SourceRef>,
    },
    Decimal {
        loc: Rc<SourceRef>,
    },
    Str {
        loc: Rc<SourceRef>,
    },
    Char {
        loc: Rc<SourceRef>,
    },
    Bool {
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
    TypeAsExpr {
        ty: Rc<Ty>,
    },
    Identifier {
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
            Expr::Underscore { loc }
            | Expr::Integer { loc }
            | Expr::Decimal { loc }
            | Expr::Str { loc }
            | Expr::Char { loc }
            | Expr::Bool { loc }
            | Expr::Tuple { loc, .. }
            | Expr::Identifier { loc }
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
            Expr::TypeAsExpr { ty, .. } => ty.get_loc(),
        }
    }

    pub fn as_str(&self, src: &SourceFile) -> String {
        match self {
            Expr::Underscore { .. } => "_".to_string(),
            Expr::Integer { loc }
            | Expr::Decimal { loc }
            | Expr::Str { loc }
            | Expr::Char { loc }
            | Expr::Bool { loc }
            | Expr::Identifier { loc } => src.text[loc.flat_start..loc.flat_end].to_string(),
            Expr::Tuple { items, .. } => {
                format!(
                    "({})",
                    items
                        .iter()
                        .map(|item| { item.as_str(src) })
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Expr::TypeAsExpr { ty, .. } => ty.as_str(src),
            Expr::MakeSlice {
                target, start, end, ..
            } => {
                let start_s = match start {
                    Some(start) => start.as_str(src),
                    None => "".to_string(),
                };
                let end_s = match end {
                    Some(end) => end.as_str(src),
                    None => "".to_string(),
                };
                format!("{}[{}:{}]", target.as_str(src), start_s, end_s)
            }
            Expr::StaticArray { ty, items, .. } => {
                format!(
                    "{}{{{}}}",
                    ty.as_str(src),
                    items
                        .iter()
                        .map(|item| { item.as_str(src) })
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Expr::UnaryOp { op, expr, .. } => format!("{}{}", op.as_str(), expr.as_str(src)),
            Expr::BinOp {
                op,
                left,
                right,
                loc,
            } => format!(
                "[{} {} {}]",
                left.as_str(src),
                op.as_str(),
                right.as_str(src)
            ),
            Expr::ConditionalExpr {
                cond,
                then,
                otherwise,
                ..
            } => {
                format!(
                    "{} ? {} : {}",
                    cond.as_str(src),
                    then.as_str(src),
                    otherwise.as_str(src)
                )
            }
            Expr::CallFn { func, args, .. } => {
                let args_str: Vec<String> = args.iter().map(|arg| arg.as_str(src)).collect();
                let args_str = args_str.join(", ");
                format!("{}({args_str})", func.as_str(src))
            }
            Expr::GroupedExpr { inner, .. } => format!("({})", inner.as_str(src)),
            Expr::IndexInto { target, index, .. } => {
                format!("{}[{}]", target.as_str(src), index.as_str(src))
            }
            Expr::AccessMember { target, mem, loc } => {
                format!("{}.{}", target.as_str(src), mem.as_str(src))
            }
            Expr::ComptimeExpr { val, .. } => {
                format!("comptime {}", val.as_str(src))
            }
            Expr::OptionalExpr { val, .. } => match val {
                Some(v) => format!("some {}", v.as_str(src)),
                None => format!("none"),
            },
            Expr::InitializerList { target, pairs, .. } => {
                let t_str = match target {
                    Some(targ) => targ.as_str(src),
                    None => ".".to_string(),
                };
                let p_str = pairs
                    .iter()
                    .map(|(k, v)| {
                        if let Some(val) = v {
                            format!("{} = {}", k.as_str(src), val.as_str(src))
                        } else {
                            k.as_str(src)
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
                            fn_param.name.as_str(src),
                            fn_param.given_ty.as_str(src)
                        )
                    })
                    .collect();
                let params_str = params_str.join(", ");
                format!(
                    "\\({}) {}\n{}",
                    params_str,
                    ret_type.as_str(src),
                    body.as_str(src)
                )
            }
            Expr::DerefPtr { target, .. } => format!("*{}", target.as_str(src)),
            Expr::MakePtrFromAddrOf { target, .. } => format!("&{}", target.as_str(src)),
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
pub enum Ins {
    DeclVariable {
        name: Expr,
        ty: Option<Rc<Ty>>, // might be provided, or not
        init_val: Expr,
        is_mutable: bool,
        loc: Rc<SourceRef>,
    },
    PubDecl {
        ins: Rc<Ins>,
        loc: Rc<SourceRef>,
    },
    DeclFunc {
        name: Expr,
        params: Vec<FnParam>,
        ret_ty: Rc<Ty>,
        body: Rc<Ins>,
        loc: Rc<SourceRef>,
    },
    DeclTypeAlias {
        name: Expr,
        ty: Rc<Ty>,
        loc: Rc<SourceRef>,
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
            | Ins::DeclTypeAlias { name, .. } => Some(name.as_str(src)),
            Ins::PubDecl { ins, .. } => ins.get_id(src),
            _ => None,
        }
    }

    pub fn get_source_ref(&self) -> Rc<SourceRef> {
        match self {
            Ins::DeclVariable { loc, .. }
            | Ins::DeclFunc { loc, .. }
            | Ins::PubDecl { loc, .. }
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

    pub fn as_str(&self, src: &SourceFile) -> String {
        match self {
            Ins::DeclVariable {
                name,
                ty,
                init_val,
                is_mutable,
                ..
            } => {
                if let Some(ty) = ty {
                    format!(
                        "{} {} {} = {}",
                        if *is_mutable { "var" } else { "const" },
                        name.as_str(src),
                        ty.as_str(src),
                        init_val.as_str(src)
                    )
                } else {
                    format!(
                        "{} {} = {}",
                        if *is_mutable { "var" } else { "const" },
                        name.as_str(src),
                        init_val.as_str(src)
                    )
                }
            }
            Ins::PubDecl { ins, .. } => {
                format!("pub {}", ins.as_str(src))
            }
            Ins::DeclTypeAlias { name, ty, .. } => {
                format!("type {} = {}", name.as_str(src), ty.as_str(src))
            }
            Ins::DeclFunc {
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
                            } else {
                                ""
                            },
                            fn_param.name.as_str(src),
                            fn_param.given_ty.as_str(src)
                        )
                    })
                    .collect();
                let params_str = params_str.join(", ");
                format!(
                    "fn {}({params_str}) {}\n{}",
                    name.as_str(src),
                    ret_ty.as_str(src),
                    body.as_str(src)
                )
            }
            Ins::SingleLineComment { loc } => src.text[loc.flat_start..loc.flat_end].to_string(),
            Ins::Block { code, .. } => {
                let mut buf = String::new();
                for instruc in code {
                    buf.push_str(&(instruc.as_str(src) + "\n"));
                }
                format!("{{\n{buf}}}")
            }
            Ins::AssignTo { target, value, .. } => {
                format!("{} = {}", target.as_str(src), value.as_str(src))
            }
            Ins::Return { expr, .. } => match expr {
                Some(val) => {
                    format!("return {}", val.as_str(src))
                }
                None => {
                    format!("return")
                }
            },
            Ins::ExprIns { expr, .. } => {
                format!("{}", expr.as_str(src))
            }
            Ins::ErrorIns { .. } => format!("[ErrIns]"),
            Ins::IfConditional { conds_and_code, .. } => {
                let mut buf = String::new();
                let mut seen_if = false;
                for (cond, body) in conds_and_code.iter() {
                    match cond {
                        Some(cond) => {
                            if !seen_if {
                                buf.push_str(&format!(
                                    "if {}:\n{}",
                                    cond.as_str(src),
                                    body.as_str(src)
                                ));
                                seen_if = true;
                            } else {
                                buf.push_str(&format!(
                                    "else if {}:\n{}",
                                    cond.as_str(src),
                                    body.as_str(src)
                                ));
                            }
                        }
                        None => buf.push_str(&format!("else:\n{}", body.as_str(src))),
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
                    "{}({})",
                    if *is_println { "println" } else { "print" },
                    output.as_str(src)
                )
            }
            Ins::Defer { sub_ins, loc } => {
                format!("defer {}", sub_ins.as_str(src))
            }
            Ins::ForInLoop {
                loop_var,
                loop_target,
                block,
                loc,
            } => {
                format!(
                    "for {} in {}\n{}",
                    loop_var.as_str(src),
                    loop_target.as_str(src),
                    block.as_str(src)
                )
            }
            Ins::Break { loc } => "break;".into(),
            Ins::Continue { loc } => "continue;".into(),
            Ins::InfiniteLoop { block, .. } => {
                format!("for\n{}", block.as_str(src))
            }
            Ins::WhileLoop {
                cond,
                block,
                post_code,
                ..
            } => {
                let post_code = if let Some(pcode) = post_code {
                    format!(" : ({})", pcode.as_str(src))
                } else {
                    "".into()
                };
                format!(
                    "for {}{}\n{}",
                    cond.as_str(src),
                    post_code,
                    block.as_str(src)
                )
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
                    init.as_str(src),
                    loop_cond.as_str(src),
                    update.as_str(src),
                    block.as_str(src)
                )
            }
        }
    }
}
