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

#[derive(Debug, Clone)]
pub enum UnaryOpType {
    Not,
    Negate,
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
    ErrorNode {
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
            | Expr::ErrorNode { loc, .. } => loc.clone(),
        }
    }
}

pub struct FnParam {
    pub name: String,
    pub given_ty: Type,
    pub loc: SourceRef,
}

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
}
