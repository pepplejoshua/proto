#![allow(dead_code)]
#![allow(unused_variables)]

use crate::{source::source::SourceRef, types::signature::Type};

// ExprLoc will track the location of an expression in the list of expressions
// for the file
pub type ExprLoc = usize;

pub enum Expr {
    // literals
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

    // operators
    Add {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
    },
    Sub {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
    },
    Mul {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
    },
    Div {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
    },
    Mod {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
    },
    Pow {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
    },
    And {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
    },
    Or {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
    },
    Not {
        expr: ExprLoc,
        loc: SourceRef,
    },
    Negate {
        expr: ExprLoc,
        loc: SourceRef,
    },
    Eq {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
    },
    Neq {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
    },
    Gt {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
    },
    Lt {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
    },
    GtEq {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
    },
    LtEq {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
    },
    AssignTo {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
    },
    AccessMember {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
    },
    InitStruct {
        struct_name: ExprLoc,
        fields: Vec<(ExprLoc, ExprLoc)>,
        loc: SourceRef,
    },
    NewFunction {
        // function name
        name: String,
        // function type
        ty: Type,
        // function body
        code: Vec<InsLoc>,
        loc: SourceRef,
    },
    NewStruct {
        // struct name
        name: String,
        // struct fields
        // parsed as either a new constant or
        // a new variable
        fields: Vec<InsLoc>,
        loc: SourceRef,
    },
    NewModule {
        // module name
        name: String,
        // module code
        // this will be allow things similar to the
        // global scope
        code: Vec<InsLoc>,
        loc: SourceRef,
    },
    CallFn {
        // function name
        func: ExprLoc,
        // function arguments
        args: Vec<ExprLoc>,
        loc: SourceRef,
    },
    IndexArray {
        // array
        arr: ExprLoc,
        // index
        idx: ExprLoc,
        loc: SourceRef,
    },
    ErrorNode {
        expectation: String,
        loc: SourceRef,
    },
}

impl Expr {
    pub fn get_source_ref(&self) -> SourceRef {
        match self {
            Expr::Number { loc, .. } => loc.clone(),
            Expr::Str { loc, .. } => loc.clone(),
            Expr::Char { loc, .. } => loc.clone(),
            Expr::Bool { loc, .. } => loc.clone(),
            Expr::Void { loc, .. } => loc.clone(),
            Expr::Ident { loc, .. } => loc.clone(),
            Expr::Add { loc, .. } => loc.clone(),
            Expr::Sub { loc, .. } => loc.clone(),
            Expr::Mul { loc, .. } => loc.clone(),
            Expr::Div { loc, .. } => loc.clone(),
            Expr::Mod { loc, .. } => loc.clone(),
            Expr::Pow { loc, .. } => loc.clone(),
            Expr::And { loc, .. } => loc.clone(),
            Expr::Or { loc, .. } => loc.clone(),
            Expr::Not { loc, .. } => loc.clone(),
            Expr::Negate { loc, .. } => loc.clone(),
            Expr::Eq { loc, .. } => loc.clone(),
            Expr::Neq { loc, .. } => loc.clone(),
            Expr::Gt { loc, .. } => loc.clone(),
            Expr::Lt { loc, .. } => loc.clone(),
            Expr::GtEq { loc, .. } => loc.clone(),
            Expr::LtEq { loc, .. } => loc.clone(),
            Expr::AssignTo { loc, .. } => loc.clone(),
            Expr::AccessMember { loc, .. } => loc.clone(),
            Expr::InitStruct { loc, .. } => loc.clone(),
            Expr::NewFunction { loc, .. } => loc.clone(),
            Expr::NewStruct { loc, .. } => loc.clone(),
            Expr::NewModule { loc, .. } => loc.clone(),
            Expr::ErrorNode { loc, .. } => loc.clone(),
            Expr::CallFn { loc, .. } => loc.clone(),
            Expr::IndexArray { loc, .. } => loc.clone(),
        }
    }
}

// InsLoc will track the location of an instruction in the list of instructions
// the first usize is which list of instructions it is in: top_level or sub_ins
// the second usize is the index of the instruction in the list
pub type InsLoc = (usize, usize);

pub enum Ins {
    NewConstant {
        // constant name
        name: String,
        // maybe a given type or will be inferred
        ty: Type,
        // value
        val: ExprLoc,
        loc: SourceRef,
    },
    NewVariable {
        // variable name
        name: String,
        // maybe a given type or will be inferred
        ty: Type,
        // value
        val: ExprLoc,
        loc: SourceRef,
    },
    NewBlock {
        // block code
        code: Vec<InsLoc>,
        loc: SourceRef,
    },
    ErrorNode {
        expectation: String,
        loc: SourceRef,
    },
}

impl Ins {
    pub fn get_source_ref(&self) -> SourceRef {
        match self {
            Ins::NewConstant { loc, .. } => loc.clone(),
            Ins::NewVariable { loc, .. } => loc.clone(),
            Ins::NewBlock { loc, .. } => loc.clone(),
            Ins::ErrorNode { loc, .. } => loc.clone(),
        }
    }
}

pub struct PCode {
    // top level instructions which will be seen and processed first
    pub top_level: Vec<Ins>,
    // sub instructions are referenced by the top level instructions and
    // other sub instructions
    pub sub_ins: Vec<Ins>,
    pub exprs: Vec<Expr>,
}

impl PCode {
    pub fn new() -> Self {
        PCode {
            top_level: Vec::new(),
            sub_ins: Vec::new(),
            exprs: Vec::new(),
        }
    }

    pub fn get_source_ref(&self, loc: InsLoc) -> SourceRef {
        match loc.0 {
            0 => self.top_level[loc.1].get_source_ref(),
            1 => self.sub_ins[loc.1].get_source_ref(),
            _ => panic!("Invalid InsLoc"),
        }
    }

    pub fn get_source_ref_expr(&self, loc: ExprLoc) -> SourceRef {
        self.exprs[loc].get_source_ref()
    }

    pub fn add_top_level(&mut self, ins: Ins) -> InsLoc {
        let loc = (0, self.top_level.len());
        self.top_level.push(ins);
        loc
    }

    pub fn add_sub_ins(&mut self, ins: Ins) -> InsLoc {
        let loc = (1, self.sub_ins.len());
        self.sub_ins.push(ins);
        loc
    }

    pub fn add_expr(&mut self, expr: Expr) -> ExprLoc {
        let loc = self.exprs.len();
        self.exprs.push(expr);
        loc
    }

    pub fn get_ins(&self, loc: InsLoc) -> &Ins {
        match loc.0 {
            0 => &self.top_level[loc.1],
            1 => &self.sub_ins[loc.1],
            _ => panic!("Invalid InsLoc"),
        }
    }

    pub fn get_expr(&self, loc: ExprLoc) -> &Expr {
        &self.exprs[loc]
    }
}
