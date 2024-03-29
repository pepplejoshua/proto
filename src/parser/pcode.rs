#![allow(dead_code)]
#![allow(unused_variables)]

use crate::{
    source::source::SourceRef,
    types::signature::{Sig, Type},
};

#[derive(Debug, Clone)]
pub struct FnArg {
    pub name: ExprLoc,
    pub ty: Type,
    pub loc: SourceRef,
}

// ExprLoc will track the location of an expression in the list of expressions
// for the file
pub type ExprLoc = usize;

#[derive(Debug, Clone)]
pub enum Expr {
    // literals
    Number {
        val: String,
        loc: SourceRef,
        ty: Option<Type>,
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
        ty: Option<Type>,
    },

    // operators
    Add {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
        ty: Option<Type>,
    },
    Sub {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
        ty: Option<Type>,
    },
    Mul {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
        ty: Option<Type>,
    },
    Div {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
        ty: Option<Type>,
    },
    Mod {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
        ty: Option<Type>,
    },
    And {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
        ty: Option<Type>,
    },
    Or {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
        ty: Option<Type>,
    },
    Not {
        expr: ExprLoc,
        loc: SourceRef,
        ty: Option<Type>,
    },
    Negate {
        expr: ExprLoc,
        loc: SourceRef,
        ty: Option<Type>,
    },
    Eq {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
        ty: Option<Type>,
    },
    Neq {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
        ty: Option<Type>,
    },
    Gt {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
        ty: Option<Type>,
    },
    Lt {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
        ty: Option<Type>,
    },
    GtEq {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
        ty: Option<Type>,
    },
    LtEq {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
        ty: Option<Type>,
    },
    AccessMember {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
        ty: Option<Type>,
    },
    InitStruct {
        struct_name: ExprLoc,
        fields: Vec<(ExprLoc, ExprLoc)>,
        loc: SourceRef,
        ty: Option<Type>,
    },
    NewFunction {
        // function name
        name: String,
        // function arguments
        args: Vec<FnArg>,
        // function return type
        ret_ty: Type,
        // function body
        code: InsLoc,
        loc: SourceRef,
        fn_sig_loc: SourceRef,
        ty: Option<Type>,
    },
    NewStruct {
        // struct name
        name: String,
        // struct fields
        // parsed as either a new constant or
        // a new variable
        code: InsLoc,
        loc: SourceRef,
        // will contain struct name, fields
        // and methods / functions
        ty: Option<Type>,
    },
    NewModule {
        // module name
        name: String,
        // module code
        // this will be allow things similar to the
        // global scope
        code: InsLoc,
        loc: SourceRef,
        // all modules have type module
    },
    CallFunction {
        // function name
        func: ExprLoc,
        // function arguments
        args: Vec<ExprLoc>,
        loc: SourceRef,
        ty: Option<Type>,
    },
    IndexArray {
        // array
        arr: ExprLoc,
        // index
        idx: ExprLoc,
        loc: SourceRef,
        ty: Option<Type>,
    },
    Directive {
        // directive name
        name: String,
        // directive arguments
        args: Vec<ExprLoc>,
        loc: SourceRef,
        ty: Option<Type>,
    },
    ErrorNode {
        expectation: String,
        loc: SourceRef,
    },
}

impl Expr {
    pub fn update_type(&mut self, new_type: Type) {
        match self {
            Expr::Number { ty, .. } => *ty = Some(new_type),
            Expr::Add { ty, .. } => *ty = Some(new_type),
            Expr::Sub { ty, .. } => *ty = Some(new_type),
            Expr::Mul { ty, .. } => *ty = Some(new_type),
            Expr::Div { ty, .. } => *ty = Some(new_type),
            Expr::Mod { ty, .. } => *ty = Some(new_type),
            Expr::And { ty, .. } => *ty = Some(new_type),
            Expr::Or { ty, .. } => *ty = Some(new_type),
            Expr::Not { ty, .. } => *ty = Some(new_type),
            Expr::Eq { ty, .. } => *ty = Some(new_type),
            Expr::Neq { ty, .. } => *ty = Some(new_type),
            Expr::Gt { ty, .. } => *ty = Some(new_type),
            Expr::Lt { ty, .. } => *ty = Some(new_type),
            Expr::GtEq { ty, .. } => *ty = Some(new_type),
            Expr::LtEq { ty, .. } => *ty = Some(new_type),
            Expr::Negate { ty, .. } => *ty = Some(new_type),
            Expr::AccessMember { ty, .. } => *ty = Some(new_type),
            Expr::InitStruct { ty, .. } => *ty = Some(new_type),
            Expr::NewFunction { ty, .. } => *ty = Some(new_type),
            Expr::NewStruct { ty, .. } => *ty = Some(new_type),
            Expr::CallFunction { ty, .. } => *ty = Some(new_type),
            Expr::IndexArray { ty, .. } => *ty = Some(new_type),
            Expr::Directive { ty, .. } => *ty = Some(new_type),
            Expr::Ident { ty, .. } => *ty = Some(new_type),
            _ => (),
        }
    }

    pub fn has_type(&self) -> bool {
        match self {
            Expr::Number { ty, .. } => ty.is_some(),
            Expr::Add { ty, .. } => ty.is_some(),
            Expr::Sub { ty, .. } => ty.is_some(),
            Expr::Mul { ty, .. } => ty.is_some(),
            Expr::Div { ty, .. } => ty.is_some(),
            Expr::Mod { ty, .. } => ty.is_some(),
            Expr::And { ty, .. } => ty.is_some(),
            Expr::Or { ty, .. } => ty.is_some(),
            Expr::Not { ty, .. } => ty.is_some(),
            Expr::Eq { ty, .. } => ty.is_some(),
            Expr::Neq { ty, .. } => ty.is_some(),
            Expr::Gt { ty, .. } => ty.is_some(),
            Expr::Lt { ty, .. } => ty.is_some(),
            Expr::GtEq { ty, .. } => ty.is_some(),
            Expr::LtEq { ty, .. } => ty.is_some(),
            Expr::Negate { ty, .. } => ty.is_some(),
            Expr::AccessMember { ty, .. } => ty.is_some(),
            Expr::InitStruct { ty, .. } => ty.is_some(),
            Expr::NewFunction { ty, .. } => ty.is_some(),
            Expr::NewStruct { ty, .. } => ty.is_some(),
            Expr::CallFunction { ty, .. } => ty.is_some(),
            Expr::IndexArray { ty, .. } => ty.is_some(),
            Expr::Directive { ty, .. } => ty.is_some(),
            Expr::Bool { .. } => true,
            Expr::Char { .. } => true,
            Expr::Str { .. } => true,
            Expr::Void { .. } => true,
            Expr::Ident { ty, .. } => ty.is_some(),
            _ => false,
        }
    }

    pub fn get_type(&self) -> Option<Type> {
        match self {
            Expr::Number { ty, .. } => ty.clone(),
            Expr::Add { ty, .. } => ty.clone(),
            Expr::Sub { ty, .. } => ty.clone(),
            Expr::Mul { ty, .. } => ty.clone(),
            Expr::Div { ty, .. } => ty.clone(),
            Expr::Mod { ty, .. } => ty.clone(),
            Expr::And { ty, .. } => ty.clone(),
            Expr::Or { ty, .. } => ty.clone(),
            Expr::Not { ty, .. } => ty.clone(),
            Expr::Eq { ty, .. } => ty.clone(),
            Expr::Neq { ty, .. } => ty.clone(),
            Expr::Gt { ty, .. } => ty.clone(),
            Expr::Lt { ty, .. } => ty.clone(),
            Expr::GtEq { ty, .. } => ty.clone(),
            Expr::LtEq { ty, .. } => ty.clone(),
            Expr::Negate { ty, .. } => ty.clone(),
            Expr::AccessMember { ty, .. } => ty.clone(),
            Expr::InitStruct { ty, .. } => ty.clone(),
            Expr::NewFunction { ty, .. } => ty.clone(),
            Expr::NewStruct { ty, .. } => ty.clone(),
            Expr::CallFunction { ty, .. } => ty.clone(),
            Expr::IndexArray { ty, .. } => ty.clone(),
            Expr::Directive { ty, .. } => ty.clone(),
            Expr::Bool { loc, .. } => Some(Type {
                tag: Sig::Bool,
                name: None,
                sub_types: vec![],
                aux_type: None,
                loc: loc.clone(),
            }),
            Expr::Char { loc, .. } => Some(Type {
                tag: Sig::Char,
                name: None,
                sub_types: vec![],
                aux_type: None,
                loc: loc.clone(),
            }),
            Expr::Str { loc, .. } => Some(Type {
                tag: Sig::Str,
                name: None,
                sub_types: vec![],
                aux_type: None,
                loc: loc.clone(),
            }),
            Expr::Void { loc, .. } => Some(Type {
                tag: Sig::Void,
                name: None,
                sub_types: vec![],
                aux_type: None,
                loc: loc.clone(),
            }),
            Expr::Ident { ty, .. } => ty.clone(),
            _ => None,
        }
    }

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
            Expr::AccessMember { loc, .. } => loc.clone(),
            Expr::InitStruct { loc, .. } => loc.clone(),
            Expr::NewFunction { loc, .. } => loc.clone(),
            Expr::NewStruct { loc, .. } => loc.clone(),
            Expr::NewModule { loc, .. } => loc.clone(),
            Expr::ErrorNode { loc, .. } => loc.clone(),
            Expr::CallFunction { loc, .. } => loc.clone(),
            Expr::IndexArray { loc, .. } => loc.clone(),
            Expr::Directive { loc, .. } => loc.clone(),
        }
    }
}

// InsLoc will track the location of an instruction in the list of instructions
// the first usize is which list of instructions it is in: top_level or sub_ins
// the second usize is the index of the instruction in the list
pub type InsLoc = (usize, usize);

#[derive(Debug, Clone)]
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
        val: Option<ExprLoc>,
        loc: SourceRef,
    },
    NewBlock {
        // block code
        code: Vec<InsLoc>,
        loc: SourceRef,
    },
    ExprIns {
        expr: ExprLoc,
        loc: SourceRef,
    },
    ErrorNode {
        expectation: String,
        loc: SourceRef,
    },
    AssignTo {
        lhs: ExprLoc,
        rhs: ExprLoc,
        loc: SourceRef,
    },
    Directive {
        // directive name
        name: String,
        // directive arguments
        ins: InsLoc,
        loc: SourceRef,
    },
    Comment {
        // comment
        comment: String,
        loc: SourceRef,
    },
    Return {
        // some expression to return
        expr: Option<ExprLoc>,
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
            Ins::ExprIns { loc, .. } => loc.clone(),
            Ins::AssignTo { loc, .. } => loc.clone(),
            Ins::Directive { loc, .. } => loc.clone(),
            Ins::Comment { loc, .. } => loc.clone(),
            Ins::Return { loc, .. } => loc.clone(),
        }
    }
}

#[derive(Debug, Clone)]
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

    pub fn update_expr_type(&mut self, loc: &ExprLoc, new_type: Type) {
        self.exprs[*loc].update_type(new_type);
    }

    pub fn as_str(&self) -> String {
        let mut lines = vec![];
        for ins in &self.top_level {
            let ins = self.show_ins(ins);
            lines.push(ins);
        }

        lines.join("\n")
    }

    pub fn show_ins(&self, ins: &Ins) -> String {
        match ins {
            Ins::NewConstant { name, ty, val, .. } => {
                format!(
                    "Constant {} : {} : {}",
                    name,
                    ty.as_str(),
                    self.show_expr(&self.exprs[*val])
                )
            }
            Ins::NewVariable { name, ty, val, loc } => {
                format!(
                    "Variable {} : {} = {}",
                    name,
                    ty.as_str(),
                    if let Some(val) = val {
                        self.show_expr(&self.exprs[*val])
                    } else {
                        "undefined".to_string()
                    }
                )
            }
            Ins::NewBlock { code, loc } => {
                let mut res = format!("\nBlock\n");
                for ins_loc in code {
                    res.push_str(&self.show_ins(&self.sub_ins[ins_loc.1]));
                    res.push('\n');
                }
                res.push_str(&format!("EndBlock"));
                res
            }
            Ins::ExprIns { expr, loc } => {
                format!("ExprIns {}", self.show_expr(&self.exprs[*expr]))
            }
            Ins::ErrorNode { expectation, loc } => {
                format!("Error: {}", expectation)
            }
            Ins::AssignTo { lhs, rhs, loc } => {
                format!(
                    "AssignTo {} = {}",
                    self.show_expr(&self.exprs[*lhs]),
                    self.show_expr(&self.exprs[*rhs])
                )
            }
            Ins::Directive { name, ins, loc } => {
                format!("@{} {};", name, self.show_ins(&self.get_ins(ins)))
            }
            Ins::Comment { comment, loc } => {
                format!("Comment {}", comment)
            }
            Ins::Return { expr, loc } => {
                format!(
                    "Return {};",
                    if let Some(expr) = expr {
                        self.show_expr(&self.exprs[*expr])
                    } else {
                        "".to_string()
                    }
                )
            }
        }
    }

    pub fn show_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::Number { val, .. } => val.clone(),
            Expr::Str { val, .. } => format!("\"{}\"", val),
            Expr::Char { val, .. } => format!("'{}'", val),
            Expr::Bool { val, .. } => val.to_string(),
            Expr::Void { .. } => "void".to_string(),
            Expr::Ident { name, .. } => name.clone(),
            Expr::Add { lhs, rhs, .. } => {
                format!(
                    "{} + {}",
                    self.show_expr(&self.exprs[*lhs]),
                    self.show_expr(&self.exprs[*rhs])
                )
            }
            Expr::Sub { lhs, rhs, .. } => {
                format!(
                    "{} - {}",
                    self.show_expr(&self.exprs[*lhs]),
                    self.show_expr(&self.exprs[*rhs])
                )
            }
            Expr::Mul { lhs, rhs, .. } => {
                format!(
                    "{} * {}",
                    self.show_expr(&self.exprs[*lhs]),
                    self.show_expr(&self.exprs[*rhs])
                )
            }
            Expr::Div { lhs, rhs, .. } => {
                format!(
                    "{} / {}",
                    self.show_expr(&self.exprs[*lhs]),
                    self.show_expr(&self.exprs[*rhs])
                )
            }
            Expr::Mod { lhs, rhs, .. } => {
                format!(
                    "{} % {}",
                    self.show_expr(&self.exprs[*lhs]),
                    self.show_expr(&self.exprs[*rhs])
                )
            }
            Expr::And { lhs, rhs, .. } => {
                format!(
                    "{} && {}",
                    self.show_expr(&self.exprs[*lhs]),
                    self.show_expr(&self.exprs[*rhs])
                )
            }
            Expr::Or { lhs, rhs, .. } => {
                format!(
                    "{} || {}",
                    self.show_expr(&self.exprs[*lhs]),
                    self.show_expr(&self.exprs[*rhs])
                )
            }
            Expr::Not { expr, .. } => {
                format!("!{}", self.show_expr(&self.exprs[*expr]))
            }
            Expr::Negate { expr, .. } => {
                format!("-{}", self.show_expr(&self.exprs[*expr]))
            }
            Expr::Eq { lhs, rhs, .. } => {
                format!(
                    "{} == {}",
                    self.show_expr(&self.exprs[*lhs]),
                    self.show_expr(&self.exprs[*rhs])
                )
            }
            Expr::Neq { lhs, rhs, .. } => {
                format!(
                    "{} != {}",
                    self.show_expr(&self.exprs[*lhs]),
                    self.show_expr(&self.exprs[*rhs])
                )
            }
            Expr::Gt { lhs, rhs, .. } => {
                format!(
                    "{} > {}",
                    self.show_expr(&self.exprs[*lhs]),
                    self.show_expr(&self.exprs[*rhs])
                )
            }
            Expr::Lt { lhs, rhs, .. } => {
                format!(
                    "{} < {}",
                    self.show_expr(&self.exprs[*lhs]),
                    self.show_expr(&self.exprs[*rhs])
                )
            }
            Expr::GtEq { lhs, rhs, .. } => {
                format!(
                    "{} >= {}",
                    self.show_expr(&self.exprs[*lhs]),
                    self.show_expr(&self.exprs[*rhs])
                )
            }
            Expr::LtEq { lhs, rhs, .. } => {
                format!(
                    "{} <= {}",
                    self.show_expr(&self.exprs[*lhs]),
                    self.show_expr(&self.exprs[*rhs])
                )
            }
            Expr::AccessMember { lhs, rhs, .. } => {
                format!(
                    "{}.{}",
                    self.show_expr(&self.exprs[*lhs]),
                    self.show_expr(&self.exprs[*rhs])
                )
            }
            Expr::InitStruct {
                struct_name,
                fields,
                ..
            } => {
                let mut res = format!("{}.(", self.show_expr(&self.exprs[*struct_name]));
                let mut arg_str = vec![];
                for (field_name, field_val) in fields {
                    arg_str.push(format!(
                        "{}: {}",
                        self.show_expr(&self.exprs[*field_name]),
                        self.show_expr(&self.exprs[*field_val])
                    ));
                }
                res.push_str(&arg_str.join(", "));
                res.push_str(")");
                res
            }
            Expr::NewFunction {
                args,
                ret_ty,
                code,
                loc,
                ..
            } => {
                let mut res = format!("fn (");
                let mut arg_str = vec![];
                for arg in args {
                    arg_str.push(format!(
                        "{}: {}",
                        self.show_expr(&self.exprs[arg.name]),
                        arg.ty.as_str()
                    ));
                }
                res.push_str(&arg_str.join(", "));
                res.push_str(&format!(") {} ", ret_ty.as_str()));
                res.push_str(&self.show_ins(&self.get_ins(code)));
                res
            }
            Expr::NewStruct { name, code, .. } => {
                format!("struct {}", self.show_ins(&self.get_ins(code)))
            }
            Expr::NewModule { name, code, loc } => {
                format!("mod {}", self.show_ins(&self.get_ins(code)))
            }
            Expr::CallFunction { func, args, .. } => {
                let mut res = format!("{}(", self.show_expr(&self.exprs[*func]));
                let mut arg_str = vec![];
                for arg in args {
                    arg_str.push(self.show_expr(&self.exprs[*arg]));
                }
                res.push_str(&arg_str.join(", "));
                res.push_str(")");
                res
            }
            Expr::IndexArray { arr, idx, .. } => {
                format!(
                    "{}[{}]",
                    self.show_expr(&self.exprs[*arr]),
                    self.show_expr(&self.exprs[*idx])
                )
            }
            Expr::Directive { name, args, .. } => {
                let mut res = format!("@{}(", name);
                let mut arg_str = vec![];
                for arg in args {
                    arg_str.push(self.show_expr(&self.exprs[*arg]));
                }
                res.push_str(&arg_str.join(", "));
                res.push_str(")");
                res
            }
            Expr::ErrorNode { expectation, loc } => {
                format!("Error: {}", expectation)
            }
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

    pub fn get_ins(&self, loc: &InsLoc) -> &Ins {
        match loc.0 {
            0 => &self.top_level[loc.1],
            1 => &self.sub_ins[loc.1],
            _ => panic!("Invalid InsLoc"),
        }
    }

    pub fn get_ins_c(&self, loc: &InsLoc) -> Ins {
        match loc.0 {
            0 => self.top_level[loc.1].clone(),
            1 => self.sub_ins[loc.1].clone(),
            _ => panic!("Invalid InsLoc"),
        }
    }

    pub fn get_expr(&self, loc: &ExprLoc) -> &Expr {
        &self.exprs[*loc]
    }

    pub fn get_expr_mut(&mut self, loc: &ExprLoc) -> &mut Expr {
        &mut self.exprs[*loc]
    }

    pub fn get_expr_c(&self, loc: &ExprLoc) -> Expr {
        self.exprs[*loc].clone()
    }

    pub fn get_mut_ins(&mut self, loc: InsLoc) -> &mut Ins {
        match loc.0 {
            0 => &mut self.top_level[loc.1],
            1 => &mut self.sub_ins[loc.1],
            _ => panic!("Invalid InsLoc"),
        }
    }

    pub fn get_mut_expr(&mut self, loc: ExprLoc) -> &mut Expr {
        &mut self.exprs[loc]
    }
}
