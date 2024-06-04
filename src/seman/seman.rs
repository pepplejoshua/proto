#![allow(unused)]

use std::collections::HashMap;

use crate::{
    parser::ast::{Expr, Ins, UnaryOpType},
    source::source::SourceRef,
    types::signature::{Sig, Type},
};

#[derive(Debug, Clone)]
struct TypeEnv {
    vars: HashMap<String, Type>,
}

impl TypeEnv {
    fn new() -> Self {
        TypeEnv {
            vars: HashMap::new(),
        }
    }

    fn extend(&self, var: String, ty: Type) -> Self {
        let mut new_env = self.clone();
        new_env.vars.insert(var, ty);
        new_env
    }

    fn lookup(&self, var: &str) -> Option<&Type> {
        self.vars.get(var)
    }
}

#[derive(Debug, Clone)]
pub struct GenTypeVar {
    count: u32,
}

impl GenTypeVar {
    pub fn new() -> Self {
        GenTypeVar { count: 0 }
    }

    pub fn next(&mut self, loc: SourceRef) -> Type {
        let ty = Type {
            tag: Sig::Infer,
            name: Some(format!("T_{count}", count = self.count)),
            sub_types: vec![],
            aux_type: None,
            loc,
        };
        self.count += 1;
        ty
    }

    pub fn next_with_tag(&mut self, tag: &str, loc: SourceRef) -> Type {
        let ty = Type {
            tag: Sig::Infer,
            name: Some(format!("T_{tag}_{count}", count = self.count)),
            sub_types: vec![],
            aux_type: None,
            loc,
        };
        self.count += 1;
        ty
    }
}

#[derive(Debug, Clone)]
pub struct Constraint {
    left: Type,
    right: Type,
}

impl Constraint {
    pub fn as_str(&self) -> String {
        format!("{} = {}", self.left.as_str(), self.right.as_str())
    }
}

#[derive(Debug, Clone)]
pub struct TypeSubstitution {
    ty_var: Type,
    resolved_ty: Type,
}

#[derive(Debug, Clone)]
pub struct State {
    constraints: Vec<Constraint>,
    env: TypeEnv,
    gen: GenTypeVar,
}

impl State {
    pub fn new() -> Self {
        State {
            constraints: vec![],
            env: TypeEnv::new(),
            gen: GenTypeVar::new(),
        }
    }

    pub fn add_constraint(&mut self, lhs: Type, rhs: Type) {
        self.constraints.push(Constraint {
            left: lhs,
            right: rhs,
        })
    }
}

pub fn collect_constraints_from_ins(ins: &Ins, state: &mut State) -> Result<(), String> {
    match &ins {
        Ins::DeclConst {
            name,
            ty,
            init_val,
            loc,
        } => todo!(),
        Ins::DeclVar {
            name,
            ty,
            init_val,
            loc,
        } => todo!(),
        Ins::DeclFunc {
            name,
            params,
            ret_type,
            body,
            loc,
        } => todo!(),
        Ins::DeclStruct { name, body, loc } => todo!(),
        Ins::DeclModule { name, body, loc } => todo!(),
        Ins::Block { code, loc } => todo!(),
        Ins::AssignTo { target, value, loc } => todo!(),
        Ins::ExprIns { expr, loc } => todo!(),
        Ins::Return { expr, loc } => todo!(),
        Ins::SingleLineComment { comment, loc } => todo!(),
        Ins::ErrorIns { msg, loc } => todo!(),
    }
}

pub fn collect_constraints_from_expr(
    expr: &Expr,
    context_ty: Option<&Type>,
    state: &mut State,
) -> Result<Type, String> {
    match &expr {
        Expr::Number { val, loc } => todo!(),
        Expr::Str { loc, .. } => Ok(Type {
            tag: Sig::Str,
            name: None,
            sub_types: vec![],
            aux_type: None,
            loc: loc.clone(),
        }),
        Expr::Char { loc, .. } => Ok(Type {
            tag: Sig::Char,
            name: None,
            sub_types: vec![],
            aux_type: None,
            loc: loc.clone(),
        }),
        Expr::Bool { loc, .. } => Ok(Type {
            tag: Sig::Bool,
            name: None,
            sub_types: vec![],
            aux_type: None,
            loc: loc.clone(),
        }),
        Expr::Void { loc } => Ok(Type {
            tag: Sig::Void,
            name: None,
            sub_types: vec![],
            aux_type: None,
            loc: loc.clone(),
        }),
        Expr::Ident { name, loc } => todo!(),
        Expr::UnaryOp { op, expr, loc } => {
            let expr_ty = collect_constraints_from_expr(expr, context_ty, state)?;

            match op {
                UnaryOpType::Not => {
                    // we will generate the following constraints:
                    // Expr_Ty = Bool
                    // Unary_Expr_Ty = Bool
                    let bool_ty = Type {
                        tag: Sig::Bool,
                        name: None,
                        sub_types: vec![],
                        aux_type: None,
                        loc: expr_ty.loc.clone(),
                    };
                    state.add_constraint(expr_ty.clone(), bool_ty.clone());
                    let unary_ty = state.gen.next_with_tag(&op.as_str(), loc.clone());
                    state.add_constraint(unary_ty.clone(), bool_ty.clone());
                    Ok(bool_ty)
                }
                UnaryOpType::Negate => {
                    // we will generate the following constraints since negation is
                    // only allowed on i8-i64 and int types:
                    // Expr_Ty = Unary_Expr_Ty
                    // Unary_Expr_Ty = Int
                    // Int = i8 | i16 | i32 | i64 | int
                    todo!()
                }
            }
        }
        Expr::BinOp {
            op,
            left,
            right,
            loc,
        } => todo!(),
        Expr::InitStruct {
            struct_name,
            fields,
            loc,
        } => todo!(),
        Expr::CallFn { func, args, loc } => todo!(),
        Expr::ErrorExpr { msg, loc } => Ok(Type {
            tag: Sig::ErrorType,
            name: None,
            sub_types: vec![],
            aux_type: None,
            loc: loc.clone(),
        }),
    }
}
