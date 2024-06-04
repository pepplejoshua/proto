#![allow(unused)]

use std::{collections::HashMap, pin::Pin};

use crate::{
    parser::ast::{BinOpType, Expr, Ins, UnaryOpType},
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
pub enum Constraint {
    Eq(Type, Type),
    OneOf(Type, Vec<Type>),
}

impl Constraint {
    pub fn as_str(&self) -> String {
        match self {
            Constraint::Eq(lhs, rhs) => format!("{} = {}", lhs.as_str(), rhs.as_str()),
            Constraint::OneOf(lhs, opts) => {
                format!(
                    "{} in [{}]",
                    lhs.as_str(),
                    opts.iter()
                        .map(|t| { t.as_str() })
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        }
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

    pub fn add_eq_constraint(&mut self, lhs: Type, rhs: Type) {
        self.constraints.push(Constraint::Eq(lhs, rhs));
    }

    pub fn add_one_of_constraint(&mut self, lhs: Type, opts: Vec<Type>) {
        self.constraints.push(Constraint::OneOf(lhs, opts));
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
                    let bool_ty = Type::new(Sig::Bool, expr_ty.loc.clone());
                    state.add_eq_constraint(expr_ty.clone(), bool_ty.clone());
                    let unary_ty = state.gen.next_with_tag(&op.as_str(), loc.clone());
                    state.add_eq_constraint(unary_ty.clone(), bool_ty.clone());
                    Ok(bool_ty)
                }
                UnaryOpType::Negate => {
                    // we will generate the following constraints since negation is
                    // only allowed on i8-i64 and int types:
                    // Expr_Ty = Int
                    // Unary_Expr_Ty = Expr_Ty
                    // Int = i8 | i16 | i32 | i64 | int
                    let int_types = vec![
                        Type::new(Sig::I8, loc.clone()),
                        Type::new(Sig::I16, loc.clone()),
                        Type::new(Sig::I32, loc.clone()),
                        Type::new(Sig::I64, loc.clone()),
                        Type::new(Sig::Int, loc.clone()),
                    ];
                    let unary_ty = state.gen.next_with_tag(&op.as_str(), loc.clone());
                    state.add_one_of_constraint(expr_ty.clone(), int_types);
                    state.add_eq_constraint(unary_ty.clone(), expr_ty);
                    Ok(unary_ty)
                }
            }
        }
        Expr::BinOp {
            op,
            left,
            right,
            loc,
        } => {
            let lhs_ty = collect_constraints_from_expr(left, context_ty, state);
            let rhs_ty = collect_constraints_from_expr(right, context_ty, state);

            match op {
                // Op(Int, Int)
                // Op(Str, Char)
                // Op(Char, Char)
                BinOpType::Add => {
                    // Int = i8 | i16 | i32 | i64 | int
                    let int_types = vec![
                        Type::new(Sig::I8, loc.clone()),
                        Type::new(Sig::I16, loc.clone()),
                        Type::new(Sig::I32, loc.clone()),
                        Type::new(Sig::I64, loc.clone()),
                        Type::new(Sig::Int, loc.clone()),
                    ];
                    todo!()
                }
                BinOpType::Sub => todo!(),
                BinOpType::Mult => todo!(),
                BinOpType::Div => todo!(),
                BinOpType::Mod => todo!(),
                BinOpType::And
                | BinOpType::Or
                | BinOpType::Eq
                | BinOpType::Neq
                | BinOpType::Gt
                | BinOpType::Lt
                | BinOpType::GtEq
                | BinOpType::LtEq => todo!(),
                BinOpType::AccessMember => todo!(),
                BinOpType::IndexArray => todo!(),
            }
            todo!()
        }
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
