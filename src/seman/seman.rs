#![allow(unused)]

use std::{collections::HashMap, pin::Pin, process::exit};

use crate::{
    parser::ast::{BinOpType, Expr, Ins, UnaryOpType},
    source::{
        errors::CheckerError,
        source::{SourceFile, SourceRef, SourceReporter},
    },
    types::signature::{Sig, Type},
};

#[derive(Debug, Clone)]
pub struct NameInfo {
    ty: Type,
    refs: Vec<SourceRef>,
    is_const: bool,
    is_initialized: bool,
}

#[derive(Debug, Clone)]
struct TypeEnv {
    vars: HashMap<String, NameInfo>,
}

impl TypeEnv {
    fn new() -> Self {
        TypeEnv {
            vars: HashMap::new(),
        }
    }

    fn extend(&self, var: String, info: NameInfo) -> Self {
        let mut new_env = self.clone();
        new_env.vars.insert(var, info);
        new_env
    }

    fn add(&mut self, var: String, info: NameInfo) {
        self.vars.insert(var, info);
    }

    fn lookup(&mut self, var: &str) -> Option<&mut NameInfo> {
        self.vars.get_mut(var)
    }
}

#[derive(Debug, Clone)]
pub struct State {
    src: SourceFile,
    env: TypeEnv,
    errs: Vec<CheckerError>,
}

impl State {
    pub fn push_err(&mut self, err: CheckerError) {
        self.errs.push(err);

        if self.errs.len() >= 10 {
            let reporter = SourceReporter::new(self.src.clone());
            for ce in self.errs.iter() {
                reporter.report_checker_error(ce.clone());
            }
            exit(1);
        }
    }
}

pub fn type_is_known(ty: &Type) -> bool {
    match ty.tag {
        Sig::Identifier => {
            unimplemented!("seman::type_is_known(): identified types are not implemented yet.")
        }
        _ => true,
    }
}

pub fn check_expr(e: &Expr, context_ty: &Option<Type>, state: &mut State) -> Type {
    match e {
        Expr::Number { val, loc } => match context_ty {
            Some(ty) => {
                if ty.tag.is_numerical_type() {
                    match ty.tag {
                        Sig::I8 => {
                            let val_i8 = val.parse::<i8>();
                            match val_i8 {
                                Ok(_) => Type::new(Sig::I8, loc.clone()),
                                Err(_) => {
                                    state.push_err(CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: ty.as_str(),
                                    });
                                    Type::new(Sig::ErrorType, loc.clone())
                                }
                            }
                        }
                        Sig::I16 => {
                            let val_i16 = val.parse::<i16>();
                            match val_i16 {
                                Ok(_) => Type::new(Sig::I16, loc.clone()),
                                Err(_) => {
                                    state.push_err(CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: ty.as_str(),
                                    });
                                    Type::new(Sig::ErrorType, loc.clone())
                                }
                            }
                        }
                        Sig::I32 | Sig::Int => {
                            let val_i32 = val.parse::<i32>();
                            match val_i32 {
                                Ok(_) => Type::new(ty.tag, loc.clone()),
                                Err(_) => {
                                    state.push_err(CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: ty.as_str(),
                                    });
                                    Type::new(Sig::ErrorType, loc.clone())
                                }
                            }
                        }
                        Sig::I64 => {
                            let val_i64 = val.parse::<i64>();
                            match val_i64 {
                                Ok(_) => Type::new(Sig::I64, loc.clone()),
                                Err(_) => {
                                    state.push_err(CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: ty.as_str(),
                                    });
                                    Type::new(Sig::ErrorType, loc.clone())
                                }
                            }
                        }
                        Sig::U8 => {
                            let val_u8 = val.parse::<u8>();
                            match val_u8 {
                                Ok(_) => Type::new(Sig::U8, loc.clone()),
                                Err(_) => {
                                    state.push_err(CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: ty.as_str(),
                                    });
                                    Type::new(Sig::ErrorType, loc.clone())
                                }
                            }
                        }
                        Sig::U16 => {
                            let val_u16 = val.parse::<u16>();
                            match val_u16 {
                                Ok(_) => Type::new(Sig::U16, loc.clone()),
                                Err(_) => {
                                    state.push_err(CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: ty.as_str(),
                                    });
                                    Type::new(Sig::ErrorType, loc.clone())
                                }
                            }
                        }
                        Sig::U32 | Sig::UInt => {
                            let val_u32 = val.parse::<u32>();
                            match val_u32 {
                                Ok(_) => Type::new(ty.tag, loc.clone()),
                                Err(_) => {
                                    state.push_err(CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: ty.as_str(),
                                    });
                                    Type::new(Sig::ErrorType, loc.clone())
                                }
                            }
                        }
                        Sig::U64 => {
                            let val_u64 = val.parse::<u64>();
                            match val_u64 {
                                Ok(_) => Type::new(Sig::U64, loc.clone()),
                                Err(_) => {
                                    state.push_err(CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: ty.as_str(),
                                    });
                                    Type::new(Sig::ErrorType, loc.clone())
                                }
                            }
                        }
                        _ => unreachable!(
                            "seman::check_expr(): unexpected type {} in number inference.",
                            ty.as_str()
                        ),
                    }
                } else {
                    state.push_err(CheckerError::NumberTypeInferenceFailed {
                        loc: loc.clone(),
                        number: val.clone(),
                        given_type: ty.as_str(),
                    });
                    Type::new(Sig::ErrorType, loc.clone())
                }
            }
            None => {
                // make sure val is valid isize and return the right type
                // else return error
                let val_i32 = val.parse::<i32>();
                match val_i32 {
                    Ok(_) => Type::new(Sig::Int, loc.clone()),
                    Err(_) => {
                        state
                            .errs
                            .push(CheckerError::NumberTypeDefaultInferenceFailed {
                                loc: loc.clone(),
                                number: val.clone(),
                            });
                        Type::new(Sig::ErrorType, loc.clone())
                    }
                }
            }
        },
        Expr::Str { loc, .. } => Type::new(Sig::Str, loc.clone()),
        Expr::Char { loc, .. } => Type::new(Sig::Char, loc.clone()),
        Expr::Bool { loc, .. } => Type::new(Sig::Bool, loc.clone()),
        Expr::Void { loc } => Type::new(Sig::Void, loc.clone()),
        Expr::Ident { name, loc } => {
            let i_ty = state.env.lookup(name);
            match i_ty {
                Some(info) => {
                    info.refs.push(loc.clone());
                    info.ty.clone()
                }
                None => {
                    state.push_err(CheckerError::ReferenceToUndefinedName {
                        loc: loc.clone(),
                        var_name: name.clone(),
                    });
                    Type::new(Sig::ErrorType, loc.clone())
                }
            }
        }
        Expr::BinOp {
            op,
            left,
            right,
            loc,
        } => {
            let l_ty = check_expr(left, context_ty, state);
            let r_ty = check_expr(right, context_ty, state);

            match op {
                BinOpType::Add
                | BinOpType::Sub
                | BinOpType::Mult
                | BinOpType::Div
                | BinOpType::Mod => {
                    if l_ty.tag.is_error_type() || r_ty.tag.is_error_type() {
                        return Type::new(Sig::ErrorType, loc.clone());
                    }

                    match (l_ty.tag, r_ty.tag) {
                        // covers the case where both a and b are numerical types
                        // and are the same type
                        (a, b) if a.is_numerical_type() && b.is_numerical_type() && a == b => {
                            Type::new(a, loc.clone())
                        }
                        (Sig::Char, Sig::Char) | (Sig::Str, Sig::Char) | (Sig::Str, Sig::Str)
                            if matches!(op, BinOpType::Add) =>
                        {
                            Type::new(Sig::Str, loc.clone())
                        }
                        _ => {
                            state.push_err(CheckerError::InvalidUseOfBinaryOperator {
                                loc: loc.clone(),
                                op: op.as_str(),
                                left: l_ty.as_str(),
                                right: r_ty.as_str(),
                            });
                            Type::new(Sig::ErrorType, loc.clone())
                        }
                    }
                }
                BinOpType::And | BinOpType::Or => {
                    if l_ty.tag.is_error_type() || r_ty.tag.is_error_type() {
                        return Type::new(Sig::Bool, loc.clone());
                    }

                    match (l_ty.tag, r_ty.tag) {
                        (Sig::Bool, Sig::Bool) => Type::new(Sig::Bool, loc.clone()),
                        _ => {
                            state.push_err(CheckerError::InvalidUseOfBinaryOperator {
                                loc: loc.clone(),
                                op: op.as_str(),
                                left: l_ty.as_str(),
                                right: r_ty.as_str(),
                            });
                            Type::new(Sig::ErrorType, loc.clone())
                        }
                    }
                }
                BinOpType::Eq
                | BinOpType::Neq
                | BinOpType::Gt
                | BinOpType::Lt
                | BinOpType::GtEq
                | BinOpType::LtEq => {
                    if l_ty.tag.is_error_type() || r_ty.tag.is_error_type() {
                        return Type::new(Sig::Bool, loc.clone());
                    }

                    match (l_ty.tag, r_ty.tag) {
                        (a, b) if a.is_numerical_type() && b.is_numerical_type() && a == b => {
                            Type::new(Sig::Bool, loc.clone())
                        }
                        (Sig::Char, Sig::Char) => Type::new(Sig::Bool, loc.clone()),
                        (Sig::Bool, Sig::Bool) if matches!(op, BinOpType::Eq | BinOpType::Neq) => {
                            Type::new(Sig::Bool, loc.clone())
                        }
                        _ => {
                            state.push_err(CheckerError::InvalidUseOfBinaryOperator {
                                loc: loc.clone(),
                                op: op.as_str(),
                                left: l_ty.as_str(),
                                right: r_ty.as_str(),
                            });
                            Type::new(Sig::ErrorType, loc.clone())
                        }
                    }
                }
                BinOpType::AccessMember => todo!(),
                BinOpType::IndexArray => todo!(),
            }
        }
        Expr::InitStruct {
            struct_name,
            fields,
            loc,
        } => todo!(),
        Expr::CallFn { func, args, loc } => todo!(),
        Expr::UnaryOp { op, expr, loc } => {
            let expr_ty = check_expr(expr, context_ty, state);

            match op {
                UnaryOpType::Not => {
                    if expr_ty.tag.is_error_type() || matches!(expr_ty.tag, Sig::Bool) {
                        // we can just return an error type
                        return Type::new(Sig::Bool, loc.clone());
                    }
                    state.push_err(CheckerError::InvalidUseOfUnaryOperator {
                        loc: loc.clone(),
                        op: op.as_str(),
                        operand: expr.as_str(),
                        tip: None,
                    });
                    Type::new(Sig::ErrorType, loc.clone())
                }
                UnaryOpType::Negate => {
                    if expr_ty.tag.is_error_type() {
                        // we can just return an error type
                        return Type::new(Sig::ErrorType, loc.clone());
                    }

                    if !expr_ty.tag.is_signed_type() {
                        state.push_err(CheckerError::InvalidUseOfUnaryOperator {
                            loc: loc.clone(),
                            op: op.as_str(),
                            operand: expr.as_str(),
                            tip: None,
                        });
                        return Type::new(Sig::ErrorType, loc.clone());
                    }

                    Type::new(expr_ty.tag, loc.clone())
                }
            }
        }
        Expr::ErrorExpr { msg, loc } => Type::new(Sig::ErrorType, loc.clone()),
    }
}

pub fn check_ins(i: &Ins, state: &mut State) {
    match i {
        Ins::DeclConst {
            name,
            ty,
            init_val,
            loc,
        } => {
            let name_info = state.env.lookup(&name.as_str());
            if name_info.is_some() {
                state.push_err(CheckerError::NameAlreadyDefined {
                    loc: loc.clone(),
                    name: name.as_str(),
                });
                return;
            }
            let expr_ty = check_expr(init_val, ty, state);
            let info = NameInfo {
                ty: expr_ty,
                refs: vec![loc.clone()],
                is_const: true,
                is_initialized: true,
            };
            state.env.add(name.as_str(), info);
        }
        Ins::DeclVar {
            name,
            ty,
            init_val,
            loc,
        } => {
            let name_info = state.env.lookup(&name.as_str());
            if name_info.is_some() {
                state.push_err(CheckerError::NameAlreadyDefined {
                    loc: loc.clone(),
                    name: name.as_str(),
                });
                return;
            }

            match (ty, init_val) {
                (Some(_), Some(expr)) => {
                    // TODO: verify that ty is an actual known type (builtin)
                    // or user defined
                    let expr_ty = check_expr(expr, ty, state);
                    let info = NameInfo {
                        ty: expr_ty,
                        refs: vec![loc.clone()],
                        is_const: false,
                        is_initialized: true,
                    };
                    state.env.add(name.as_str(), info);
                }
                (Some(ty), None) => {
                    // TODO: verify that ty is an actual known type (builtin)
                    // or user defined
                    let info = NameInfo {
                        ty: ty.clone(),
                        refs: vec![loc.clone()],
                        is_const: false,
                        is_initialized: false,
                    };
                    state.env.add(name.as_str(), info);
                }
                (None, Some(expr)) => {
                    let expr_ty = check_expr(expr, &None, state);
                    let info = NameInfo {
                        ty: expr_ty,
                        refs: vec![loc.clone()],
                        is_const: false,
                        is_initialized: true,
                    };
                    state.env.add(name.as_str(), info);
                }
                (None, None) => {
                    unreachable!(
                        "seman::check_ins(): variable declaration with no type or init value."
                    )
                }
            }
        }
        Ins::DeclFunc {
            name,
            params,
            ret_type,
            body,
            loc,
        } => {
            // make sure name is not taken
            let fn_info = state.env.lookup(&name.as_str());
            if fn_info.is_some() {
                state.push_err(CheckerError::NameAlreadyDefined {
                    loc: name.get_source_ref(),
                    name: name.as_str(),
                });
                return;
            }

            // check the parameters and add them to the environment
            for param in params.into_iter() {}

            // construct the function type and add it to the environment

            // check the body

            todo!()
        }
        Ins::DeclStruct { name, body, loc } => todo!(),
        Ins::DeclModule { name, body, loc } => todo!(),
        Ins::Block { code, loc } => todo!(),
        Ins::AssignTo { target, value, loc } => todo!(),
        Ins::ExprIns { expr, loc } => {
            check_expr(expr, &None, state);
        }
        Ins::Return { expr, loc } => todo!(),
        Ins::SingleLineComment { .. } | Ins::ErrorIns { .. } => {
            // do nothing
        }
    }
}
