#![allow(unused)]

use std::{collections::HashMap, pin::Pin};

use crate::{
    parser::ast::{BinOpType, Expr, Ins, UnaryOpType},
    source::{errors::CheckerError, source::SourceRef},
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

pub struct State {
    env: TypeEnv,
    errs: Vec<CheckerError>,
}

pub fn check_expr(e: &Expr, context_ty: Option<&Type>, state: &mut State) -> Type {
    match e {
        Expr::Number { val, loc } => match context_ty {
            Some(ty) => {
                if ty.tag.is_signed_type() || ty.tag.is_unsigned_type() {
                    match ty.tag {
                        Sig::I8 => {
                            let val_i8 = val.parse::<i8>();
                            match val_i8 {
                                Ok(_) => Type::new(Sig::I8, loc.clone()),
                                Err(_) => {
                                    state.errs.push(CheckerError::NumberTypeInferenceFailed {
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
                                    state.errs.push(CheckerError::NumberTypeInferenceFailed {
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
                                    state.errs.push(CheckerError::NumberTypeInferenceFailed {
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
                                    state.errs.push(CheckerError::NumberTypeInferenceFailed {
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
                                    state.errs.push(CheckerError::NumberTypeInferenceFailed {
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
                                    state.errs.push(CheckerError::NumberTypeInferenceFailed {
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
                                    state.errs.push(CheckerError::NumberTypeInferenceFailed {
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
                                    state.errs.push(CheckerError::NumberTypeInferenceFailed {
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
                    state.errs.push(CheckerError::NumberTypeInferenceFailed {
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
            todo!()
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
        Expr::UnaryOp { op, expr, loc } => todo!(),
        Expr::ErrorExpr { msg, loc } => todo!(),
    }
}

pub fn check_ins(i: &Ins, state: &mut State) {}
