#![allow(unused)]

use std::{collections::HashMap, pin::Pin, process::exit};

use crate::{
    parser::ast::{BinOpType, Expr, FileModule, Ins, UnaryOpType},
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

    fn extend(&self) -> Self {
        let mut new_env = self.clone();
        new_env
    }

    fn add(&mut self, var: String, info: NameInfo) {
        self.vars.insert(var, info);
    }

    fn lookup(&mut self, var: &str) -> Option<&mut NameInfo> {
        self.vars.get_mut(var)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Scope {
    TopLevel,
    Func,
    Block,
    Struct,
    Mod,
    Method,
}

#[derive(Debug, Clone)]
pub struct State {
    src: SourceFile,
    env: TypeEnv,
    pub errs: Vec<CheckerError>,
    scope_stack: Vec<Scope>,
    enter_new_scope: bool,
}

impl State {
    pub fn new(src: SourceFile) -> Self {
        State {
            src,
            env: TypeEnv::new(),
            errs: vec![],
            scope_stack: vec![Scope::TopLevel],
            enter_new_scope: true,
        }
    }

    pub fn is_in_x_scope(&self, x: &Scope) -> bool {
        for scope in self.scope_stack.iter().rev() {
            if scope == x {
                return true;
            }
        }
        return false;
    }

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

pub fn types_are_eq(a: &Type, b: &Type) -> bool {
    match (a.tag, b.tag) {
        (int_a, int_b) if int_a.is_signed_type() && int_b.is_signed_type() && int_a == int_b => {
            true
        }
        (uint_a, uint_b)
            if uint_a.is_unsigned_type() && uint_b.is_unsigned_type() && uint_a == uint_b =>
        {
            true
        }
        (Sig::Bool, Sig::Bool) => true,
        (Sig::Char, Sig::Char) => true,
        (Sig::Str, Sig::Str) => true,
        (Sig::Void, Sig::Void) => true,
        (Sig::Identifier, Sig::Identifier) => todo!(),
        (Sig::ErrorType, _) | (_, Sig::ErrorType) | _ => false,
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
            let l_ty = check_expr(left, &None, state);
            let r_ty = check_expr(right, &None, state);

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
        Expr::CallFn { func, args, loc } => {
            let info = state.env.lookup(&func.as_str());
            match info {
                Some(info) => {
                    info.refs.push(loc.clone());
                    // make sure they have the same arity
                    let fn_arity = info.ty.sub_types.len();
                    let fn_ret_ty = info.ty.aux_type.clone().unwrap();
                    if fn_arity != args.len() {
                        state.push_err(CheckerError::IncorrectFunctionArity {
                            func: func.as_str(),
                            exp: fn_arity,
                            given: args.len(),
                            loc_given: loc.clone(),
                        });
                        return Type::new(Sig::ErrorType, loc.clone());
                    }

                    let fn_param_tys = info.ty.sub_types.clone();
                    for (param_ty, arg) in fn_param_tys.iter().zip(args.iter()) {
                        let arg_ty = check_expr(arg, &Some(param_ty.clone()), state);

                        if !arg_ty.tag.is_error_type() && !types_are_eq(param_ty, &arg_ty) {
                            state.push_err(CheckerError::TypeMismatch {
                                loc: arg.get_source_ref(),
                                expected: param_ty.as_str(),
                                found: arg_ty.as_str(),
                            });
                        }
                    }
                    *fn_ret_ty
                }
                None => {
                    state.push_err(CheckerError::ReferenceToUndefinedName {
                        loc: loc.clone(),
                        var_name: func.as_str(),
                    });
                    Type::new(Sig::ErrorType, loc.clone())
                }
            }
        }
        Expr::UnaryOp { op, expr, loc } => {
            let expr_ty = check_expr(expr, &None, state);

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

pub fn check_ins(i: &Ins, context_ty: &Option<Type>, state: &mut State) {
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
            if let Some(ty) = ty {
                if !expr_ty.tag.is_error_type() && !types_are_eq(ty, &expr_ty) {
                    state.push_err(CheckerError::TypeMismatch {
                        loc: loc.clone(),
                        expected: ty.as_str(),
                        found: expr_ty.as_str(),
                    });
                }
            }
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
                    if let Some(ty) = ty {
                        if !expr_ty.tag.is_error_type() && !types_are_eq(ty, &expr_ty) {
                            state.push_err(CheckerError::TypeMismatch {
                                loc: loc.clone(),
                                expected: ty.as_str(),
                                found: expr_ty.as_str(),
                            });
                        }
                    }
                    let info = NameInfo {
                        ty: expr_ty,
                        refs: vec![loc.clone()],
                        is_const: false,
                        is_initialized: true,
                    };
                    state.env.add(name.as_str(), info);
                }
                (Some(ty), None) => {
                    let var_ty = if !type_is_known(ty) {
                        Type::new(Sig::ErrorType, loc.clone())
                    } else {
                        ty.clone()
                    };
                    // TODO: verify that ty is an actual known type (builtin)
                    // or user defined
                    let info = NameInfo {
                        ty: var_ty,
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

            // construct the function type and add it to the environment
            let mut fn_type = Type::new(Sig::Function, loc.clone());
            let mut pairs_to_register = vec![];
            for param in params.into_iter() {
                let p_ty = if !type_is_known(&param.given_ty) {
                    state.push_err(CheckerError::InvalidType {
                        loc: param.given_ty.loc.clone(),
                        type_name: param.given_ty.as_str(),
                    });
                    Type::new(Sig::ErrorType, param.given_ty.loc.clone())
                } else {
                    param.given_ty.clone()
                };
                fn_type.sub_types.push(p_ty.clone());
                pairs_to_register.push((
                    param.name.as_str(),
                    NameInfo {
                        ty: p_ty,
                        refs: vec![param.loc.clone()],
                        is_const: true,
                        is_initialized: true,
                    },
                ));
            }
            fn_type.aux_type = Some(Box::new(ret_type.clone()));
            let fn_info = NameInfo {
                ty: fn_type,
                refs: vec![loc.clone()],
                is_const: true,
                is_initialized: true,
            };
            state.env.add(name.as_str(), fn_info);
            // copy the current environment and preserve it so we can keep
            // reset it back to it later
            let old_env = state.env.extend();
            state.scope_stack.push(Scope::Func);

            // add the pairs to the environment
            for (param_name, param_info) in pairs_to_register {
                state.env.add(param_name, param_info);
            }

            // check the body
            let old_enter_new_scope = state.enter_new_scope;
            state.enter_new_scope = false;
            check_ins(body, &Some(ret_type.clone()), state);

            // reset the scope info
            state.enter_new_scope = old_enter_new_scope;
            state.scope_stack.pop();
            state.env = old_env;
        }
        Ins::DeclStruct { name, body, loc } => todo!(),
        Ins::DeclModule { name, body, loc } => todo!(),
        Ins::Block { code, loc } => {
            let old_env = if state.enter_new_scope {
                state.scope_stack.push(Scope::Block);
                Some(state.env.extend())
            } else {
                None
            };

            for s_ins in code.iter() {
                check_ins(s_ins, &context_ty, state)
            }

            if let Some(old_env) = old_env {
                state.scope_stack.pop();
                state.env = old_env;
            }
        }
        Ins::AssignTo { target, value, loc } => todo!(),
        Ins::ExprIns { expr, loc } => {
            check_expr(expr, &None, state);
        }
        Ins::Return { expr, loc } => {
            if let Some(e) = expr {
                let expr_ty = check_expr(e, context_ty, state);
                if let Some(context_ty) = context_ty {
                    if !expr_ty.tag.is_error_type() && context_ty.tag == Sig::Void {
                        state.push_err(CheckerError::MismatchingReturnType {
                            exp: "void".to_string(),
                            given: expr_ty.as_str(),
                            loc_given: loc.clone(),
                        });
                        return;
                    }

                    if !expr_ty.tag.is_error_type() && !types_are_eq(&expr_ty, context_ty) {
                        state.push_err(CheckerError::MismatchingReturnType {
                            exp: context_ty.as_str(),
                            given: expr_ty.as_str(),
                            loc_given: loc.clone(),
                        })
                    }
                }
            } else {
                if let Some(context_ty) = context_ty {
                    if context_ty.tag != Sig::Void {
                        state.push_err(CheckerError::MismatchingReturnType {
                            exp: context_ty.as_str(),
                            given: "void".to_string(),
                            loc_given: loc.clone(),
                        })
                    }
                }
            }
        }
        Ins::SingleLineComment { .. } | Ins::ErrorIns { .. } => {
            // do nothing
        }
    }
}

pub fn check_top_level(file_mod: &FileModule, src_file: SourceFile) -> State {
    let mut state = State::new(src_file);
    for tl_ins in file_mod.top_level.iter() {
        check_ins(tl_ins, &None, &mut state);
    }
    state
}
