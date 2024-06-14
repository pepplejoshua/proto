#![allow(unused)]

use std::{collections::HashMap, pin::Pin, process::exit};

use crate::{
    codegen::tast::{TyExpr, TyFileModule, TyFnParam, TyIns},
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
    depth: usize,
}

#[derive(Debug, Clone)]
struct TypeEnv {
    vars: Vec<HashMap<String, NameInfo>>,
    depth: usize,
}

impl TypeEnv {
    fn new() -> Self {
        TypeEnv {
            vars: vec![HashMap::new()],
            depth: 0,
        }
    }

    fn extend(&mut self) {
        self.depth += 1;
        self.vars.push(HashMap::new());
    }

    fn pop(&mut self) {
        self.depth -= 1;
        self.vars.pop();
    }

    fn add(&mut self, var: String, info: NameInfo) {
        self.vars.last_mut().unwrap().insert(var, info);
    }

    fn lookup(&mut self, var: &str) -> Option<&mut NameInfo> {
        for group in self.vars.iter_mut().rev() {
            if group.contains_key(var) {
                return group.get_mut(var);
            }
        }
        return None;
    }

    fn shallow_lookup(&mut self, var: &str) -> Option<&mut NameInfo> {
        self.vars.last_mut().unwrap().get_mut(var)
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
    ty_file_mod: TyFileModule,
    pub errs: Vec<CheckerError>,
    scope_stack: Vec<Scope>,
    enter_new_scope: bool,
}

impl State {
    pub fn new(src: SourceFile) -> Self {
        State {
            ty_file_mod: TyFileModule::new(src.path.clone()),
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
        (Sig::StaticArray, Sig::StaticArray) => {
            types_are_eq(a.aux_type.as_ref().unwrap(), b.aux_type.as_ref().unwrap())
                && a.sub_expr.as_ref().unwrap().as_str() == b.sub_expr.as_ref().unwrap().as_str()
        }
        (Sig::ErrorType, _) | (_, Sig::ErrorType) | _ => false,
    }
}

pub fn check_expr(
    e: &Expr,
    context_ty: &Option<Type>,
    state: &mut State,
) -> (Type, Option<TyExpr>) {
    match e {
        Expr::Number { val, loc } => match context_ty {
            Some(ty) => {
                if ty.tag.is_numerical_type() {
                    match ty.tag {
                        Sig::I8 => {
                            let val_i8 = val.parse::<i8>();
                            match val_i8 {
                                Ok(_) => (
                                    Type::new(Sig::I8, loc.clone()),
                                    Some(TyExpr::Integer { val: val.clone() }),
                                ),
                                Err(_) => {
                                    state.push_err(CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: ty.as_str(),
                                    });
                                    (Type::new(Sig::ErrorType, loc.clone()), None)
                                }
                            }
                        }
                        Sig::I16 => {
                            let val_i16 = val.parse::<i16>();
                            match val_i16 {
                                Ok(_) => (
                                    Type::new(Sig::I16, loc.clone()),
                                    Some(TyExpr::Integer { val: val.clone() }),
                                ),
                                Err(_) => {
                                    state.push_err(CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: ty.as_str(),
                                    });
                                    (Type::new(Sig::ErrorType, loc.clone()), None)
                                }
                            }
                        }
                        Sig::I32 | Sig::Int => {
                            // TODO: make int be either i32 or i64 based on platform
                            // mem::size_of::<usize>() == 8
                            // mem::size_of::<usize>() == 4
                            let val_i32 = val.parse::<i32>();
                            match val_i32 {
                                Ok(_) => (
                                    Type::new(ty.tag, loc.clone()),
                                    Some(TyExpr::Integer { val: val.clone() }),
                                ),
                                Err(_) => {
                                    state.push_err(CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: ty.as_str(),
                                    });
                                    (Type::new(Sig::ErrorType, loc.clone()), None)
                                }
                            }
                        }
                        Sig::I64 => {
                            let val_i64 = val.parse::<i64>();
                            match val_i64 {
                                Ok(_) => (
                                    Type::new(Sig::I64, loc.clone()),
                                    Some(TyExpr::Integer { val: val.clone() }),
                                ),
                                Err(_) => {
                                    state.push_err(CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: ty.as_str(),
                                    });
                                    (Type::new(Sig::ErrorType, loc.clone()), None)
                                }
                            }
                        }
                        Sig::U8 => {
                            let val_u8 = val.parse::<u8>();
                            match val_u8 {
                                Ok(_) => (
                                    Type::new(Sig::U8, loc.clone()),
                                    Some(TyExpr::Integer { val: val.clone() }),
                                ),
                                Err(_) => {
                                    state.push_err(CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: ty.as_str(),
                                    });
                                    (Type::new(Sig::ErrorType, loc.clone()), None)
                                }
                            }
                        }
                        Sig::U16 => {
                            let val_u16 = val.parse::<u16>();
                            match val_u16 {
                                Ok(_) => (
                                    Type::new(Sig::U16, loc.clone()),
                                    Some(TyExpr::Integer { val: val.clone() }),
                                ),
                                Err(_) => {
                                    state.push_err(CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: ty.as_str(),
                                    });
                                    (Type::new(Sig::ErrorType, loc.clone()), None)
                                }
                            }
                        }
                        Sig::U32 | Sig::UInt => {
                            // TODO: make uint be either u32 or u64 based on platform
                            // mem::size_of::<usize>() == 8
                            // mem::size_of::<usize>() == 4
                            let val_u32 = val.parse::<u32>();
                            match val_u32 {
                                Ok(_) => (
                                    Type::new(ty.tag, loc.clone()),
                                    Some(TyExpr::Integer { val: val.clone() }),
                                ),
                                Err(_) => {
                                    state.push_err(CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: ty.as_str(),
                                    });
                                    (Type::new(Sig::ErrorType, loc.clone()), None)
                                }
                            }
                        }
                        Sig::U64 => {
                            let val_u64 = val.parse::<u64>();
                            match val_u64 {
                                Ok(_) => (
                                    Type::new(Sig::U64, loc.clone()),
                                    Some(TyExpr::Integer { val: val.clone() }),
                                ),
                                Err(_) => {
                                    state.push_err(CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: ty.as_str(),
                                    });
                                    (Type::new(Sig::ErrorType, loc.clone()), None)
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
                    (Type::new(Sig::ErrorType, loc.clone()), None)
                }
            }
            None => {
                // make sure val is valid isize and return the right type
                // else return error
                let val_i32 = val.parse::<i32>();
                match val_i32 {
                    Ok(_) => (
                        Type::new(Sig::Int, loc.clone()),
                        Some(TyExpr::Integer { val: val.clone() }),
                    ),
                    Err(_) => {
                        state
                            .errs
                            .push(CheckerError::NumberTypeDefaultInferenceFailed {
                                loc: loc.clone(),
                                number: val.clone(),
                            });
                        (Type::new(Sig::ErrorType, loc.clone()), None)
                    }
                }
            }
        },
        Expr::Str { loc, val } => (
            Type::new(Sig::Str, loc.clone()),
            Some(TyExpr::Str { val: val.clone() }),
        ),
        Expr::Char { loc, val } => (
            Type::new(Sig::Char, loc.clone()),
            Some(TyExpr::Char { val: val.clone() }),
        ),
        Expr::Bool { loc, val } => (
            Type::new(Sig::Bool, loc.clone()),
            Some(TyExpr::Bool { val: *val }),
        ),
        Expr::StaticArray { vals, loc } => {
            // static array type can have:
            // - size usize or underscore identifier to infer from number of items.
            // - type or underscore identifier to infer from type of first item or suffix type of array init
            // static array expression can have:
            // - array section with the expected number of items (cannot be changed).
            match context_ty {
                Some(ty) => {
                    // we expect an array type here where the aux_type
                    // will be used as the context_ty to process the items in the array.
                    // assuming all goes well, we can take the type of the context ty,
                    // and the count of the array to create the new type to be returned
                    if !matches!(ty.tag, Sig::StaticArray) {
                        state.push_err(CheckerError::StaticArrayTypeInferenceFailed {
                            given_ty: ty.as_str(),
                            arr_loc: loc.clone(),
                        });
                        return (Type::new(Sig::ErrorType, loc.clone()), None);
                    }

                    // we get the aux_type and sub_expr (if any) in the array type and use it
                    // to perform our checks
                    let arr_size = ty.sub_expr.as_ref();

                    // if there is an array size provided, make sure it is an Expr::Number
                    // and it passes a uint check. Then we can make sure vals.len() is the
                    // same number.
                    let arr_size = match arr_size {
                        Some(expr) => match expr {
                            Expr::Number { val, .. } => {
                                // TODO: make this be either u32 or u64 based on platform
                                // mem::size_of::<usize>() == 8
                                // mem::size_of::<usize>() == 4
                                let size_usize = val.parse::<usize>();
                                if let Ok(size_usize) = size_usize {
                                    if size_usize != vals.len() {
                                        state.push_err(
                                            CheckerError::MismismatchStaticArrayLength {
                                                exp: size_usize,
                                                given: vals.len(),
                                                arr_loc: loc.clone(),
                                            },
                                        );
                                        return (Type::new(Sig::ErrorType, loc.clone()), None);
                                    }
                                    val.clone()
                                } else {
                                    state.push_err(
                                        CheckerError::NonConstantNumberSizeForStaticArray {
                                            loc: loc.clone(),
                                        },
                                    );
                                    return (Type::new(Sig::ErrorType, loc.clone()), None);
                                }
                            }
                            _ => {
                                state.push_err(CheckerError::NonConstantNumberSizeForStaticArray {
                                    loc: expr.get_source_ref(),
                                });
                                return (Type::new(Sig::ErrorType, loc.clone()), None);
                            }
                        },
                        None => vals.len().to_string(),
                    };

                    // we can now go about checking each item in the static array against
                    // the expected item type.
                    let expected_item_ty = ty.aux_type.clone().unwrap();
                    let mut item_ty_exprs = vec![];
                    let mut had_item_error = false;
                    for item in vals.iter() {
                        let (item_ty, item_ty_expr) =
                            check_expr(item, &Some(*(expected_item_ty.clone())), state);

                        if item_ty.tag.is_error_type() {
                            continue;
                        }

                        if !types_are_eq(&expected_item_ty, &item_ty) {
                            state.push_err(CheckerError::MismatchingStaticArrayItemTypes {
                                expected_ty: expected_item_ty.as_str(),
                                given_ty: item_ty.as_str(),
                                loc: item.get_source_ref(),
                            });
                            had_item_error = true;
                        } else {
                            item_ty_exprs.push(item_ty_expr.unwrap());
                        }
                    }

                    if had_item_error {
                        return (Type::new(Sig::ErrorType, loc.clone()), None);
                    }
                    let mut arr_ty = Type::new(Sig::StaticArray, loc.clone());
                    arr_ty.aux_type = Some(Box::new(*expected_item_ty));
                    arr_ty.sub_expr = Some(Expr::Number {
                        val: arr_size,
                        loc: loc.clone(),
                    });
                    (
                        arr_ty,
                        Some(TyExpr::StaticArray {
                            vals: item_ty_exprs,
                        }),
                    )
                }
                None => {
                    // we can do a standard inference on each item in the array, taking the
                    // type of the first item as the type and making sure all the other
                    // items are of the same type. This means there has to be at least
                    // 1 item in the array
                    if vals.len() == 0 {
                        state.push_err(CheckerError::CannotInferTypeOfEmptyArray {
                            loc: loc.clone(),
                        });
                        return (Type::new(Sig::ErrorType, loc.clone()), None);
                    }

                    let (expected_item_ty, first_ty_expr) = check_expr(&vals[0], context_ty, state);
                    if expected_item_ty.tag.is_error_type() {
                        return (Type::new(Sig::ErrorType, loc.clone()), None);
                    }

                    let mut had_item_error = false;
                    let mut item_ty_exprs = vec![first_ty_expr.unwrap()];
                    for index in 1..vals.len() {
                        let cur_item = &vals[index];
                        let (cur_item_ty, cur_item_ty_expr) =
                            check_expr(cur_item, context_ty, state);

                        if cur_item_ty.tag.is_error_type() {
                            continue;
                        }

                        if !types_are_eq(&expected_item_ty, &cur_item_ty) {
                            state.push_err(CheckerError::MismatchingStaticArrayItemTypes {
                                expected_ty: expected_item_ty.as_str(),
                                given_ty: cur_item_ty.as_str(),
                                loc: cur_item.get_source_ref(),
                            });
                            had_item_error = true;
                        } else {
                            item_ty_exprs.push(cur_item_ty_expr.unwrap());
                        }
                    }

                    if had_item_error {
                        return (Type::new(Sig::ErrorType, loc.clone()), None);
                    }
                    let mut arr_ty = Type::new(Sig::StaticArray, loc.clone());
                    arr_ty.aux_type = Some(Box::new(expected_item_ty));
                    arr_ty.sub_expr = Some(Expr::Number {
                        val: vals.len().to_string(),
                        loc: loc.clone(),
                    });
                    (
                        arr_ty,
                        Some(TyExpr::StaticArray {
                            vals: item_ty_exprs,
                        }),
                    )
                }
            }
        }
        Expr::Ident { name, loc } => {
            let i_ty = state.env.lookup(name);
            match i_ty {
                Some(info) => {
                    info.refs.push(loc.clone());
                    (info.ty.clone(), Some(TyExpr::Ident { name: name.clone() }))
                }
                None => {
                    state.push_err(CheckerError::ReferenceToUndefinedName {
                        loc: loc.clone(),
                        var_name: name.clone(),
                    });
                    (Type::new(Sig::ErrorType, loc.clone()), None)
                }
            }
        }
        Expr::BinOp {
            op,
            left,
            right,
            loc,
        } => {
            let (l_ty, l_ty_expr) = check_expr(left, &None, state);
            let (r_ty, r_ty_expr) = check_expr(right, &None, state);

            match op {
                BinOpType::Add
                | BinOpType::Sub
                | BinOpType::Mult
                | BinOpType::Div
                | BinOpType::Mod => {
                    if l_ty.tag.is_error_type() || r_ty.tag.is_error_type() {
                        return (Type::new(Sig::ErrorType, loc.clone()), None);
                    }

                    match (l_ty.tag, r_ty.tag) {
                        // covers the case where both a and b are numerical types
                        // and are the same type
                        (a, b) if a.is_numerical_type() && b.is_numerical_type() && a == b => (
                            Type::new(a, loc.clone()),
                            Some(TyExpr::BinOp {
                                op: *op,
                                lhs: Box::new(l_ty_expr.unwrap()),
                                rhs: Box::new(r_ty_expr.unwrap()),
                            }),
                        ),
                        (Sig::Char, Sig::Char) | (Sig::Str, Sig::Char) | (Sig::Str, Sig::Str)
                            if matches!(op, BinOpType::Add) =>
                        {
                            (
                                Type::new(Sig::Str, loc.clone()),
                                Some(TyExpr::BinOp {
                                    op: *op,
                                    lhs: Box::new(l_ty_expr.unwrap()),
                                    rhs: Box::new(r_ty_expr.unwrap()),
                                }),
                            )
                        }
                        _ => {
                            state.push_err(CheckerError::InvalidUseOfBinaryOperator {
                                loc: loc.clone(),
                                op: op.as_str(),
                                left: l_ty.as_str(),
                                right: r_ty.as_str(),
                            });
                            (Type::new(Sig::ErrorType, loc.clone()), None)
                        }
                    }
                }
                BinOpType::And | BinOpType::Or => {
                    if l_ty.tag.is_error_type() || r_ty.tag.is_error_type() {
                        return (Type::new(Sig::Bool, loc.clone()), None);
                    }

                    match (l_ty.tag, r_ty.tag) {
                        (Sig::Bool, Sig::Bool) => (
                            Type::new(Sig::Bool, loc.clone()),
                            Some(TyExpr::BinOp {
                                op: *op,
                                lhs: Box::new(l_ty_expr.unwrap()),
                                rhs: Box::new(r_ty_expr.unwrap()),
                            }),
                        ),
                        _ => {
                            state.push_err(CheckerError::InvalidUseOfBinaryOperator {
                                loc: loc.clone(),
                                op: op.as_str(),
                                left: l_ty.as_str(),
                                right: r_ty.as_str(),
                            });
                            (Type::new(Sig::ErrorType, loc.clone()), None)
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
                        return (Type::new(Sig::Bool, loc.clone()), None);
                    }

                    match (l_ty.tag, r_ty.tag) {
                        (a, b) if a.is_numerical_type() && b.is_numerical_type() && a == b => (
                            Type::new(Sig::Bool, loc.clone()),
                            Some(TyExpr::BinOp {
                                op: *op,
                                lhs: Box::new(l_ty_expr.unwrap()),
                                rhs: Box::new(r_ty_expr.unwrap()),
                            }),
                        ),
                        (Sig::Char, Sig::Char) => (
                            Type::new(Sig::Bool, loc.clone()),
                            Some(TyExpr::BinOp {
                                op: *op,
                                lhs: Box::new(l_ty_expr.unwrap()),
                                rhs: Box::new(r_ty_expr.unwrap()),
                            }),
                        ),
                        (Sig::Bool, Sig::Bool) if matches!(op, BinOpType::Eq | BinOpType::Neq) => (
                            Type::new(Sig::Bool, loc.clone()),
                            Some(TyExpr::BinOp {
                                op: *op,
                                lhs: Box::new(l_ty_expr.unwrap()),
                                rhs: Box::new(r_ty_expr.unwrap()),
                            }),
                        ),
                        _ => {
                            state.push_err(CheckerError::InvalidUseOfBinaryOperator {
                                loc: loc.clone(),
                                op: op.as_str(),
                                left: l_ty.as_str(),
                                right: r_ty.as_str(),
                            });
                            (Type::new(Sig::ErrorType, loc.clone()), None)
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
            let (func_ty, func_ty_expr) = check_expr(func, &None, state);

            if func_ty.tag.is_error_type() {
                return (func_ty, None);
            }

            match func_ty.tag {
                Sig::Function => {
                    let fn_arity = func_ty.sub_types.len();
                    if fn_arity != args.len() {
                        state.push_err(CheckerError::IncorrectFunctionArity {
                            func: func.as_str(),
                            exp: fn_arity,
                            given: args.len(),
                            loc_given: loc.clone(),
                        });
                        return (Type::new(Sig::ErrorType, loc.clone()), None);
                    }
                    let fn_ret_ty = func_ty.aux_type.clone().unwrap();
                    let fn_param_tys = func_ty.sub_types.clone();
                    let mut fn_ty_args = vec![];
                    for (param_ty, arg) in fn_param_tys.iter().zip(args.iter()) {
                        let (arg_ty, arg_ty_expr) = check_expr(arg, &Some(param_ty.clone()), state);

                        if !arg_ty.tag.is_error_type() && !types_are_eq(param_ty, &arg_ty) {
                            state.push_err(CheckerError::TypeMismatch {
                                loc: arg.get_source_ref(),
                                expected: param_ty.as_str(),
                                found: arg_ty.as_str(),
                            });
                        }
                        if let Some(arg_ty_expr) = arg_ty_expr {
                            fn_ty_args.push(arg_ty_expr);
                        }
                    }
                    (
                        *fn_ret_ty,
                        Some(TyExpr::CallFn {
                            func: Box::new(func_ty_expr.unwrap()),
                            args: fn_ty_args,
                        }),
                    )
                }
                _ => {
                    state.push_err(CheckerError::NameIsNotCallable {
                        name: func.as_str(),
                        name_ty: func_ty.as_str(),
                        loc: func.get_source_ref(),
                    });
                    return (Type::new(Sig::ErrorType, loc.clone()), None);
                }
            }
        }
        Expr::UnaryOp { op, expr, loc } => {
            let (expr_ty, expr_ty_expr) = check_expr(expr, &None, state);

            match op {
                UnaryOpType::Not => {
                    if expr_ty.tag.is_error_type() {
                        return (Type::new(Sig::Bool, loc.clone()), None);
                    }

                    if matches!(expr_ty.tag, Sig::Bool) {
                        return (
                            Type::new(Sig::Bool, loc.clone()),
                            Some(TyExpr::UnaryOp {
                                op: *op,
                                expr: Box::new(expr_ty_expr.unwrap()),
                            }),
                        );
                    }
                    state.push_err(CheckerError::InvalidUseOfUnaryOperator {
                        loc: loc.clone(),
                        op: op.as_str(),
                        operand: expr.as_str(),
                        tip: None,
                    });
                    (Type::new(Sig::ErrorType, loc.clone()), None)
                }
                UnaryOpType::Negate => {
                    if expr_ty.tag.is_error_type() {
                        // we can just return an error type
                        return (Type::new(Sig::ErrorType, loc.clone()), None);
                    }

                    if !expr_ty.tag.is_signed_type() {
                        state.push_err(CheckerError::InvalidUseOfUnaryOperator {
                            loc: loc.clone(),
                            op: op.as_str(),
                            operand: expr.as_str(),
                            tip: None,
                        });
                        return (Type::new(Sig::ErrorType, loc.clone()), None);
                    }

                    (
                        Type::new(expr_ty.tag, loc.clone()),
                        Some(TyExpr::UnaryOp {
                            op: *op,
                            expr: Box::new(expr_ty_expr.unwrap()),
                        }),
                    )
                }
            }
        }
        Expr::ErrorExpr { msg, loc } => (Type::new(Sig::ErrorType, loc.clone()), None),
    }
}

pub fn check_ins(i: &Ins, context_ty: &Option<Type>, state: &mut State) -> Option<TyIns> {
    match i {
        Ins::DeclConst {
            name,
            ty,
            init_val,
            loc,
        } => {
            let name_info = state.env.shallow_lookup(&name.as_str());
            if name_info.is_some() {
                state.push_err(CheckerError::NameAlreadyDefined {
                    loc: loc.clone(),
                    name: name.as_str(),
                });
                return None;
            }
            let (expr_ty, expr_ty_expr) = check_expr(init_val, ty, state);
            let ty_ins = if let Some(ty) = ty {
                if !expr_ty.tag.is_error_type() && !types_are_eq(ty, &expr_ty) {
                    state.push_err(CheckerError::TypeMismatch {
                        loc: loc.clone(),
                        expected: ty.as_str(),
                        found: expr_ty.as_str(),
                    });
                    None
                } else {
                    Some(TyIns::Constant {
                        name: name.as_str(),
                        ty: expr_ty.clone(),
                        init: expr_ty_expr.unwrap(),
                    })
                }
            } else if expr_ty.tag.is_error_type() {
                None
            } else {
                Some(TyIns::Constant {
                    name: name.as_str(),
                    ty: expr_ty.clone(),
                    init: expr_ty_expr.unwrap(),
                })
            };
            let info = NameInfo {
                ty: expr_ty.clone(),
                refs: vec![loc.clone()],
                is_const: true,
                is_initialized: true,
                depth: state.env.depth,
            };
            state.env.add(name.as_str(), info);
            ty_ins
        }
        Ins::DeclVar {
            name,
            ty,
            init_val,
            loc,
        } => {
            let name_info = state.env.shallow_lookup(&name.as_str());
            if name_info.is_some() {
                state.push_err(CheckerError::NameAlreadyDefined {
                    loc: loc.clone(),
                    name: name.as_str(),
                });
                return None;
            }

            let ty_ins = match (ty, init_val) {
                (Some(_), Some(expr)) => {
                    // TODO: verify that ty is an actual known type (builtin)
                    // or user defined
                    let (expr_ty, expr_ty_expr) = check_expr(expr, ty, state);
                    let ty_ins = if let Some(ty) = ty {
                        if !expr_ty.tag.is_error_type() && !types_are_eq(ty, &expr_ty) {
                            state.push_err(CheckerError::TypeMismatch {
                                loc: loc.clone(),
                                expected: ty.as_str(),
                                found: expr_ty.as_str(),
                            });
                            None
                        } else {
                            Some(TyIns::Var {
                                name: name.as_str(),
                                ty: expr_ty.clone(),
                                init: expr_ty_expr,
                            })
                        }
                    } else if expr_ty.tag.is_error_type() {
                        None
                    } else {
                        Some(TyIns::Var {
                            name: name.as_str(),
                            ty: expr_ty.clone(),
                            init: expr_ty_expr,
                        })
                    };
                    let info = NameInfo {
                        ty: expr_ty.clone(),
                        refs: vec![loc.clone()],
                        is_const: false,
                        is_initialized: true,
                        depth: state.env.depth,
                    };
                    state.env.add(name.as_str(), info);
                    ty_ins
                }
                (Some(ty), None) => {
                    let var_ty = if !type_is_known(ty) {
                        Type::new(Sig::ErrorType, loc.clone())
                    } else {
                        ty.clone()
                    };
                    // TODO: verify that ty is an actual known type (builtin)
                    // or user defined
                    // For static arrays where the size and type are not specified
                    // we will need to either enforce providing them or
                    let info = NameInfo {
                        ty: var_ty.clone(),
                        refs: vec![loc.clone()],
                        is_const: false,
                        is_initialized: false,
                        depth: state.env.depth,
                    };
                    state.env.add(name.as_str(), info);
                    Some(TyIns::Var {
                        name: name.as_str(),
                        ty: var_ty.clone(),
                        init: None,
                    })
                }
                (None, Some(expr)) => {
                    let (expr_ty, expr_ty_expr) = check_expr(expr, &None, state);
                    let info = NameInfo {
                        ty: expr_ty.clone(),
                        refs: vec![loc.clone()],
                        is_const: false,
                        is_initialized: true,
                        depth: state.env.depth,
                    };

                    let ty_ins = if expr_ty.tag.is_error_type() {
                        None
                    } else {
                        Some(TyIns::Var {
                            name: name.as_str(),
                            ty: expr_ty,
                            init: expr_ty_expr,
                        })
                    };
                    state.env.add(name.as_str(), info);
                    ty_ins
                }
                (None, None) => {
                    unreachable!(
                        "seman::check_ins(): variable declaration with no type or init value."
                    )
                }
            };
            ty_ins
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
                return None;
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
                        depth: state.env.depth + 1,
                    },
                ));
            }
            fn_type.aux_type = Some(Box::new(ret_type.clone()));
            let fn_info = NameInfo {
                ty: fn_type,
                refs: vec![loc.clone()],
                is_const: true,
                is_initialized: true,
                depth: state.env.depth,
            };
            state.env.add(name.as_str(), fn_info);
            // copy the current environment and preserve it so we can keep
            // reset it back to it later
            state.env.extend();
            state.scope_stack.push(Scope::Func);

            // add the pairs to the environment
            let mut ty_params = vec![];
            for (param_name, param_info) in pairs_to_register {
                state.env.add(param_name.clone(), param_info.clone());
                ty_params.push(TyFnParam {
                    name: param_name,
                    given_ty: param_info.ty,
                });
            }

            // check the body
            let old_enter_new_scope = state.enter_new_scope;
            state.enter_new_scope = false;
            let new_body = check_ins(body, &Some(ret_type.clone()), state);

            // make sure the last instruction in the body returns a valid value
            // if we are not in a function that returns void. This specifically
            // focuses on if conditional and code blocks. Any other
            // instruction should raise an error since we cannot return from
            // within them.

            // reset the scope info
            state.enter_new_scope = old_enter_new_scope;
            state.scope_stack.pop();
            state.env.pop();
            Some(TyIns::Func {
                name: name.as_str(),
                params: ty_params,
                ret_ty: ret_type.clone(),
                body: Box::new(new_body.unwrap()),
            })
        }
        Ins::IfConditional {
            conds_and_code,
            loc,
        } => todo!(),
        Ins::DeclStruct { name, body, loc } => todo!(),
        Ins::DeclModule { name, body, loc } => todo!(),
        Ins::Block { code, loc } => {
            if state.enter_new_scope {
                state.scope_stack.push(Scope::Block);
                state.env.extend();
            }

            let mut new_code = vec![];
            let old_enter_new_scope = state.enter_new_scope;
            state.enter_new_scope = true;
            for s_ins in code.iter() {
                let new_ins = check_ins(s_ins, &context_ty, state);
                if new_ins.is_some() {
                    new_code.push(new_ins.unwrap())
                }
            }

            if old_enter_new_scope {
                state.scope_stack.pop();
                state.env.pop();
            }

            state.enter_new_scope = old_enter_new_scope;

            Some(TyIns::Block { code: new_code })
        }
        Ins::AssignTo { target, value, loc } => todo!(),
        Ins::ExprIns { expr, loc } => {
            let (expr_ty, expr_ty_expr) = check_expr(expr, &None, state);

            if expr_ty.tag.is_error_type() {
                return None;
            }
            Some(TyIns::ExprIns {
                expr: expr_ty_expr.unwrap(),
            })
        }
        Ins::Return { expr, loc } => {
            if let Some(e) = expr {
                let (expr_ty, ret_ty_expr) = check_expr(e, context_ty, state);
                if let Some(context_ty) = context_ty {
                    if !expr_ty.tag.is_error_type() && context_ty.tag == Sig::Void {
                        state.push_err(CheckerError::MismatchingReturnType {
                            exp: "void".to_string(),
                            given: expr_ty.as_str(),
                            loc_given: loc.clone(),
                        });
                        return None;
                    }

                    if !expr_ty.tag.is_error_type() && !types_are_eq(&expr_ty, context_ty) {
                        state.push_err(CheckerError::MismatchingReturnType {
                            exp: context_ty.as_str(),
                            given: expr_ty.as_str(),
                            loc_given: loc.clone(),
                        });
                        return None;
                    }
                }
                Some(TyIns::Return { expr: ret_ty_expr })
            } else {
                if let Some(context_ty) = context_ty {
                    if context_ty.tag != Sig::Void {
                        state.push_err(CheckerError::MismatchingReturnType {
                            exp: context_ty.as_str(),
                            given: "void".to_string(),
                            loc_given: loc.clone(),
                        });
                        return None;
                    }
                }

                Some(TyIns::Return { expr: None })
            }
        }
        Ins::SingleLineComment { .. } | Ins::ErrorIns { .. } => {
            // do nothing
            None
        }
    }
}

pub fn check_top_level(file_mod: &FileModule, src_file: SourceFile) -> (State, TyFileModule) {
    let mut state = State::new(src_file.clone());
    let mut tl_ty_ins = vec![];
    for tl_ins in file_mod.top_level.iter() {
        let n_tl_ins = check_ins(tl_ins, &None, &mut state);
        if n_tl_ins.is_some() {
            tl_ty_ins.push(n_tl_ins.unwrap());
        }
    }
    let ty_file_mod = TyFileModule {
        top_level: tl_ty_ins,
        src_file: src_file.path,
    };
    (state, ty_file_mod)
}
