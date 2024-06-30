#![allow(unused)]

use std::{collections::HashMap, pin::Pin, process::exit, sync::Once};

use serde::de::Expected;

use crate::{
    codegen::tast::{TyExpr, TyFileModule, TyFnParam, TyIns},
    parser::ast::{BinOpType, Expr, FileModule, Ins, UnaryOpType},
    source::{
        errors::CheckerError,
        source::{SourceFile, SourceRef, SourceReporter},
    },
    types::signature::Ty,
};

#[derive(Debug, Clone)]
pub struct NameInfo {
    ty: Ty,
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
    pub errs: Vec<CheckerError>,
    scope_stack: Vec<Scope>,
    enter_new_scope: bool,
    check_for_return: bool,
}

impl State {
    pub fn new(src: SourceFile) -> Self {
        State {
            src,
            env: TypeEnv::new(),
            errs: vec![],
            scope_stack: vec![Scope::TopLevel],
            enter_new_scope: true,
            check_for_return: false,
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

static BUILTIN_METHODS_INIT: Once = Once::new();
// It will be a [str: [str: Ty]]
// The main hashmap will be keyed by the type. The sub hashmap will be keyed by
// the method
static mut BUILTIN_METHODS_HASHMAP: Option<HashMap<&'static str, HashMap<&'static str, Ty>>> = None;

fn init_builtins() {
    let mut m = HashMap::new();
    let builtin_loc = SourceRef {
        file: "__proto_gen_builtins__".to_string(),
        start_line: 0,
        start_col: 0,
        end_line: 0,
        end_col: 0,
        flat_start: 0,
        flat_end: 0,
    };
    // m.insert(
    //     "str",
    //     HashMap::from([(
    //         "len",
    //         Ty::Func {
    //             params: vec![],
    //             ret: Box::new(Ty::get_uint_ty(builtin_loc.clone())),
    //             loc: builtin_loc.clone(),
    //         },
    //     )]),
    // );

    unsafe {
        BUILTIN_METHODS_HASHMAP = Some(m);
    }
}

fn get_builtin_methods() -> &'static HashMap<&'static str, HashMap<&'static str, Ty>> {
    unsafe {
        BUILTIN_METHODS_INIT.call_once(init_builtins);
        BUILTIN_METHODS_HASHMAP.as_ref().unwrap()
    }
}

enum TypeValidErr {
    Invalid,
    Incomplete,
}

fn type_is_valid(ty: &Ty) -> Result<(), (TypeValidErr, SourceRef)> {
    match ty {
        Ty::Func { params, ret, .. } => {
            for p in params.iter() {
                type_is_valid(p)?
            }
            type_is_valid(ret)
        }
        Ty::StaticArray { sub_ty, size, loc } => match size {
            Some(_) => type_is_valid(sub_ty),
            None => Err((TypeValidErr::Incomplete, loc.clone())),
        },
        Ty::Slice { sub_ty, .. } | Ty::Optional { sub_ty, .. } => type_is_valid(sub_ty),
        Ty::ErrorType { loc } => Err((TypeValidErr::Invalid, loc.clone())),
        _ => Ok(()),
    }
}

pub fn types_are_eq(a: &Ty, b: &Ty) -> bool {
    match (a, b) {
        (
            Ty::Signed { size, is_int, .. },
            Ty::Signed {
                size: b_size,
                is_int: b_is_int,
                ..
            },
        ) => *size == *b_size && *is_int == *b_is_int,
        (
            Ty::Unsigned { size, is_uint, .. },
            Ty::Unsigned {
                size: b_size,
                is_uint: b_is_uint,
                ..
            },
        ) => *size == *b_size && *is_uint == *b_is_uint,
        (Ty::Bool { .. }, Ty::Bool { .. })
        | (Ty::Str { .. }, Ty::Str { .. })
        | (Ty::Char { .. }, Ty::Char { .. })
        | (Ty::Void { .. }, Ty::Void { .. }) => true,
        (
            Ty::StaticArray { sub_ty, size, .. },
            Ty::StaticArray {
                sub_ty: b_sub_ty,
                size: b_size,
                ..
            },
        ) => {
            let sub_tys_are_eq = types_are_eq(sub_ty, b_sub_ty);

            if size.is_none() || b_size.is_none() {
                sub_tys_are_eq
            } else {
                size.as_ref().unwrap().as_str() == b_size.as_ref().unwrap().as_str()
            }
        }
        (
            Ty::Slice { sub_ty, .. },
            Ty::Slice {
                sub_ty: b_sub_ty, ..
            },
        )
        | (
            Ty::Optional { sub_ty, .. },
            Ty::Optional {
                sub_ty: b_sub_ty, ..
            },
        ) => types_are_eq(sub_ty, b_sub_ty),
        (_, Ty::ErrorType { .. }) | (Ty::ErrorType { .. }, _) | _ => false,
    }
}

pub fn check_expr(e: &Expr, context_ty: &Option<Ty>, state: &mut State) -> (Ty, Option<TyExpr>) {
    match e {
        Expr::Number { val, loc } => match context_ty {
            Some(ty) => {
                if ty.is_num_ty() {
                    let val_is_err = match ty {
                        Ty::Signed { size, is_int, loc } => {
                            match size {
                                8 => {
                                    val.parse::<i8>().is_err()
                                }
                                16 => {
                                    val.parse::<i16>().is_err()
                                }
                                32 => {
                                    val.parse::<i32>().is_err()
                                }
                                64 => {
                                    val.parse::<i64>().is_err()
                                }
                                _ => unreachable!(
                                    "seman::check_expr(): unexpected signed size {} in number inference.",
                                    size
                                )
                            }
                        }
                        Ty::Unsigned { size, is_uint, loc } => {
                            match size {
                                8 => {
                                    val.parse::<u8>().is_err()
                                }
                                16 => {
                                    val.parse::<u16>().is_err()
                                }
                                32 => {
                                    val.parse::<u32>().is_err()
                                }
                                64 => {
                                    val.parse::<u64>().is_err()
                                }
                                _ => unreachable!(
                                    "seman::check_expr(): unexpected unsigned size {} in number inference.",
                                    size
                                )
                            }
                        }
                        _ => unreachable!(
                            "seman::check_expr(): unexpected type {} in number inference.",
                            ty.as_str()
                        ),
                    };

                    if val_is_err {
                        state.push_err(CheckerError::NumberTypeInferenceFailed {
                            loc: loc.clone(),
                            number: val.clone(),
                            given_type: ty.as_str(),
                        });
                        (Ty::ErrorType { loc: loc.clone() }, None)
                    } else {
                        (
                            ty.clone_loc(loc.clone()),
                            Some(TyExpr::Integer { val: val.clone() }),
                        )
                    }
                } else {
                    // make sure val is valid isize and return the right type
                    // else return error
                    // 64 bit platform
                    let size = Ty::get_platform_size();
                    let val_is_err = if size == 64 {
                        val.parse::<i64>().is_err()
                    } else {
                        val.parse::<i32>().is_err()
                    };

                    if val_is_err {
                        state.push_err(CheckerError::NumberTypeDefaultInferenceFailed {
                            loc: loc.clone(),
                            number: val.clone(),
                        });
                        (Ty::ErrorType { loc: loc.clone() }, None)
                    } else {
                        (
                            Ty::Signed {
                                size,
                                is_int: true,
                                loc: loc.clone(),
                            },
                            Some(TyExpr::Integer { val: val.clone() }),
                        )
                    }
                }
            }
            None => {
                // make sure val is valid isize and return the right type
                // else return error
                // make sure val is valid isize and return the right type
                // else return error
                // 64 bit platform
                let size = Ty::get_platform_size();
                let val_is_err = if size == 64 {
                    val.parse::<i64>().is_err()
                } else {
                    val.parse::<i32>().is_err()
                };

                if val_is_err {
                    state.push_err(CheckerError::NumberTypeDefaultInferenceFailed {
                        loc: loc.clone(),
                        number: val.clone(),
                    });
                    (Ty::ErrorType { loc: loc.clone() }, None)
                } else {
                    (
                        Ty::Signed {
                            size,
                            is_int: true,
                            loc: loc.clone(),
                        },
                        Some(TyExpr::Integer { val: val.clone() }),
                    )
                }
            }
        },
        Expr::Str { loc, val } => (
            Ty::Str {
                loc: loc.clone(),
                is_interp: false,
            },
            Some(TyExpr::Str { val: val.clone() }),
        ),
        Expr::Char { loc, val } => (
            Ty::Char { loc: loc.clone() },
            Some(TyExpr::Char { val: val.clone() }),
        ),
        Expr::Bool { loc, val } => (
            Ty::Bool { loc: loc.clone() },
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
                    match ty {
                        Ty::StaticArray { sub_ty, size, loc } => {
                            let arr_size = match size {
                                Some(e) => {
                                    if !matches!(e, Expr::Number { val, .. }) {
                                        state.push_err(
                                            CheckerError::NonConstantNumberSizeForStaticArray {
                                                loc: e.get_source_ref(),
                                            },
                                        );
                                        return (Ty::ErrorType { loc: loc.clone() }, None);
                                    }

                                    let (e_ty, e_expr) = check_expr(
                                        e,
                                        &Some(Ty::get_uint_ty(e.get_source_ref())),
                                        state,
                                    );

                                    if e_ty.is_error_ty() {
                                        return (Ty::ErrorType { loc: loc.clone() }, None);
                                    }

                                    let e_expr = e_expr.unwrap();
                                    let e_expr_str = e_expr.as_str();
                                    if e_expr_str != vals.len().to_string() {
                                        state.push_err(
                                            CheckerError::MismismatchStaticArrayLength {
                                                exp: e_expr_str,
                                                given: vals.len().to_string(),
                                                arr_loc: loc.clone(),
                                            },
                                        );
                                        return (Ty::ErrorType { loc: loc.clone() }, None);
                                    }
                                    e_expr_str
                                }
                                _ => vals.len().to_string(),
                            };

                            // we can now go about checking each item in the static array against
                            // the expected item type.
                            let expected_item_ty = *sub_ty.clone();
                            let mut item_ty_exprs = vec![];
                            let mut had_item_error = false;
                            for item in vals.iter() {
                                let (item_ty, item_ty_expr) =
                                    check_expr(item, &Some(expected_item_ty.clone()), state);

                                if item_ty.is_error_ty() {
                                    had_item_error = true;
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
                                return (Ty::ErrorType { loc: loc.clone() }, None);
                            }

                            (
                                Ty::StaticArray {
                                    sub_ty: Box::new(sub_ty.clone_loc(loc.clone())),
                                    size: Some(Expr::Number {
                                        val: arr_size,
                                        loc: loc.clone(),
                                    }),
                                    loc: loc.clone(),
                                },
                                Some(TyExpr::StaticArray {
                                    vals: item_ty_exprs,
                                }),
                            )
                        }
                        _ => {
                            state.push_err(CheckerError::StaticArrayTypeInferenceFailed {
                                given_ty: ty.as_str(),
                                arr_loc: loc.clone(),
                            });
                            (Ty::ErrorType { loc: loc.clone() }, None)
                        }
                    }
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
                        return (Ty::ErrorType { loc: loc.clone() }, None);
                    }

                    let (expected_item_ty, first_ty_expr) = check_expr(&vals[0], context_ty, state);
                    if expected_item_ty.is_error_ty() {
                        return (Ty::ErrorType { loc: loc.clone() }, None);
                    }

                    let mut had_item_error = false;
                    let mut item_ty_exprs = vec![first_ty_expr.unwrap()];
                    for index in 1..vals.len() {
                        let cur_item = &vals[index];
                        let (cur_item_ty, cur_item_ty_expr) =
                            check_expr(cur_item, context_ty, state);

                        if cur_item_ty.is_error_ty() {
                            had_item_error = true;
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
                        return (Ty::ErrorType { loc: loc.clone() }, None);
                    }

                    (
                        Ty::StaticArray {
                            sub_ty: Box::new(expected_item_ty.clone_loc(loc.clone())),
                            size: Some(Expr::Number {
                                val: vals.len().to_string(),
                                loc: loc.clone(),
                            }),
                            loc: loc.clone(),
                        },
                        Some(TyExpr::StaticArray {
                            vals: item_ty_exprs,
                        }),
                    )
                }
            }
        }
        Expr::Ident { name, loc } => {
            // check to see if it is a builtin name
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
                    (Ty::ErrorType { loc: loc.clone() }, None)
                }
            }
        }
        Expr::BinOp {
            op,
            left,
            right,
            loc,
        } => {
            // we might be able to decide an order in which to check these
            // expressions. This will let us use other expressions to typecheck
            // numerical literals. For example:
            // ```
            // a : i8 : 20;
            // some_variable :: a + 1;
            // ```
            // Currently, this will throw an error since a is typed i8 and
            // 1 will be inferred to be int. But we can provide context for checking
            // 1 based on a. It will also apply and work if it is reversed and mixed into
            // other expressions with literals:
            // ```
            // a : i8 : 20;
            // some_variable :: 2 + (1 + a); // parsed into [2 + [1 + a]]
            // ```
            // Where we would have typechecked 2 to be int, we will typecheck the rhs
            // of the expression first to see if we can gather context to typecheck 2.
            // In [1 + a], we will also defer the checking of 1 since a is non-literal
            // and can provide context for checking 1. Since a is typed i8, 1 can be
            // checked to see if it can be typed i8. [1 + a] will be typed i8 and then
            // 2 can be checked.
            let lhs_is_first = match (left.is_literal(), right.is_literal()) {
                (true, true) | (false, true) => true,
                (true, false) => false,
                (false, false) => {
                    if left.get_non_literal_ranking() > right.get_non_literal_ranking() {
                        true
                    } else {
                        false
                    }
                }
            };

            let (mut l_ty, mut l_ty_expr) =
                check_expr(if lhs_is_first { left } else { right }, context_ty, state);
            if l_ty.is_error_ty() {
                return (Ty::ErrorType { loc: loc.clone() }, None);
            }
            let temp_l_ty = &Some(l_ty.clone());
            let (mut r_ty, mut r_ty_expr) = check_expr(
                if lhs_is_first { right } else { left },
                if context_ty.is_none() {
                    temp_l_ty
                } else {
                    context_ty
                },
                state,
            );
            if r_ty.is_error_ty() {
                return (Ty::ErrorType { loc: loc.clone() }, None);
            }

            if !lhs_is_first {
                let (temp_l_ty, temp_l_ty_expr) = (l_ty, l_ty_expr);
                (l_ty, l_ty_expr) = (r_ty, r_ty_expr);
                (r_ty, r_ty_expr) = (temp_l_ty, temp_l_ty_expr);
            }

            match op {
                BinOpType::Add
                | BinOpType::Sub
                | BinOpType::Mult
                | BinOpType::Div
                | BinOpType::Mod => match (&l_ty, &r_ty) {
                    (Ty::Signed { .. }, Ty::Signed { .. }) if types_are_eq(&l_ty, &r_ty) => (
                        l_ty.clone_loc(loc.clone()),
                        Some(TyExpr::BinOp {
                            op: *op,
                            lhs: Box::new(l_ty_expr.unwrap()),
                            rhs: Box::new(r_ty_expr.unwrap()),
                        }),
                    ),
                    (Ty::Unsigned { .. }, Ty::Unsigned { .. }) if types_are_eq(&l_ty, &r_ty) => (
                        l_ty.clone_loc(loc.clone()),
                        Some(TyExpr::BinOp {
                            op: *op,
                            lhs: Box::new(l_ty_expr.unwrap()),
                            rhs: Box::new(r_ty_expr.unwrap()),
                        }),
                    ),
                    (Ty::Char { .. }, Ty::Char { .. })
                    | (Ty::Str { .. }, Ty::Char { .. })
                    | (Ty::Str { .. }, Ty::Str { .. })
                        if matches!(op, BinOpType::Add) =>
                    {
                        (
                            Ty::Str {
                                loc: loc.clone(),
                                is_interp: false,
                            },
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
                        (Ty::ErrorType { loc: loc.clone() }, None)
                    }
                },
                BinOpType::And | BinOpType::Or => match (&l_ty, &r_ty) {
                    (Ty::Bool { .. }, Ty::Bool { .. }) => (
                        l_ty.clone_loc(loc.clone()),
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
                        (Ty::ErrorType { loc: loc.clone() }, None)
                    }
                },
                BinOpType::Eq
                | BinOpType::Neq
                | BinOpType::Gt
                | BinOpType::Lt
                | BinOpType::GtEq
                | BinOpType::LtEq => match (&l_ty, &r_ty) {
                    (Ty::Signed { .. }, Ty::Signed { .. })
                    | (Ty::Unsigned { .. }, Ty::Unsigned { .. })
                        if types_are_eq(&l_ty, &r_ty) && types_are_eq(&l_ty, &r_ty) =>
                    {
                        (
                            Ty::Bool { loc: loc.clone() },
                            Some(TyExpr::BinOp {
                                op: *op,
                                lhs: Box::new(l_ty_expr.unwrap()),
                                rhs: Box::new(r_ty_expr.unwrap()),
                            }),
                        )
                    }
                    (Ty::Char { .. }, Ty::Char { .. }) => (
                        Ty::Bool { loc: loc.clone() },
                        Some(TyExpr::BinOp {
                            op: *op,
                            lhs: Box::new(l_ty_expr.unwrap()),
                            rhs: Box::new(r_ty_expr.unwrap()),
                        }),
                    ),
                    (Ty::Bool { .. }, Ty::Bool { .. })
                        if matches!(op, BinOpType::Eq | BinOpType::Neq) =>
                    {
                        (
                            l_ty.clone_loc(loc.clone()),
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
                        (Ty::ErrorType { loc: loc.clone() }, None)
                    }
                },
            }
        }
        Expr::InitStruct {
            struct_name,
            fields,
            loc,
        } => todo!(),
        Expr::CallFn { func, args, loc } => {
            let (func_ty, func_ty_expr) = check_expr(func, &None, state);

            if func_ty.is_error_ty() {
                return (Ty::ErrorType { loc: loc.clone() }, None);
            }

            match func_ty {
                Ty::Func { params, ret, loc } => {
                    let fn_arity = params.len();
                    if fn_arity != args.len() {
                        state.push_err(CheckerError::IncorrectFunctionArity {
                            func: func.as_str(),
                            exp: fn_arity,
                            given: args.len(),
                            loc_given: loc.clone(),
                        });
                        return (Ty::ErrorType { loc: loc.clone() }, None);
                    }

                    let mut fn_ty_args = vec![];
                    for (param_ty, arg) in params.iter().zip(args.iter()) {
                        let (arg_ty, arg_ty_expr) = check_expr(arg, &Some(param_ty.clone()), state);

                        if !arg_ty.is_error_ty() && !types_are_eq(param_ty, &arg_ty) {
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
                        ret.clone_loc(loc.clone()),
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
                    return (Ty::ErrorType { loc: loc.clone() }, None);
                }
            }
        }
        Expr::UnaryOp { op, expr, loc } => {
            let (expr_ty, expr_ty_expr) = check_expr(expr, context_ty, state);

            match op {
                UnaryOpType::Not => {
                    if expr_ty.is_error_ty() {
                        return (Ty::Bool { loc: loc.clone() }, None);
                    }

                    if matches!(expr_ty, Ty::Bool { .. }) {
                        return (
                            expr_ty.clone_loc(loc.clone()),
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
                    (Ty::ErrorType { loc: loc.clone() }, None)
                }
                UnaryOpType::Negate => {
                    if expr_ty.is_error_ty() {
                        // we can just return an error type
                        return (Ty::ErrorType { loc: loc.clone() }, None);
                    }

                    if !expr_ty.is_signed_ty() {
                        state.push_err(CheckerError::InvalidUseOfUnaryOperator {
                            loc: loc.clone(),
                            op: op.as_str(),
                            operand: expr.as_str(),
                            tip: None,
                        });
                        return (Ty::ErrorType { loc: loc.clone() }, None);
                    }

                    (
                        expr_ty.clone_loc(loc.clone()),
                        Some(TyExpr::UnaryOp {
                            op: *op,
                            expr: Box::new(expr_ty_expr.unwrap()),
                        }),
                    )
                }
            }
        }
        Expr::ErrorExpr { msg, loc } => (Ty::ErrorType { loc: loc.clone() }, None),
        Expr::GroupedExpr { inner, loc } => {
            let (inner_ty, inner_ty_expr) = check_expr(inner, context_ty, state);
            if inner_ty.is_error_ty() {
                (Ty::ErrorType { loc: loc.clone() }, None)
            } else {
                (
                    inner_ty.clone_loc(loc.clone()),
                    Some(TyExpr::GroupedExpr {
                        inner: Box::new(inner_ty_expr.unwrap()),
                    }),
                )
            }
        }
        Expr::TernaryConditional {
            cond,
            then,
            otherwise,
            loc,
        } => {
            let (cond_ty, cond_ty_expr) = check_expr(cond, &None, state);

            // we expect the condition to be typed bool
            if !cond_ty.is_error_ty() && !matches!(cond_ty, Ty::Bool { .. }) {
                state.push_err(CheckerError::ConditionShouldBeTypedBool {
                    given_ty: cond_ty.as_str(),
                    loc: cond.get_source_ref(),
                });
                return (Ty::ErrorType { loc: loc.clone() }, None);
            }

            // typecheck then and otherwise expressions and make sure they have
            // the same type. remember to prioritize then or otherwise if we need
            // to extra context to typecheck the other expression, similar to binary
            // expressions.
            let (then_ty, then_ty_expr) = check_expr(then, context_ty, state);
            let (other_ty, other_ty_expr) = check_expr(otherwise, context_ty, state);

            if then_ty.is_error_ty() || other_ty.is_error_ty() {
                return (Ty::ErrorType { loc: loc.clone() }, None);
            }

            if !types_are_eq(&then_ty, &other_ty) {
                state.push_err(CheckerError::TypeMismatch {
                    loc: then.get_source_ref().combine(otherwise.get_source_ref()),
                    expected: then_ty.as_str(),
                    found: other_ty.as_str(),
                });
                return (Ty::ErrorType { loc: loc.clone() }, None);
            }

            (
                then_ty.clone_loc(loc.clone()),
                Some(TyExpr::TernaryConditional {
                    cond: Box::new(cond_ty_expr.unwrap()),
                    then: Box::new(then_ty_expr.unwrap()),
                    otherwise: Box::new(other_ty_expr.unwrap()),
                }),
            )
        }
        Expr::InterpStr { val, loc } => (
            Ty::Str {
                loc: loc.clone(),
                is_interp: true,
            },
            Some(TyExpr::Str { val: val.clone() }),
        ),
        Expr::InterpolatedString { parts, loc } => {
            // check each expression in parts and create a TyExpr from it with a
            // string type
            let mut new_ty_exprs = vec![];
            for part in parts.iter() {
                let (new_ty, mut new_ty_expr) = check_expr(part, &None, state);
                if !new_ty.is_error_ty() {
                    new_ty_expr = match new_ty {
                        Ty::Void { .. } => Some(TyExpr::MultiExpr {
                            exprs: vec![
                                new_ty_expr.unwrap(),
                                TyExpr::Str {
                                    val: "void".to_string(),
                                },
                            ],
                        }),
                        Ty::Str { is_interp, .. } => {
                            if is_interp {
                                Some(TyExpr::CallFn {
                                    func: Box::new(TyExpr::Ident {
                                        name: "str".to_string(),
                                    }),
                                    args: vec![new_ty_expr.unwrap()],
                                })
                            } else {
                                Some(TyExpr::CallFn {
                                    func: Box::new(TyExpr::Ident {
                                        name: "proto_str".to_string(),
                                    }),
                                    args: vec![TyExpr::CallFn {
                                        func: Box::new(TyExpr::Ident {
                                            name: "str".to_string(),
                                        }),
                                        args: vec![new_ty_expr.unwrap()],
                                    }],
                                })
                            }
                        }
                        Ty::Bool { .. }
                        | Ty::Char { .. }
                        | Ty::Signed { .. }
                        | Ty::Unsigned { .. }
                        | Ty::StaticArray { .. }
                        | Ty::Slice { .. }
                        | Ty::Optional { .. } => Some(TyExpr::CallFn {
                            func: Box::new(TyExpr::Ident {
                                name: "proto_str".to_string(),
                            }),
                            args: vec![new_ty_expr.unwrap()],
                        }),
                        _ => {
                            unreachable!(
                                "seman::check_expr(): {:#?} in string interpolation.",
                                new_ty
                            )
                        }
                    };
                    new_ty_exprs.push(new_ty_expr.unwrap());
                } else {
                    return (Ty::ErrorType { loc: loc.clone() }, None);
                }
            }

            (
                Ty::Str {
                    loc: loc.clone(),
                    is_interp: false,
                },
                Some(TyExpr::InterpolatedString {
                    parts: new_ty_exprs,
                }),
            )
        }
        Expr::MakeSlice {
            target,
            start,
            end_excl,
            loc,
        } => {
            let (target_ty, target_ty_expr) = check_expr(target, &None, state);
            if target_ty.is_error_ty() {
                return (Ty::ErrorType { loc: loc.clone() }, None);
            }

            if !target_ty.is_sliceable() {
                state.push_err(CheckerError::ExpectedArrayOrSlice {
                    given_ty: target_ty.as_str(),
                    loc: target_ty.get_loc(),
                });
                return (Ty::ErrorType { loc: loc.clone() }, None);
            }

            // check both start and end_excl expressions, if they are provided
            let (start_ty, start_ty_expr) = match start {
                Some(start) => {
                    let (a, b) =
                        check_expr(start, &Some(Ty::get_uint_ty(start.get_source_ref())), state);
                    if a.is_error_ty() {
                        return (Ty::ErrorType { loc: loc.clone() }, None);
                    }

                    if !a.is_unsigned_ty() {
                        state.push_err(CheckerError::TypeMismatch {
                            loc: start.get_source_ref(),
                            expected: "uint".into(),
                            found: a.as_str(),
                        });
                        return (Ty::ErrorType { loc: loc.clone() }, None);
                    };

                    (a, b.unwrap())
                }
                None => (
                    Ty::get_uint_ty(target.get_source_ref()),
                    TyExpr::Integer { val: "0".into() },
                ),
            };

            match end_excl {
                Some(end_excl) => {
                    let (end_ty, end_ty_expr) = check_expr(end_excl,
                        &Some(Ty::get_uint_ty(end_excl.get_source_ref())), state);
                    if end_ty.is_error_ty() {
                        return (Ty::ErrorType { loc: loc.clone() }, None);
                    }

                    if !end_ty.is_unsigned_ty() {
                        state.push_err(CheckerError::TypeMismatch {
                            loc: end_excl.get_source_ref(),
                            expected: "uint".into(),
                            found: end_ty.as_str(),
                        });
                        return (Ty::ErrorType { loc: loc.clone() }, None);
                    }

                    match target_ty {
                        Ty::StaticArray { sub_ty, .. } => {
                            let slice_ty = Ty::Slice { sub_ty: Box::new(sub_ty.clone_loc(loc.clone())), loc: loc.clone() };
                            if let Some(context_ty) = context_ty {
                                if !types_are_eq(context_ty, &slice_ty) {
                                    state.push_err(CheckerError::TypeMismatch {
                                        loc: loc.clone(),
                                        expected: context_ty.as_str(),
                                        found: slice_ty.as_str()
                                    });
                                    return (Ty::ErrorType { loc: loc.clone() }, None);
                                }
                            }
                            (slice_ty, Some(TyExpr::MakeSliceWithEnd {
                                target: Box::new(target_ty_expr.unwrap()),
                                start: Box::new(start_ty_expr),
                                end_excl: Box::new(end_ty_expr.unwrap())
                            }))
                        }
                        Ty::Slice { sub_ty, .. } => {
                            let slice_ty = Ty::Slice { sub_ty: Box::new(sub_ty.clone_loc(loc.clone())), loc: loc.clone() };
                            if let Some(context_ty) = context_ty {
                                if !types_are_eq(context_ty, &slice_ty) {
                                    state.push_err(CheckerError::TypeMismatch {
                                        loc: loc.clone(),
                                        expected: context_ty.as_str(),
                                        found: slice_ty.as_str()
                                    });
                                    return (Ty::ErrorType { loc: loc.clone() }, None);
                                }
                            }

                            (slice_ty, Some(TyExpr::MakeSliceWithEnd {
                                target: Box::new(target_ty_expr.unwrap()),
                                start: Box::new(start_ty_expr),
                                end_excl: Box::new(end_ty_expr.unwrap())
                            }))
                        }
                        _ => unreachable!("seman::check_expr(): while checking exclusive end of MakeSlice expr, {:#?} made it through", target_ty)
                    }
                }
                None =>
                    match target_ty {
                        Ty::StaticArray { sub_ty, size, .. } => {
                            let end_ty_expr = TyExpr::Integer {
                                val: size.as_ref().unwrap().as_str(),
                            };
                            let slice_ty = Ty::Slice { sub_ty: Box::new(sub_ty.clone_loc(loc.clone())), loc: loc.clone() };
                            if let Some(context_ty) = context_ty {
                                if !types_are_eq(context_ty, &slice_ty) {
                                    state.push_err(CheckerError::TypeMismatch {
                                        loc: loc.clone(),
                                        expected: context_ty.as_str(),
                                        found: slice_ty.as_str()
                                    });
                                    return (Ty::ErrorType { loc: loc.clone() }, None);
                                }
                            }
                            (slice_ty, Some(TyExpr::MakeSliceWithEnd {
                                target: Box::new(target_ty_expr.unwrap()),
                                start: Box::new(start_ty_expr),
                                end_excl: Box::new(end_ty_expr)
                            }))
                        }
                        Ty::Slice { sub_ty, .. } => {
                            let slice_ty = Ty::Slice { sub_ty: Box::new(sub_ty.clone_loc(loc.clone())), loc: loc.clone() };
                            if let Some(context_ty) = context_ty {
                                if !types_are_eq(context_ty, &slice_ty) {
                                    state.push_err(CheckerError::TypeMismatch {
                                        loc: loc.clone(),
                                        expected: context_ty.as_str(),
                                        found: slice_ty.as_str()
                                    });
                                    return (Ty::ErrorType { loc: loc.clone() }, None);
                                }
                            }
                            (slice_ty, Some(TyExpr::MakeSliceFrom {
                                target: Box::new(target_ty_expr.unwrap()),
                                start: Box::new(start_ty_expr)
                            }))
                        }
                        _ => unreachable!("seman::check_expr(): while checking exclusive end of MakeSlice expr, {:#?} made it through", target_ty),
                    }
            }
        }
        Expr::IndexInto { target, index, loc } => {
            // target has to be one of: array, slice, string
            // index must be usize.
            let (target_ty, target_ty_expr) = check_expr(target, &None, state);
            if target_ty.is_error_ty() {
                return (Ty::ErrorType { loc: loc.clone() }, None);
            }

            if target_ty.is_indexable() {
                state.push_err(CheckerError::IndexIntoOpRequiresArraySliceOrString {
                    given_ty: target_ty.as_str(),
                    loc: target.get_source_ref(),
                });
                return (Ty::ErrorType { loc: loc.clone() }, None);
            }

            let (index_ty, index_ty_expr) =
                check_expr(index, &Some(Ty::get_uint_ty(index.get_source_ref())), state);
            if index_ty.is_error_ty() {
                return (Ty::ErrorType { loc: loc.clone() }, None);
            }
            if index_ty.is_unsigned_ty() {
                state.push_err(CheckerError::TypeMismatch {
                    loc: index.get_source_ref(),
                    expected: "uint".into(),
                    found: index_ty.as_str(),
                });
                return (Ty::ErrorType { loc: loc.clone() }, None);
            };

            let new_ty = match target_ty {
                Ty::Str { .. } => Ty::Char { loc: loc.clone() },
                Ty::StaticArray { sub_ty, .. } | Ty::Slice { sub_ty, .. } => {
                    let sub_ty = sub_ty.clone_loc(loc.clone());
                    sub_ty
                }
                _ => unreachable!(
                    "seman::check_expr(): Found unexpected type for index to expression: {:#?}",
                    target_ty
                ),
            };

            (
                new_ty,
                Some(TyExpr::IndexInto {
                    target: Box::new(target_ty_expr.unwrap()),
                    index: Box::new(index_ty_expr.unwrap()),
                }),
            )
        }
        Expr::AccessMember { target, mem, loc } => {
            // support for Slice, Array, Option and String methods
            let (target_ty, target_ty_expr) = check_expr(target, &None, state);
            if target_ty.is_error_ty() {
                return (Ty::ErrorType { loc: loc.clone() }, None);
            }

            match type_is_valid(&target_ty) {
                Ok(_) => {}
                Err((e, e_loc)) => match e {
                    TypeValidErr::Invalid => {
                        state.push_err(CheckerError::InvalidType {
                            loc: e_loc,
                            type_name: target_ty.as_str(),
                        });
                        return (Ty::ErrorType { loc: loc.clone() }, None);
                    }
                    TypeValidErr::Incomplete => {
                        state.push_err(CheckerError::IncompleteType {
                            loc: e_loc,
                            type_name: target_ty.as_str(),
                        });
                        return (Ty::ErrorType { loc: loc.clone() }, None);
                    }
                },
            }

            let builtin_loc = SourceRef {
                file: "__proto_gen_builtins__".to_string(),
                start_line: 0,
                start_col: 0,
                end_line: 0,
                end_col: 0,
                flat_start: 0,
                flat_end: 0,
            };

            match &target_ty {
                Ty::Str { .. } => {
                    let mut methods: HashMap<&str, Ty> = HashMap::from([(
                        "len",
                        Ty::Func {
                            params: vec![],
                            ret: Box::new(Ty::get_uint_ty(builtin_loc.clone())),
                            loc: builtin_loc,
                        },
                    )]);
                    // check if it is one of the methods on the type
                    let mem_str = mem.as_str();
                    match methods.get(mem_str.as_str()) {
                        Some(meth_ty) => (
                            meth_ty.clone(),
                            Some(TyExpr::AccessMember {
                                target: Box::new(target_ty_expr.unwrap()),
                                mem: Box::new(TyExpr::Ident {
                                    name: if mem_str == "len" {
                                        "size".into()
                                    } else {
                                        mem_str
                                    },
                                }),
                            }),
                        ),
                        None => {
                            state.push_err(CheckerError::MemberDoesNotExist {
                                given_ty: "str".into(),
                                mem: mem_str,
                                loc: loc.clone(),
                            });
                            (Ty::ErrorType { loc: loc.clone() }, None)
                        }
                    }
                }
                Ty::StaticArray { sub_ty, .. } | Ty::Slice { sub_ty, .. } => {
                    let mut methods: HashMap<&str, Ty> = HashMap::from([(
                        "len",
                        Ty::Func {
                            params: vec![],
                            ret: Box::new(Ty::get_uint_ty(builtin_loc.clone())),
                            loc: builtin_loc.clone(),
                        },
                    )]);
                    methods.insert(
                        "get",
                        Ty::Func {
                            params: vec![Ty::get_uint_ty(builtin_loc.clone())],
                            ret: Box::new(Ty::Optional {
                                sub_ty: Box::new(sub_ty.clone_loc(builtin_loc.clone())),
                                loc: builtin_loc.clone(),
                            }),
                            loc: builtin_loc.clone(),
                        },
                    );
                    methods.insert(
                        "make_slice",
                        Ty::Func {
                            params: vec![
                                Ty::get_uint_ty(builtin_loc.clone()),
                                Ty::get_uint_ty(builtin_loc.clone()),
                            ],
                            ret: Box::new(Ty::Slice {
                                sub_ty: Box::new(sub_ty.clone_loc(builtin_loc.clone())),
                                loc: builtin_loc.clone(),
                            }),
                            loc: builtin_loc.clone(),
                        },
                    );
                    methods.insert(
                        "make_slice_from",
                        Ty::Func {
                            params: vec![Ty::get_uint_ty(builtin_loc.clone())],
                            ret: Box::new(Ty::Slice {
                                sub_ty: Box::new(sub_ty.clone_loc(builtin_loc.clone())),
                                loc: builtin_loc.clone(),
                            }),
                            loc: builtin_loc,
                        },
                    );
                    // check if it is one of the methods on the type
                    let mem_str = mem.as_str();
                    match methods.get(mem_str.as_str()) {
                        Some(meth_ty) => (
                            meth_ty.clone(),
                            Some(TyExpr::AccessMember {
                                target: Box::new(target_ty_expr.unwrap()),
                                mem: Box::new(TyExpr::Ident { name: mem_str }),
                            }),
                        ),
                        None => {
                            state.push_err(CheckerError::MemberDoesNotExist {
                                given_ty: target_ty.as_str(),
                                mem: mem_str,
                                loc: loc.clone(),
                            });
                            (Ty::ErrorType { loc: loc.clone() }, None)
                        }
                    }
                }
                Ty::Optional { sub_ty, .. } => {
                    let mut methods = HashMap::from([
                        (
                            "is_some",
                            Ty::Func {
                                params: vec![],
                                ret: Box::new(Ty::Bool {
                                    loc: builtin_loc.clone(),
                                }),
                                loc: builtin_loc.clone(),
                            },
                        ),
                        (
                            "is_none",
                            Ty::Func {
                                params: vec![],
                                ret: Box::new(Ty::Bool {
                                    loc: builtin_loc.clone(),
                                }),
                                loc: builtin_loc.clone(),
                            },
                        ),
                    ]);
                    methods.insert(
                        "unwrap",
                        Ty::Func {
                            params: vec![],
                            ret: Box::new(sub_ty.clone_loc(builtin_loc.clone())),
                            loc: builtin_loc,
                        },
                    );

                    // check if it is one of the methods on the type
                    let mem_str = mem.as_str();
                    match methods.get(mem_str.as_str()) {
                        Some(meth_ty) => (
                            meth_ty.clone(),
                            Some(TyExpr::AccessMember {
                                target: Box::new(target_ty_expr.unwrap()),
                                mem: Box::new(TyExpr::Ident { name: mem_str }),
                            }),
                        ),
                        None => {
                            state.push_err(CheckerError::MemberDoesNotExist {
                                given_ty: target_ty.as_str(),
                                mem: mem_str,
                                loc: loc.clone(),
                            });
                            (Ty::ErrorType { loc: loc.clone() }, None)
                        }
                    }
                }
                _ => {
                    state.push_err(CheckerError::AccessMemberOpCannotBePerformedOnType {
                        given_ty: target_ty.as_str(),
                        loc: target.get_source_ref(),
                    });
                    (Ty::ErrorType { loc: loc.clone() }, None)
                }
            }
        }
        Expr::OptionalExpr { val, loc } => {
            let (v_ty, v_ty_expr) = match (context_ty, val) {
                (Some(ty), Some(val)) => match ty {
                    Ty::Optional { sub_ty, loc } => {
                        let expected_item_ty = *sub_ty.clone();
                        check_expr(val, &Some(expected_item_ty), state)
                    }
                    _ => {
                        state.push_err(CheckerError::OptionalTypeInferenceFailed {
                            given_ty: ty.as_str(),
                            opt_loc: loc.clone(),
                        });
                        return (Ty::ErrorType { loc: loc.clone() }, None);
                    }
                },
                (None, Some(val)) => check_expr(val, context_ty, state),
                (Some(ty), None) => match ty {
                    Ty::Optional { sub_ty, loc } => {
                        let opt_ty = Ty::Optional {
                            sub_ty: sub_ty.clone(),
                            loc: loc.clone(),
                        };
                        return (opt_ty, Some(TyExpr::OptionalExpr { val: None }));
                    }
                    _ => {
                        state.push_err(CheckerError::OptionalTypeInferenceFailed {
                            given_ty: ty.as_str(),
                            opt_loc: loc.clone(),
                        });
                        return (Ty::ErrorType { loc: loc.clone() }, None);
                    }
                },
                (None, None) => {
                    state.push_err(
                        CheckerError::OptionalTypeInferenceFailedWithoutContextualTy {
                            opt_loc: loc.clone(),
                        },
                    );
                    return (Ty::ErrorType { loc: loc.clone() }, None);
                }
            };

            if v_ty.is_error_ty() {
                return (Ty::ErrorType { loc: loc.clone() }, None);
            }

            let opt_ty = Ty::Optional {
                sub_ty: Box::new(v_ty),
                loc: loc.clone(),
            };
            (
                opt_ty,
                Some(TyExpr::OptionalExpr {
                    val: Some(Box::new(v_ty_expr.unwrap())),
                }),
            )
        }
    }
}

pub fn check_ins_for_return(i: &Ins, context_ty: &Option<Ty>, state: &mut State) -> Option<TyIns> {
    let ret_ty = context_ty.as_ref().unwrap();
    if !ret_ty.is_void() {
        if i.contains_sub_instructions() {
            // we will leave the checking to the sub instruction
            state.check_for_return = true;
            check_ins(i, context_ty, state)
        } else {
            // we will check the instruction and then record an error
            // since we expect a return here
            let ty_ins = check_ins(i, context_ty, state);
            state.push_err(CheckerError::Expected(
                format!(
                    "1. a return statement with a value of type '{}'.",
                    ret_ty.as_str()
                ),
                i.get_source_ref(),
                None,
            ));
            ty_ins
        }
    } else {
        check_ins(i, context_ty, state)
    }
}

pub fn check_ins(i: &Ins, context_ty: &Option<Ty>, state: &mut State) -> Option<TyIns> {
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
            // println!("expr_ty_expr: {expr_ty_expr:#?}, \n{:#?}", state.errs);
            let ty_ins = if expr_ty.is_error_ty() {
                None
            } else {
                if let Some(ty) = ty {
                    if !types_are_eq(ty, &expr_ty) {
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
                } else {
                    Some(TyIns::Constant {
                        name: name.as_str(),
                        ty: expr_ty.clone(),
                        init: expr_ty_expr.unwrap(),
                    })
                }
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
                (Some(inner), Some(expr)) => {
                    // TODO: verify that ty is an actual known type (builtin)
                    // or user defined
                    let (expr_ty, expr_ty_expr) = check_expr(expr, ty, state);
                    let ty_ins = if expr_ty.is_error_ty() {
                        None
                    } else {
                        if !types_are_eq(inner, &expr_ty) {
                            state.push_err(CheckerError::TypeMismatch {
                                loc: loc.clone(),
                                expected: inner.as_str(),
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
                    let var_ty = if type_is_valid(ty).is_err() {
                        Ty::ErrorType { loc: loc.clone() }
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

                    let ty_ins = if expr_ty.is_error_ty() {
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
        Ins::DeclMethod {
            inst_name,
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
            let ret_ty = match type_is_valid(ret_type) {
                Ok(_) => ret_type.clone(),
                Err((e, e_loc)) => match e {
                    TypeValidErr::Invalid => {
                        state.push_err(CheckerError::InvalidType {
                            loc: e_loc,
                            type_name: ret_type.as_str(),
                        });
                        Ty::ErrorType {
                            loc: ret_type.get_loc(),
                        }
                    }
                    TypeValidErr::Incomplete => {
                        state.push_err(CheckerError::IncompleteType {
                            loc: e_loc,
                            type_name: ret_type.as_str(),
                        });
                        Ty::ErrorType {
                            loc: ret_type.get_loc(),
                        }
                    }
                },
            };

            if ret_ty.is_error_ty() {
                return None;
            }

            let mut fn_type_params = vec![];
            let mut pairs_to_register = vec![];
            for param in params.into_iter() {
                let p_ty = match type_is_valid(&param.given_ty) {
                    Ok(_) => param.given_ty.clone(),
                    Err((e, e_loc)) => match e {
                        TypeValidErr::Invalid => {
                            state.push_err(CheckerError::InvalidType {
                                loc: e_loc,
                                type_name: param.given_ty.as_str(),
                            });
                            Ty::ErrorType {
                                loc: param.given_ty.get_loc(),
                            }
                        }
                        TypeValidErr::Incomplete => {
                            state.push_err(CheckerError::IncompleteType {
                                loc: e_loc,
                                type_name: param.given_ty.as_str(),
                            });
                            Ty::ErrorType {
                                loc: param.given_ty.get_loc(),
                            }
                        }
                    },
                };
                fn_type_params.push(p_ty.clone());
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

            let fn_type = Ty::Func {
                params: fn_type_params,
                ret: Box::new(ret_ty.clone()),
                loc: loc.clone(),
            };

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
            let old_check_for_return = state.check_for_return;
            state.check_for_return = true;
            let new_body = if i.contains_sub_instructions() {
                check_ins(body, &Some(ret_type.clone()), state)
            } else {
                let ty_i = check_ins(body, &Some(ret_type.clone()), state);
                if !ret_ty.is_void() {
                    state.push_err(CheckerError::Expected(
                        "2. a return statement with a value of type '{}'.".into(),
                        loc.clone(),
                        None,
                    ));
                }
                ty_i
            };

            if new_body.is_none() {
                return None;
            }

            let new_body = new_body.unwrap();

            // reset the scope info
            state.enter_new_scope = old_enter_new_scope;
            state.check_for_return = old_check_for_return;
            state.scope_stack.pop();
            state.env.pop();
            Some(TyIns::Func {
                name: name.as_str(),
                params: ty_params,
                ret_ty: ret_type.clone(),
                body: Box::new(new_body),
            })
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
            let ret_ty = match type_is_valid(ret_type) {
                Ok(_) => ret_type.clone(),
                Err((e, e_loc)) => match e {
                    TypeValidErr::Invalid => {
                        state.push_err(CheckerError::InvalidType {
                            loc: e_loc,
                            type_name: ret_type.as_str(),
                        });
                        Ty::ErrorType {
                            loc: ret_type.get_loc(),
                        }
                    }
                    TypeValidErr::Incomplete => {
                        state.push_err(CheckerError::IncompleteType {
                            loc: e_loc,
                            type_name: ret_type.as_str(),
                        });
                        Ty::ErrorType {
                            loc: ret_type.get_loc(),
                        }
                    }
                },
            };

            if ret_ty.is_error_ty() {
                return None;
            }

            let mut fn_type_params = vec![];
            let mut pairs_to_register = vec![];
            for param in params.into_iter() {
                let p_ty = match type_is_valid(&param.given_ty) {
                    Ok(_) => param.given_ty.clone(),
                    Err((e, e_loc)) => match e {
                        TypeValidErr::Invalid => {
                            state.push_err(CheckerError::InvalidType {
                                loc: e_loc,
                                type_name: param.given_ty.as_str(),
                            });
                            Ty::ErrorType {
                                loc: param.given_ty.get_loc(),
                            }
                        }
                        TypeValidErr::Incomplete => {
                            state.push_err(CheckerError::IncompleteType {
                                loc: e_loc,
                                type_name: param.given_ty.as_str(),
                            });
                            Ty::ErrorType {
                                loc: param.given_ty.get_loc(),
                            }
                        }
                    },
                };
                fn_type_params.push(p_ty.clone());
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

            let fn_type = Ty::Func {
                params: fn_type_params,
                ret: Box::new(ret_ty.clone()),
                loc: loc.clone(),
            };

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
            let old_check_for_return = state.check_for_return;
            state.check_for_return = true;
            let new_body = if body.contains_sub_instructions() {
                check_ins(body, &Some(ret_type.clone()), state)
            } else {
                let ty_i = check_ins(body, &Some(ret_type.clone()), state);
                if !ret_ty.is_void() {
                    state.push_err(CheckerError::Expected(
                        format!(
                            "3. a return statement with a value of type '{}'.",
                            ret_ty.as_str()
                        ),
                        loc.clone(),
                        None,
                    ));
                }
                ty_i
            };

            if new_body.is_none() {
                return None;
            }

            let new_body = new_body.unwrap();

            // reset the scope info
            state.enter_new_scope = old_enter_new_scope;
            state.check_for_return = old_check_for_return;
            state.scope_stack.pop();
            state.env.pop();
            Some(TyIns::Func {
                name: name.as_str(),
                params: ty_params,
                ret_ty: ret_type.clone(),
                body: Box::new(new_body),
            })
        }
        Ins::IfConditional {
            conds_and_code,
            loc,
        } => {
            // we will check each of the pairs types
            let old_enter_new_scope = state.enter_new_scope;
            state.enter_new_scope = true;
            let mut new_conds_and_code = vec![];
            for (i, (cond, code)) in conds_and_code.iter().enumerate() {
                let cond_ty_expr = if let Some(cond) = cond {
                    // we will type check the condition and then
                    // make sure its type is bool
                    let (cond_ty, cond_ty_expr) = check_expr(cond, context_ty, state);
                    if cond_ty.is_error_ty() {
                        None
                    } else if !matches!(cond_ty, Ty::Bool { .. }) {
                        state.push_err(CheckerError::ConditionShouldBeTypedBool {
                            given_ty: cond_ty.as_str(),
                            loc: cond.get_source_ref(),
                        });
                        None
                    } else {
                        cond_ty_expr
                    }
                } else {
                    None
                };

                let ty_body = check_ins(code, context_ty, state);
                if let Some(ty_body) = ty_body {
                    new_conds_and_code.push((cond_ty_expr, ty_body));
                }

                if state.check_for_return && i == conds_and_code.len() - 1 {
                    // if we are required to check for a return type, we have to
                    // make sure a default case (an else) is provided
                    if cond.is_some() {
                        state.push_err(CheckerError::Expected(
                            format!(
                                "a following else branch that returns a value of type '{}'.",
                                context_ty.as_ref().unwrap().as_str()
                            ),
                            code.get_source_ref(),
                            None,
                        ));
                    }
                }
            }
            state.enter_new_scope = old_enter_new_scope;

            Some(TyIns::IfConditional {
                comb: new_conds_and_code,
            })
        }
        Ins::DeclStruct { name, body, loc } => {
            todo!()
        }
        Ins::DeclModule { name, body, loc } => todo!(),
        Ins::Block { code, loc } => {
            if state.enter_new_scope {
                state.scope_stack.push(Scope::Block);
                state.env.extend();
            }

            if state.check_for_return && code.len() == 0 && !context_ty.as_ref().unwrap().is_void()
            {
                state.push_err(CheckerError::Expected(
                    format!(
                        "4. a return statement with a value of type '{}'.",
                        context_ty.as_ref().unwrap().as_str()
                    ),
                    loc.clone(),
                    None,
                ));
            }
            let old_check_for_return = state.check_for_return;
            state.check_for_return = false;
            let mut new_code = vec![];
            let old_enter_new_scope = state.enter_new_scope;
            state.enter_new_scope = true;
            for (i, s_ins) in code.iter().enumerate() {
                let new_ins = if i == code.len() - 1 && old_check_for_return {
                    check_ins_for_return(s_ins, context_ty, state)
                } else {
                    check_ins(s_ins, context_ty, state)
                };

                if new_ins.is_some() {
                    new_code.push(new_ins.unwrap())
                }
            }

            if old_enter_new_scope {
                state.scope_stack.pop();
                state.env.pop();
            }

            state.enter_new_scope = old_enter_new_scope;
            state.check_for_return = old_check_for_return;

            Some(TyIns::Block { code: new_code })
        }
        Ins::AssignTo { target, value, loc } => todo!(),
        Ins::ExprIns { expr, loc } => {
            let (expr_ty, expr_ty_expr) = check_expr(expr, &None, state);

            if expr_ty.is_error_ty() {
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
                    if !expr_ty.is_error_ty() && matches!(context_ty, Ty::Void { .. }) {
                        state.push_err(CheckerError::MismatchingReturnType {
                            exp: "void".to_string(),
                            given: expr_ty.as_str(),
                            loc_given: loc.clone(),
                        });
                        return None;
                    }

                    if !expr_ty.is_error_ty() && !types_are_eq(&expr_ty, context_ty) {
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
                    if !matches!(context_ty, Ty::Void { .. }) {
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
        Ins::PrintIns {
            is_println,
            output,
            loc,
        } => {
            // check the output to make sure it is a string
            let (output_ty, output_ty_expr) = check_expr(output, &None, state);

            if output_ty.is_error_ty() {
                return None;
            }

            if !matches!(output_ty, Ty::Str { .. }) {
                state.push_err(CheckerError::PrintRequiresAStringArg {
                    is_println: *is_println,
                    given_ty: output_ty.as_str(),
                    loc: loc.clone(),
                });
                return None;
            }

            Some(TyIns::PrintIns {
                is_println: *is_println,
                output: output_ty_expr.unwrap(),
            })
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
