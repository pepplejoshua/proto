#![allow(unused)]

use std::{cell::RefCell, collections::HashMap, process::exit, rc::Rc, sync::Once};

use crate::{
    codegen::tast::{TyExpr, TyFileModule, TyFnParam, TyHashMapPair, TyIns},
    parser::ast::{BinOpType, Expr, FileModule, Ins, UnaryOpType},
    source::{
        errors::CheckerError,
        source::{SourceFile, SourceRef, SourceReporter},
    },
    types::signature::Ty,
};

#[derive(Debug, Clone)]
pub struct NameInfo {
    ty: Rc<RefCell<Ty>>,
    refs: Vec<Rc<SourceRef>>,
    is_const: bool,
    is_initialized: bool,
}

#[derive(Debug, Clone)]
struct TypeEnv {
    vars: Vec<HashMap<String, NameInfo>>,
}

impl TypeEnv {
    fn new() -> Self {
        TypeEnv {
            vars: vec![HashMap::new()],
        }
    }

    fn extend(&mut self) {
        self.vars.push(HashMap::new());
    }

    fn pop(&mut self) {
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
    Loop,
    Defer,
    Mod,
    Trait,
}

impl Scope {
    pub fn is_major_scope(&self) -> bool {
        match self {
            Scope::Block => false,
            _ => true,
        }
    }
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

    pub fn first_major_scope(&self) -> Scope {
        for scope in self.scope_stack.iter().rev() {
            if scope.is_major_scope() {
                return *scope;
            }
        }
        self.scope_stack[0]
    }

    pub fn x_scope_index(&self, x: &Scope) -> Option<usize> {
        for (index, scope) in self.scope_stack.iter().rev().enumerate() {
            if scope == x {
                return Some(index);
            }
        }

        None
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

static BUILTIN_INIT: Once = Once::new();
static mut BUILTIN_HASHMAP: Option<HashMap<&'static str, Rc<Ty>>> = None;

fn init_builtins() {
    let mut m = HashMap::new();
    let builtin_loc = Rc::new(SourceRef {
        file: Rc::new("__proto_gen_builtins__".to_string()),
        start_line: 0,
        start_col: 0,
        end_line: 0,
        end_col: 0,
        flat_start: 0,
        flat_end: 0,
    });
    m.insert(
        "make_char",
        Rc::new(Ty::Func {
            params: vec![Ty::get_int_ty(builtin_loc.clone())],
            ret: Rc::new(Ty::Char {
                loc: builtin_loc.clone(),
            }),
            loc: builtin_loc.clone(),
            is_const: false,
        }),
    );
    unsafe {
        BUILTIN_HASHMAP = Some(m);
    }
}

fn get_builtin_methods() -> &'static HashMap<&'static str, Rc<Ty>> {
    unsafe {
        BUILTIN_INIT.call_once(init_builtins);
        BUILTIN_HASHMAP.as_ref().unwrap()
    }
}

enum TypeValidErr {
    Invalid,
    Incomplete,
}

fn type_is_valid(ty: &Ty) -> Result<(), (TypeValidErr, Rc<SourceRef>)> {
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
        Ty::HashMap { key_ty, val_ty, .. } => type_is_valid(key_ty).and(type_is_valid(val_ty)),
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
        (Ty::Float { size, .. }, Ty::Float { size: b_size, .. }) => *size == *b_size,
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
        (Ty::Trait { name, .. }, Ty::Trait { name: b_name, .. })
        | (Ty::NamedType { name, .. }, Ty::Trait { name: b_name, .. })
        | (Ty::Trait { name, .. }, Ty::NamedType { name: b_name, .. })
        | (Ty::Struct { name, .. }, Ty::Struct { name: b_name, .. }) => name == b_name,
        (Ty::NamedType { name, .. }, Ty::Struct { name: s_name, .. })
        | (Ty::Struct { name: s_name, .. }, Ty::NamedType { name, .. })
        | (Ty::NamedType { name, .. }, Ty::NamedType { name: s_name, .. }) => name == s_name,
        (
            Ty::Func { params, ret, .. },
            Ty::Func {
                params: b_params,
                ret: b_ret,
                ..
            },
        ) => {
            if params.len() != b_params.len() && !types_are_eq(ret, b_ret) {
                return false;
            }

            for (pty, b_pty) in params.iter().zip(b_params.iter()) {
                if !types_are_eq(pty, b_pty) {
                    return false;
                }
            }

            return true;
        }
        (
            Ty::Pointer { sub_ty, .. },
            Ty::Pointer {
                sub_ty: b_sub_ty, ..
            },
        ) => types_are_eq(sub_ty, b_sub_ty),
        (
            Ty::HashMap { key_ty, val_ty, .. },
            Ty::HashMap {
                key_ty: b_key_ty,
                val_ty: b_val_ty,
                ..
            },
        ) => types_are_eq(key_ty, b_key_ty) && types_are_eq(val_ty, b_val_ty),
        (_, Ty::ErrorType { .. }) | (Ty::ErrorType { .. }, _) | _ => false,
    }
}

fn can_access_method(target_is_const: bool, method: &Ty) -> bool {
    match method {
        Ty::Func { is_const, .. } => {
            if target_is_const {
                // target is const and method is const, it can be accessed
                // if method is not const, it can't be accessed
                *is_const
            } else {
                true
            }
        }
        _ => unreachable!(),
    }
}

fn check_for_member(target_is_const: bool, target_ty: &Ty, mem: &str, state: &mut State) -> Rc<Ty> {
    let builtin_loc = Rc::new(SourceRef {
        file: Rc::new("__proto_gen_builtins__".to_string()),
        start_line: 0,
        start_col: 0,
        end_line: 0,
        end_col: 0,
        flat_start: 0,
        flat_end: 0,
    });

    match target_ty {
        Ty::Pointer { sub_ty, loc } => {
            if sub_ty.is_pointer() {
                return Rc::new(Ty::ErrorType { loc: loc.clone() });
            }

            check_for_member(
                target_is_const,
                &sub_ty.clone_loc(target_ty.get_loc()),
                mem,
                state,
            )
        }
        Ty::NamedType { name, loc } => {
            // we will get the underlying type and then check that
            let n_info = state.env.lookup(name);

            if n_info.is_none() {
                state.push_err(CheckerError::InvalidType {
                    loc: target_ty.get_loc(),
                    type_name: name.clone(),
                });
                return Rc::new(Ty::ErrorType { loc: loc.clone() });
            }

            // we want to find the init() void function in the Struct
            let actual_ty_info = n_info.unwrap().clone();
            let actual_ty = &*actual_ty_info.ty.borrow();

            check_for_member(
                target_is_const,
                &actual_ty.clone_loc(target_ty.get_loc()),
                mem,
                state,
            )
        }
        Ty::Struct {
            fields, funcs, loc, ..
        } => {
            // we will check fields, assoc_funcs and methods
            let field_ty = fields.get(mem);
            if let Some(field_ty) = field_ty {
                return field_ty.clone();
            }

            let func_ty = funcs.get(mem);
            if let Some(func_ty) = func_ty {
                // determine if we are accessing a non-const function on a const type
                if can_access_method(target_is_const, func_ty) {
                    return func_ty.clone();
                } else {
                    state.push_err(CheckerError::CannotAccessNonConstFuncOnConstTarget {
                        loc: target_ty.get_loc(),
                    });
                    return Rc::new(Ty::ErrorType { loc: loc.clone() });
                }
            }

            state.push_err(CheckerError::MemberDoesNotExist {
                given_ty: target_ty.as_str(),
                mem: mem.to_string(),
                loc: loc.clone(),
            });
            Rc::new(Ty::ErrorType { loc: loc.clone() })
        }
        Ty::HashMap {
            key_ty,
            val_ty,
            loc,
        } => {
            let mut methods: HashMap<&str, Rc<Ty>> = HashMap::from([(
                "len",
                Rc::new(Ty::Func {
                    params: vec![],
                    ret: Ty::get_uint_ty(builtin_loc.clone()),
                    loc: builtin_loc.clone(),
                    is_const: true,
                }),
            )]);

            methods.insert(
                "get",
                Rc::new(Ty::Func {
                    params: vec![key_ty.clone_loc(builtin_loc.clone())],
                    ret: Rc::new(Ty::Optional {
                        sub_ty: val_ty.clone_loc(builtin_loc.clone()),
                        loc: builtin_loc.clone(),
                    }),
                    loc: builtin_loc.clone(),
                    is_const: true,
                }),
            );
            methods.insert(
                "insert",
                Rc::new(Ty::Func {
                    params: vec![
                        key_ty.clone_loc(builtin_loc.clone()),
                        val_ty.clone_loc(builtin_loc.clone()),
                    ],
                    ret: Rc::new(Ty::Void {
                        loc: builtin_loc.clone(),
                    }),
                    loc: builtin_loc.clone(),
                    is_const: false,
                }),
            );
            methods.insert(
                "contains",
                Rc::new(Ty::Func {
                    params: vec![key_ty.clone_loc(builtin_loc.clone())],
                    ret: Rc::new(Ty::Bool {
                        loc: builtin_loc.clone(),
                    }),
                    loc: builtin_loc.clone(),
                    is_const: true,
                }),
            );
            // check if it is one of the methods on the type
            match methods.get(mem) {
                Some(meth_ty) => {
                    if can_access_method(target_is_const, meth_ty) {
                        meth_ty.clone()
                    } else {
                        state.push_err(CheckerError::CannotAccessNonConstFuncOnConstTarget {
                            loc: target_ty.get_loc(),
                        });
                        Rc::new(Ty::ErrorType { loc: loc.clone() })
                    }
                }
                None => {
                    state.push_err(CheckerError::MemberDoesNotExist {
                        given_ty: target_ty.as_str(),
                        mem: mem.to_string(),
                        loc: loc.clone(),
                    });
                    Rc::new(Ty::ErrorType { loc: loc.clone() })
                }
            }
        }
        Ty::Str { loc, .. } => {
            let mut methods: HashMap<&str, Rc<Ty>> = HashMap::from([(
                "len",
                Rc::new(Ty::Func {
                    params: vec![],
                    ret: Ty::get_uint_ty(builtin_loc.clone()),
                    loc: builtin_loc,
                    is_const: true,
                }),
            )]);
            // check if it is one of the methods on the type
            match methods.get(mem) {
                Some(meth_ty) => meth_ty.clone(),
                None => {
                    state.push_err(CheckerError::MemberDoesNotExist {
                        given_ty: "str".into(),
                        mem: mem.to_string(),
                        loc: loc.clone(),
                    });
                    Rc::new(Ty::ErrorType { loc: loc.clone() })
                }
            }
        }
        Ty::StaticArray { sub_ty, loc, .. } | Ty::Slice { sub_ty, loc } => {
            let mut methods: HashMap<&str, Rc<Ty>> = HashMap::from([(
                "len",
                Rc::new(Ty::Func {
                    params: vec![],
                    ret: Ty::get_uint_ty(builtin_loc.clone()),
                    loc: builtin_loc.clone(),
                    is_const: true,
                }),
            )]);
            methods.insert(
                "get",
                Rc::new(Ty::Func {
                    params: vec![Ty::get_uint_ty(builtin_loc.clone())],
                    ret: Rc::new(Ty::Optional {
                        sub_ty: sub_ty.clone_loc(builtin_loc.clone()),
                        loc: builtin_loc.clone(),
                    }),
                    loc: builtin_loc.clone(),
                    is_const: true,
                }),
            );
            methods.insert(
                "make_slice",
                Rc::new(Ty::Func {
                    params: vec![
                        Ty::get_uint_ty(builtin_loc.clone()),
                        Ty::get_uint_ty(builtin_loc.clone()),
                    ],
                    ret: Rc::new(Ty::Slice {
                        sub_ty: sub_ty.clone_loc(builtin_loc.clone()),
                        loc: builtin_loc.clone(),
                    }),
                    loc: builtin_loc.clone(),
                    is_const: true,
                }),
            );
            methods.insert(
                "make_slice_from",
                Rc::new(Ty::Func {
                    params: vec![Ty::get_uint_ty(builtin_loc.clone())],
                    ret: Rc::new(Ty::Slice {
                        sub_ty: sub_ty.clone_loc(builtin_loc.clone()),
                        loc: builtin_loc.clone(),
                    }),
                    loc: builtin_loc.clone(),
                    is_const: true,
                }),
            );

            if matches!(target_ty, Ty::Slice { .. }) {
                methods.insert(
                    "append",
                    Rc::new(Ty::Func {
                        params: vec![sub_ty.clone_loc(builtin_loc.clone())],
                        ret: Rc::new(Ty::Void {
                            loc: builtin_loc.clone(),
                        }),
                        loc: builtin_loc,
                        is_const: false,
                    }),
                );
            }
            // check if it is one of the methods on the type
            match methods.get(mem) {
                Some(meth_ty) => {
                    if can_access_method(target_is_const, meth_ty) {
                        meth_ty.clone()
                    } else {
                        state.push_err(CheckerError::CannotAccessNonConstFuncOnConstTarget {
                            loc: target_ty.get_loc(),
                        });
                        Rc::new(Ty::ErrorType { loc: loc.clone() })
                    }
                }
                None => {
                    state.push_err(CheckerError::MemberDoesNotExist {
                        given_ty: target_ty.as_str(),
                        mem: mem.to_string(),
                        loc: loc.clone(),
                    });
                    Rc::new(Ty::ErrorType { loc: loc.clone() })
                }
            }
        }
        Ty::Optional { sub_ty, loc } => {
            let mut methods = HashMap::from([
                (
                    "is_some",
                    Rc::new(Ty::Func {
                        params: vec![],
                        ret: Rc::new(Ty::Bool {
                            loc: builtin_loc.clone(),
                        }),
                        loc: builtin_loc.clone(),
                        is_const: true,
                    }),
                ),
                (
                    "is_none",
                    Rc::new(Ty::Func {
                        params: vec![],
                        ret: Rc::new(Ty::Bool {
                            loc: builtin_loc.clone(),
                        }),
                        loc: builtin_loc.clone(),
                        is_const: true,
                    }),
                ),
            ]);
            methods.insert(
                "unwrap",
                Rc::new(Ty::Func {
                    params: vec![],
                    ret: sub_ty.clone_loc(builtin_loc.clone()),
                    loc: builtin_loc,
                    is_const: true,
                }),
            );

            // check if it is one of the methods on the type
            match methods.get(mem) {
                Some(meth_ty) => meth_ty.clone(),
                None => {
                    state.push_err(CheckerError::MemberDoesNotExist {
                        given_ty: target_ty.as_str(),
                        mem: mem.to_string(),
                        loc: loc.clone(),
                    });
                    Rc::new(Ty::ErrorType {
                        loc: target_ty.get_loc(),
                    })
                }
            }
        }
        _ => {
            state.push_err(CheckerError::AccessMemberOpCannotBePerformedOnType {
                given_ty: target_ty.as_str(),
                loc: target_ty.get_loc(),
            });
            Rc::new(Ty::ErrorType {
                loc: target_ty.get_loc(),
            })
        }
    }
}

pub fn check_expr(
    e: &Expr,
    context_ty: &Option<Rc<Ty>>,
    state: &mut State,
) -> (Rc<Ty>, Option<TyExpr>) {
    match e {
        Expr::Decimal { val, loc } => match context_ty {
            Some(ty) => {
                if ty.is_float_ty() {
                    let val_is_err = match ty.as_ref() {
                        Ty::Float { size, loc } => {
                            match size {
                                32 => {
                                    val.parse::<f32>().is_err()
                                }
                                64 => {
                                    val.parse::<f64>().is_err()
                                }
                                _ => unreachable!(
                                    "seman::check_expr(): unexpected float size {} in number inference.",
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
                        state.push_err(CheckerError::DecimalTypeInferenceFailed {
                            loc: loc.clone(),
                            number: val.clone(),
                            given_type: ty.as_str(),
                        });
                        (Rc::new(Ty::ErrorType { loc: loc.clone() }), None)
                    } else {
                        (
                            ty.clone_loc(loc.clone()),
                            Some(TyExpr::Decimal { val: val.clone() }),
                        )
                    }
                } else {
                    // make sure val is valid f32 and return the right type
                    // else return error
                    let val_is_err = val.parse::<f32>().is_err();

                    if val_is_err {
                        state.push_err(CheckerError::DecimalTypeDefaultInferenceFailed {
                            loc: loc.clone(),
                            number: val.clone(),
                        });
                        (Rc::new(Ty::ErrorType { loc: loc.clone() }), None)
                    } else {
                        (
                            Rc::new(Ty::Float {
                                size: 32,
                                loc: loc.clone(),
                            }),
                            Some(TyExpr::Decimal { val: val.clone() }),
                        )
                    }
                }
            }
            None => {
                let val_is_err = val.parse::<f32>().is_err();

                if val_is_err {
                    state.push_err(CheckerError::DecimalTypeDefaultInferenceFailed {
                        loc: loc.clone(),
                        number: val.clone(),
                    });
                    (Rc::new(Ty::ErrorType { loc: loc.clone() }), None)
                } else {
                    (
                        Rc::new(Ty::Float {
                            size: 32,
                            loc: loc.clone(),
                        }),
                        Some(TyExpr::Decimal { val: val.clone() }),
                    )
                }
            }
        },
        Expr::Number { val, loc } => match context_ty {
            Some(ty) => {
                if ty.is_num_ty() {
                    let (val_is_err, is_float) = match ty.as_ref() {
                        Ty::Signed { size, is_int, loc } => {
                            match size {
                                8 => {
                                    (val.parse::<i8>().is_err(), false)
                                }
                                16 => {
                                    (val.parse::<i16>().is_err(), false)
                                }
                                32 => {
                                    (val.parse::<i32>().is_err(), false)
                                }
                                64 => {
                                    (val.parse::<i64>().is_err(), false)
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
                                    (val.parse::<u8>().is_err(), false)
                                }
                                16 => {
                                    (val.parse::<u16>().is_err(), false)
                                }
                                32 => {
                                    (val.parse::<u32>().is_err(), false)
                                }
                                64 => {
                                    (val.parse::<u64>().is_err(), false)
                                }
                                _ => unreachable!(
                                    "seman::check_expr(): unexpected unsigned size {} in number inference.",
                                    size
                                )
                            }
                        }
                        Ty::Float { size, loc } => {
                            match size {
                                32 => {
                                    (val.parse::<f32>().is_err(), true)
                                }
                                64 => {
                                    (val.parse::<f64>().is_err(), true)
                                }
                                _ => unreachable!(
                                    "seman::check_expr(): unexpected float size {} in number inference.",
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
                        (Rc::new(Ty::ErrorType { loc: loc.clone() }), None)
                    } else {
                        if is_float {
                            (
                                ty.clone_loc(loc.clone()),
                                Some(TyExpr::Decimal { val: val.clone() }),
                            )
                        } else {
                            (
                                ty.clone_loc(loc.clone()),
                                Some(TyExpr::Integer { val: val.clone() }),
                            )
                        }
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
                        (Rc::new(Ty::ErrorType { loc: loc.clone() }), None)
                    } else {
                        (
                            Rc::new(Ty::Signed {
                                size,
                                is_int: true,
                                loc: loc.clone(),
                            }),
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
                    (Rc::new(Ty::ErrorType { loc: loc.clone() }), None)
                } else {
                    (
                        Rc::new(Ty::Signed {
                            size,
                            is_int: true,
                            loc: loc.clone(),
                        }),
                        Some(TyExpr::Integer { val: val.clone() }),
                    )
                }
            }
        },
        Expr::Str { loc, val } => (
            Rc::new(Ty::Str {
                loc: loc.clone(),
                is_interp: false,
            }),
            Some(TyExpr::Str { val: val.clone() }),
        ),
        Expr::Char { loc, val } => (
            Rc::new(Ty::Char { loc: loc.clone() }),
            Some(TyExpr::Char { val: val.clone() }),
        ),
        Expr::Bool { loc, val } => (
            Rc::new(Ty::Bool { loc: loc.clone() }),
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
                    match ty.as_ref() {
                        Ty::StaticArray { sub_ty, size, loc } => {
                            let arr_size = match size {
                                Some(e) => {
                                    if !matches!(e, Expr::Number { val, .. }) {
                                        state.push_err(
                                            CheckerError::NonConstantNumberSizeForStaticArray {
                                                loc: e.get_source_ref(),
                                            },
                                        );
                                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                                    }

                                    let (e_ty, e_expr) = check_expr(
                                        e,
                                        &Some(Ty::get_uint_ty(e.get_source_ref())),
                                        state,
                                    );

                                    if e_ty.is_error_ty() {
                                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
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
                                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                                    }
                                    e_expr_str
                                }
                                _ => vals.len().to_string(),
                            };

                            // we can now go about checking each item in the static array against
                            // the expected item type.
                            let expected_item_ty = sub_ty.clone();
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
                                }

                                item_ty_exprs.push(item_ty_expr.unwrap());
                            }

                            if had_item_error {
                                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                            }

                            (
                                Rc::new(Ty::StaticArray {
                                    sub_ty: sub_ty.clone_loc(loc.clone()),
                                    size: Some(Expr::Number {
                                        val: arr_size,
                                        loc: loc.clone(),
                                    }),
                                    loc: loc.clone(),
                                }),
                                Some(TyExpr::StaticArray {
                                    vals: item_ty_exprs,
                                }),
                            )
                        }
                        _ => {
                            state.push_err(CheckerError::StaticArrayTypeCheckFailed {
                                given_ty: ty.as_str(),
                                arr_loc: loc.clone(),
                            });
                            (Rc::new(Ty::ErrorType { loc: loc.clone() }), None)
                        }
                    }
                }
                None => {
                    // we can do a standard inference on each item in the array, taking the
                    // type of the first item as the type and making sure all the other
                    // items are of the same type. This means there has to be at least
                    // 1 item in the array
                    if vals.is_empty() {
                        state.push_err(CheckerError::CannotInferTypeOfEmptyArray {
                            loc: loc.clone(),
                        });
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }

                    let (expected_item_ty, first_ty_expr) = check_expr(&vals[0], context_ty, state);
                    if expected_item_ty.is_error_ty() {
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
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
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }

                    (
                        Rc::new(Ty::StaticArray {
                            sub_ty: expected_item_ty.clone_loc(loc.clone()),
                            size: Some(Expr::Number {
                                val: vals.len().to_string(),
                                loc: loc.clone(),
                            }),
                            loc: loc.clone(),
                        }),
                        Some(TyExpr::StaticArray {
                            vals: item_ty_exprs,
                        }),
                    )
                }
            }
        }
        Expr::Ident { name, loc } => {
            // look into builtins first
            let builtins = get_builtin_methods();
            if builtins.get(&name.as_ref()).is_some() {
                let ty = builtins[&name.as_ref()].clone();
                return (ty, Some(TyExpr::BuiltinIdent { name: name.clone() }));
            }

            let i_ty = state.env.lookup(name);
            match i_ty {
                Some(info) => {
                    info.refs.push(loc.clone());
                    (
                        Rc::new(info.ty.borrow().clone()),
                        Some(TyExpr::Ident { name: name.clone() }),
                    )
                }
                None => {
                    state.push_err(CheckerError::ReferenceToUndefinedName {
                        loc: loc.clone(),
                        var_name: name.clone(),
                    });
                    (Rc::new(Ty::ErrorType { loc: loc.clone() }), None)
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
                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
            }
            let temp_l_ty = &Some(l_ty.clone());
            let (mut r_ty, mut r_ty_expr) =
                check_expr(if lhs_is_first { right } else { left }, temp_l_ty, state);
            if r_ty.is_error_ty() {
                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
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
                | BinOpType::Mod => match (l_ty.as_ref(), r_ty.as_ref()) {
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
                            Rc::new(Ty::Str {
                                loc: loc.clone(),
                                is_interp: false,
                            }),
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
                        (Rc::new(Ty::ErrorType { loc: loc.clone() }), None)
                    }
                },
                BinOpType::And | BinOpType::Or => match (l_ty.as_ref(), r_ty.as_ref()) {
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
                        (Rc::new(Ty::ErrorType { loc: loc.clone() }), None)
                    }
                },
                BinOpType::Eq
                | BinOpType::Neq
                | BinOpType::Gt
                | BinOpType::Lt
                | BinOpType::GtEq
                | BinOpType::LtEq => match (l_ty.as_ref(), r_ty.as_ref()) {
                    (Ty::Signed { .. }, Ty::Signed { .. })
                    | (Ty::Unsigned { .. }, Ty::Unsigned { .. })
                        if types_are_eq(&l_ty, &r_ty) && types_are_eq(&l_ty, &r_ty) =>
                    {
                        (
                            Rc::new(Ty::Bool { loc: loc.clone() }),
                            Some(TyExpr::BinOp {
                                op: *op,
                                lhs: Box::new(l_ty_expr.unwrap()),
                                rhs: Box::new(r_ty_expr.unwrap()),
                            }),
                        )
                    }
                    (Ty::Char { .. }, Ty::Char { .. }) => (
                        Rc::new(Ty::Bool { loc: loc.clone() }),
                        Some(TyExpr::BinOp {
                            op: *op,
                            lhs: Box::new(l_ty_expr.unwrap()),
                            rhs: Box::new(r_ty_expr.unwrap()),
                        }),
                    ),
                    (Ty::Str { .. }, Ty::Str { .. }) => (
                        Rc::new(Ty::Bool { loc: loc.clone() }),
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
                        (Rc::new(Ty::ErrorType { loc: loc.clone() }), None)
                    }
                },
            }
        }
        Expr::CallFn { func, args, loc } => {
            let (func_ty, func_ty_expr) = check_expr(func, &None, state);

            if func_ty.is_error_ty() {
                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
            }

            match func_ty.as_ref() {
                Ty::Struct { name, funcs, .. } => {
                    // look for init() func
                    if !funcs.contains_key(&"init".to_string()) {
                        state.push_err(CheckerError::StructHasNoInitFunction {
                            given_ty: name.clone(),
                            loc: func.get_source_ref(),
                        });
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }

                    let init_fn = &funcs[&"init".to_string()];

                    if let Ty::Func { params, ret, .. } = init_fn.as_ref() {
                        // make sure ret equals the struct ty
                        if !ret.is_void() {
                            state.push_err(CheckerError::TypeMismatch {
                                loc: func.get_source_ref(),
                                expected: "void".into(),
                                found: ret.as_str(),
                            });
                            return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                        }

                        // check parameters
                        let fn_arity = params.len();
                        if fn_arity != args.len() {
                            state.push_err(CheckerError::IncorrectFunctionArity {
                                func: func.as_str(),
                                exp: fn_arity,
                                given: args.len(),
                                loc_given: loc.clone(),
                            });
                            return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                        }

                        let mut fn_ty_args = vec![];
                        for (param_ty, arg) in params.iter().zip(args.iter()) {
                            let (arg_ty, arg_ty_expr) =
                                check_expr(arg, &Some(param_ty.clone()), state);

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
                            func_ty.clone_loc(loc.clone()),
                            Some(TyExpr::CallFn {
                                func: Box::new(func_ty_expr.unwrap()),
                                args: fn_ty_args,
                            }),
                        )
                    } else {
                        unreachable!("seman::check_expr(): found init with a non-function type.");
                    }
                }
                Ty::Func {
                    params, ret, loc, ..
                } => {
                    let fn_arity = params.len();
                    if fn_arity != args.len() {
                        state.push_err(CheckerError::IncorrectFunctionArity {
                            func: func.as_str(),
                            exp: fn_arity,
                            given: args.len(),
                            loc_given: loc.clone(),
                        });
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
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

                    // determine if the func_ty_expr is an TyExpr::BuiltinIdent
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
                    return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                }
            }
        }
        Expr::UnaryOp { op, expr, loc } => {
            let (expr_ty, expr_ty_expr) = check_expr(expr, context_ty, state);

            match op {
                UnaryOpType::Not => {
                    if expr_ty.is_error_ty() {
                        return (Rc::new(Ty::Bool { loc: loc.clone() }), None);
                    }

                    if matches!(expr_ty.as_ref(), Ty::Bool { .. }) {
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
                    (Rc::new(Ty::ErrorType { loc: loc.clone() }), None)
                }
                UnaryOpType::Negate => {
                    if expr_ty.is_error_ty() {
                        // we can just return an error type
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }

                    if !expr_ty.is_signed_ty() {
                        state.push_err(CheckerError::InvalidUseOfUnaryOperator {
                            loc: loc.clone(),
                            op: op.as_str(),
                            operand: expr.as_str(),
                            tip: None,
                        });
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
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
        Expr::ErrorExpr { loc } => (Rc::new(Ty::ErrorType { loc: loc.clone() }), None),
        Expr::GroupedExpr { inner, loc } => {
            let (inner_ty, inner_ty_expr) = check_expr(inner, context_ty, state);
            if inner_ty.is_error_ty() {
                (Rc::new(Ty::ErrorType { loc: loc.clone() }), None)
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
            if !cond_ty.is_error_ty() && !matches!(cond_ty.as_ref(), Ty::Bool { .. }) {
                state.push_err(CheckerError::ConditionShouldBeTypedBool {
                    given_ty: cond_ty.as_str(),
                    loc: cond.get_source_ref(),
                });
                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
            }

            // typecheck then and otherwise expressions and make sure they have
            // the same type. remember to prioritize then or otherwise if we need
            // to extra context to typecheck the other expression, similar to binary
            // expressions.
            let (then_ty, then_ty_expr) = check_expr(then, context_ty, state);
            let (other_ty, other_ty_expr) = check_expr(otherwise, context_ty, state);

            if then_ty.is_error_ty() || other_ty.is_error_ty() {
                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
            }

            if !types_are_eq(&then_ty, &other_ty) {
                state.push_err(CheckerError::TypeMismatch {
                    loc: then.get_source_ref().combine(otherwise.get_source_ref()),
                    expected: then_ty.as_str(),
                    found: other_ty.as_str(),
                });
                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
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
            Rc::new(Ty::Str {
                loc: loc.clone(),
                is_interp: true,
            }),
            Some(TyExpr::Str { val: val.clone() }),
        ),
        Expr::InterpolatedString { parts, loc } => {
            // check each expression in parts and create a TyExpr from it with a
            // string type
            let mut new_ty_exprs = vec![];
            for part in parts.iter() {
                let (new_ty, mut new_ty_expr) = check_expr(part, &None, state);
                if !new_ty.is_error_ty() {
                    new_ty_expr = match new_ty.as_ref() {
                        Ty::Void { .. } => Some(TyExpr::MultiExpr {
                            exprs: vec![
                                new_ty_expr.unwrap(),
                                TyExpr::Str {
                                    val: "void".to_string(),
                                },
                            ],
                        }),
                        Ty::Str { is_interp, .. } => {
                            if *is_interp {
                                new_ty_expr
                            } else {
                                Some(TyExpr::CallFn {
                                    func: Box::new(TyExpr::Ident {
                                        name: "proto_str".to_string(),
                                    }),
                                    args: vec![new_ty_expr.unwrap()],
                                })
                            }
                        }
                        Ty::Bool { .. }
                        | Ty::Char { .. }
                        | Ty::Signed { .. }
                        | Ty::Unsigned { .. }
                        | Ty::StaticArray { .. }
                        | Ty::Slice { .. }
                        | Ty::Optional { .. }
                        | Ty::Pointer { .. }
                        | Ty::Float { .. }
                        | Ty::HashMap { .. } => Some(TyExpr::CallFn {
                            func: Box::new(TyExpr::Ident {
                                name: "proto_str".to_string(),
                            }),
                            args: vec![new_ty_expr.unwrap()],
                        }),
                        Ty::NamedType { name, .. } => todo!(),
                        Ty::Struct { funcs, .. } => {
                            // make sure there is an as_str() function on the struct
                            todo!()
                        }
                        _ => {
                            unreachable!(
                                "seman::check_expr(): {:#?} in string interpolation.",
                                new_ty
                            )
                        }
                    };
                    new_ty_exprs.push(new_ty_expr.unwrap());
                } else {
                    return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                }
            }

            (
                Rc::new(Ty::Str {
                    loc: loc.clone(),
                    is_interp: false,
                }),
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
                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
            }

            if !target_ty.is_sliceable() {
                state.push_err(CheckerError::ExpectedArrayOrSlice {
                    given_ty: target_ty.as_str(),
                    loc: target_ty.get_loc(),
                });
                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
            }

            // check both start and end_excl expressions, if they are provided
            let (start_ty, start_ty_expr) = match start {
                Some(start) => {
                    let (a, b) =
                        check_expr(start, &Some(Ty::get_uint_ty(start.get_source_ref())), state);
                    if a.is_error_ty() {
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }

                    if !a.is_unsigned_ty() {
                        state.push_err(CheckerError::TypeMismatch {
                            loc: start.get_source_ref(),
                            expected: "uint".into(),
                            found: a.as_str(),
                        });
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
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
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }

                    if !end_ty.is_unsigned_ty() {
                        state.push_err(CheckerError::TypeMismatch {
                            loc: end_excl.get_source_ref(),
                            expected: "uint".into(),
                            found: end_ty.as_str(),
                        });
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }

                    match target_ty.as_ref() {
                        Ty::StaticArray { sub_ty, .. } => {
                            let slice_ty = Ty::Slice { sub_ty: sub_ty.clone_loc(loc.clone()), loc: loc.clone() };
                            if let Some(context_ty) = context_ty {
                                if !types_are_eq(context_ty, &slice_ty) {
                                    state.push_err(CheckerError::TypeMismatch {
                                        loc: loc.clone(),
                                        expected: context_ty.as_str(),
                                        found: slice_ty.as_str()
                                    });
                                    return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                                }
                            }
                            (Rc::new(slice_ty), Some(TyExpr::MakeSliceWithEnd {
                                target: Box::new(target_ty_expr.unwrap()),
                                start: Box::new(start_ty_expr),
                                end_excl: Box::new(end_ty_expr.unwrap())
                            }))
                        }
                        Ty::Slice { sub_ty, .. } => {
                            let slice_ty = Ty::Slice { sub_ty: sub_ty.clone_loc(loc.clone()), loc: loc.clone() };
                            if let Some(context_ty) = context_ty {
                                if !types_are_eq(context_ty, &slice_ty) {
                                    state.push_err(CheckerError::TypeMismatch {
                                        loc: loc.clone(),
                                        expected: context_ty.as_str(),
                                        found: slice_ty.as_str()
                                    });
                                    return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                                }
                            }

                            (Rc::new(slice_ty), Some(TyExpr::MakeSliceWithEnd {
                                target: Box::new(target_ty_expr.unwrap()),
                                start: Box::new(start_ty_expr),
                                end_excl: Box::new(end_ty_expr.unwrap())
                            }))
                        }
                        _ => unreachable!("seman::check_expr(): while checking exclusive end of MakeSlice expr, {:#?} made it through", target_ty)
                    }
                }
                None =>
                    match target_ty.as_ref() {
                        Ty::StaticArray { sub_ty, size, .. } => {
                            let end_ty_expr = TyExpr::Integer {
                                val: size.as_ref().unwrap().as_str(),
                            };
                            let slice_ty = Ty::Slice { sub_ty: sub_ty.clone_loc(loc.clone()), loc: loc.clone() };
                            if let Some(context_ty) = context_ty {
                                if !types_are_eq(context_ty, &slice_ty) {
                                    state.push_err(CheckerError::TypeMismatch {
                                        loc: loc.clone(),
                                        expected: context_ty.as_str(),
                                        found: slice_ty.as_str()
                                    });
                                    return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                                }
                            }
                            (Rc::new(slice_ty), Some(TyExpr::MakeSliceWithEnd {
                                target: Box::new(target_ty_expr.unwrap()),
                                start: Box::new(start_ty_expr),
                                end_excl: Box::new(end_ty_expr)
                            }))
                        }
                        Ty::Slice { sub_ty, .. } => {
                            let slice_ty = Ty::Slice { sub_ty: sub_ty.clone_loc(loc.clone()), loc: loc.clone() };
                            if let Some(context_ty) = context_ty {
                                if !types_are_eq(context_ty, &slice_ty) {
                                    state.push_err(CheckerError::TypeMismatch {
                                        loc: loc.clone(),
                                        expected: context_ty.as_str(),
                                        found: slice_ty.as_str()
                                    });
                                    return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                                }
                            }
                            (Rc::new(slice_ty), Some(TyExpr::MakeSliceFrom {
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
                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
            }

            if !target_ty.is_indexable() {
                state.push_err(CheckerError::IndexIntoOpRequiresArraySliceOrString {
                    given_ty: target_ty.as_str(),
                    loc: target.get_source_ref(),
                });
                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
            }

            // handle the case where the target's type is a hashmap
            if let Ty::HashMap { key_ty, val_ty, .. } = target_ty.as_ref() {
                // we will use the key type to check the index expression
                let key_ty = (*key_ty).clone();
                let (index_ty, index_ty_expr) = check_expr(index, &Some(key_ty.clone()), state);
                if index_ty.is_error_ty() {
                    return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                }
                if !types_are_eq(&key_ty, &index_ty) {
                    state.push_err(CheckerError::TypeMismatch {
                        loc: index.get_source_ref(),
                        expected: key_ty.as_str(),
                        found: index_ty.as_str(),
                    });
                    return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                }

                return (
                    val_ty.clone_loc(loc.clone()),
                    Some(TyExpr::IndexInto {
                        target: Box::new(target_ty_expr.unwrap()),
                        index: Box::new(index_ty_expr.unwrap()),
                    }),
                );
            }

            let (index_ty, index_ty_expr) =
                check_expr(index, &Some(Ty::get_uint_ty(index.get_source_ref())), state);
            if index_ty.is_error_ty() {
                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
            }
            if !index_ty.is_unsigned_ty() {
                state.push_err(CheckerError::TypeMismatch {
                    loc: index.get_source_ref(),
                    expected: "uint".into(),
                    found: index_ty.as_str(),
                });
                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
            };

            let new_ty = match target_ty.as_ref() {
                Ty::Str { .. } => Rc::new(Ty::Char { loc: loc.clone() }),
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
                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
            }

            match type_is_valid(&target_ty) {
                Ok(_) => {}
                Err((e, e_loc)) => match e {
                    TypeValidErr::Invalid => {
                        state.push_err(CheckerError::InvalidType {
                            loc: e_loc,
                            type_name: target_ty.as_str(),
                        });
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }
                    TypeValidErr::Incomplete => {
                        state.push_err(CheckerError::IncompleteType {
                            loc: e_loc,
                            type_name: target_ty.as_str(),
                        });
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }
                },
            }

            let builtin_loc = Rc::new(SourceRef {
                file: Rc::new("__proto_gen_builtins__".to_string()),
                start_line: 0,
                start_col: 0,
                end_line: 0,
                end_col: 0,
                flat_start: 0,
                flat_end: 0,
            });

            // determine if we are accessing a non-const function on a const type
            let mut target_is_const = false;
            let mut cur = target;
            loop {
                match cur.as_ref() {
                    Expr::Ident { name, .. } => {
                        // check the environment to see if it is modifiable
                        // we will need to check the body and update the struct type under the name
                        // as we get new information (fields, associated functions and methods)
                        let name_info = state.env.lookup(&name).unwrap();
                        target_is_const = name_info.is_const;
                        break;
                    }
                    _ => {
                        break;
                    }
                }
            }

            let mem_ty = check_for_member(
                target_is_const,
                &target_ty.clone_loc(target.get_source_ref()),
                &mem.as_str(),
                state,
            );
            if mem_ty.is_error_ty() {
                return (mem_ty, None);
            }

            let is_self = if let Expr::Ident { name, loc } = target.as_ref() {
                name == "self"
            } else {
                false
            };

            (
                mem_ty,
                Some(TyExpr::AccessMember {
                    target: Box::new(target_ty_expr.unwrap()),
                    mem: Box::new(TyExpr::Ident { name: mem.as_str() }),
                    is_self_access: is_self,
                    is_ptr: target_ty.is_pointer(),
                }),
            )
        }
        Expr::OptionalExpr { val, loc } => {
            let (v_ty, v_ty_expr) = match (context_ty, val) {
                (Some(ty), Some(val)) => match ty.as_ref() {
                    Ty::Optional { sub_ty, loc } => {
                        let expected_item_ty = sub_ty.clone();
                        check_expr(val, &Some(expected_item_ty), state)
                    }
                    _ => {
                        state.push_err(CheckerError::OptionalTypeInferenceFailed {
                            given_ty: ty.as_str(),
                            opt_loc: loc.clone(),
                        });
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }
                },
                (None, Some(val)) => check_expr(val, context_ty, state),
                (Some(ty), None) => match ty.as_ref() {
                    Ty::Optional { sub_ty, loc } => {
                        let opt_ty = Rc::new(Ty::Optional {
                            sub_ty: sub_ty.clone(),
                            loc: loc.clone(),
                        });
                        return (opt_ty, Some(TyExpr::OptionalExpr { val: None }));
                    }
                    _ => {
                        state.push_err(CheckerError::OptionalTypeInferenceFailed {
                            given_ty: ty.as_str(),
                            opt_loc: loc.clone(),
                        });
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }
                },
                (None, None) => {
                    state.push_err(
                        CheckerError::OptionalTypeInferenceFailedWithoutContextualTy {
                            opt_loc: loc.clone(),
                        },
                    );
                    return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                }
            };

            if v_ty.is_error_ty() {
                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
            }

            let opt_ty = Ty::Optional {
                sub_ty: v_ty,
                loc: loc.clone(),
            };
            (
                Rc::new(opt_ty),
                Some(TyExpr::OptionalExpr {
                    val: Some(Box::new(v_ty_expr.unwrap())),
                }),
            )
        }
        Expr::Lambda {
            params,
            ret_type,
            body,
            loc,
        } => {
            // construct the function type and add it to the environment
            let ret_ty = match type_is_valid(ret_type) {
                Ok(_) => ret_type.clone(),
                Err((e, e_loc)) => match e {
                    TypeValidErr::Invalid => {
                        state.push_err(CheckerError::InvalidType {
                            loc: e_loc,
                            type_name: ret_type.as_str(),
                        });
                        Rc::new(Ty::ErrorType {
                            loc: ret_type.get_loc(),
                        })
                    }
                    TypeValidErr::Incomplete => {
                        state.push_err(CheckerError::IncompleteType {
                            loc: e_loc,
                            type_name: ret_type.as_str(),
                        });
                        Rc::new(Ty::ErrorType {
                            loc: ret_type.get_loc(),
                        })
                    }
                },
            };

            if ret_ty.is_error_ty() {
                return (
                    Rc::new(Ty::ErrorType {
                        loc: ret_ty.get_loc(),
                    }),
                    None,
                );
            }

            let mut fn_type_params = vec![];
            let mut pairs_to_register = vec![];
            let mut has_error = false;
            for param in params.into_iter() {
                let p_ty = match type_is_valid(&param.given_ty) {
                    Ok(_) => param.given_ty.clone(),
                    Err((e, e_loc)) => match e {
                        TypeValidErr::Invalid => {
                            state.push_err(CheckerError::InvalidType {
                                loc: e_loc,
                                type_name: param.given_ty.as_str(),
                            });
                            Rc::new(Ty::ErrorType {
                                loc: param.given_ty.get_loc(),
                            })
                        }
                        TypeValidErr::Incomplete => {
                            state.push_err(CheckerError::IncompleteType {
                                loc: e_loc,
                                type_name: param.given_ty.as_str(),
                            });
                            Rc::new(Ty::ErrorType {
                                loc: param.given_ty.get_loc(),
                            })
                        }
                    },
                };
                if p_ty.is_error_ty() {
                    has_error = true;
                }
                fn_type_params.push(p_ty.clone());
                pairs_to_register.push((
                    param.name.as_str(),
                    NameInfo {
                        ty: Rc::new(RefCell::new((*p_ty).clone())),
                        refs: vec![param.loc.clone()],
                        is_const: true,
                        is_initialized: true,
                    },
                ));
            }

            if has_error {
                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
            }

            let fn_type = Rc::new(Ty::Func {
                params: fn_type_params,
                ret: ret_ty.clone(),
                loc: loc.clone(),
                is_const: false,
            });

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
                    given_ty: Rc::new(param_info.ty.borrow().clone()),
                });
            }

            // check the body
            let old_enter_new_scope = state.enter_new_scope;
            state.enter_new_scope = false;
            let old_check_for_return = state.check_for_return;
            state.check_for_return = true;
            let new_body = if body.contains_sub_instructions() {
                check_ins(body, &Some(ret_ty.clone()), state)
            } else {
                let ty_i = check_ins(body, &Some(ret_ty.clone()), state);
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
                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
            }

            let new_body = new_body.unwrap();
            // reset the scope info
            state.enter_new_scope = old_enter_new_scope;
            state.check_for_return = old_check_for_return;
            state.scope_stack.pop();
            state.env.pop();
            (
                fn_type,
                Some(TyExpr::Lambda {
                    params: ty_params,
                    ret_ty: ret_type.clone(),
                    body: Box::new(new_body),
                }),
            )
        }
        Expr::DerefPtr { target, loc } => {
            // we need to make sure target has a type of pointer. Then we can return
            // its subtype
            let (ty_targ, ty_targ_expr) = check_expr(target, &None, state);
            if ty_targ.is_error_ty() {
                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
            }

            if !ty_targ.is_pointer() {
                state.push_err(CheckerError::CannotDerefNonPtrType {
                    given_ty: ty_targ.as_str(),
                    loc: target.get_source_ref(),
                });
                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
            }

            // get the subtype
            if let Ty::Pointer { sub_ty, .. } = ty_targ.as_ref() {
                let c_sub_ty = sub_ty.clone_loc(target.get_source_ref());
                return (
                    c_sub_ty,
                    Some(TyExpr::DerefPtr {
                        target: Box::new(ty_targ_expr.unwrap()),
                    }),
                );
            } else {
                unreachable!(
                    "seman::check_expr(): Non-pointer type passed pointer check: {}({}).",
                    ty_targ.as_str(),
                    ty_targ.get_loc().as_str()
                );
            }
        }
        Expr::MakePtrFromAddrOf { target, loc } => {
            // we need to make sure target is a non-ephemeral expression i.e. it has an address
            // that can be taken and referred to.
            if !target.has_address() {
                state.push_err(CheckerError::CannotTakeAddressOfExpr {
                    loc: target.get_source_ref(),
                });
                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
            }

            // check the target so we can determine the type of what we are taking an address of.
            // we can then construct the type
            let (ty_targ, ty_targ_expr) = check_expr(target, &None, state);
            if ty_targ.is_error_ty() {
                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
            }

            let n_ty = Rc::new(Ty::Pointer {
                sub_ty: ty_targ.clone_loc(loc.clone()),
                loc: loc.clone(),
            });

            (
                n_ty,
                Some(TyExpr::MakePtrFromAddrOf {
                    target: Box::new(ty_targ_expr.unwrap()),
                }),
            )
        }
        Expr::NewAlloc { ty, args, loc } => {
            // we need to make sure type is valid, and if it is:
            // - a struct: we type check arguments and look for an init() on it then
            // return a ptr to the struct type
            // - an array: it can have the items to be inserted into the array after
            // creation.
            // - a scalar type: type check any init expr (has to be a single one)
            let ty = match type_is_valid(ty) {
                Ok(_) => ty.clone(),
                Err((e, e_loc)) => match e {
                    TypeValidErr::Invalid => {
                        state.push_err(CheckerError::InvalidType {
                            loc: e_loc,
                            type_name: ty.as_str(),
                        });
                        Rc::new(Ty::ErrorType { loc: ty.get_loc() })
                    }
                    TypeValidErr::Incomplete => {
                        state.push_err(CheckerError::IncompleteType {
                            loc: e_loc,
                            type_name: ty.as_str(),
                        });
                        Rc::new(Ty::ErrorType { loc: ty.get_loc() })
                    }
                },
            };

            if ty.is_error_ty() {
                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
            }

            match ty.as_ref() {
                Ty::Signed { .. }
                | Ty::Unsigned { .. }
                | Ty::Float { .. }
                | Ty::Str { .. }
                | Ty::Char { .. }
                | Ty::Void { .. }
                | Ty::Bool { .. }
                | Ty::Func { .. }
                | Ty::Optional { .. }
                | Ty::Pointer { .. } => {
                    // we will check to make sure there is 1 argument and then we will check
                    // that expression with ty.
                    if args.is_empty() {
                        let n_ty = Rc::new(Ty::Pointer {
                            sub_ty: ty.clone_loc(loc.clone()),
                            loc: loc.clone(),
                        });
                        return (
                            n_ty.clone(),
                            Some(TyExpr::NewAlloc {
                                ty: ty.clone(),
                                args: vec![],
                            }),
                        );
                    }
                    if args.len() > 1 {
                        // we cannot have more than 1 argument
                        state.push_err(CheckerError::Expected(
                            format!(
                                "1 expression to initialize newly allocated '{}' but found {} expressions.",
                                ty.as_str(),
                                args.len()
                            ),
                            loc.clone(),
                            None,
                        ));
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }

                    // now we can check the expression
                    let (ty_init, ty_init_expr) = check_expr(&args[0], &Some(ty.clone()), state);

                    if ty_init.is_error_ty() {
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }

                    if !types_are_eq(&ty, &ty_init) {
                        state.push_err(CheckerError::TypeMismatch {
                            loc: args[0].get_source_ref(),
                            expected: ty.as_str(),
                            found: ty_init.as_str(),
                        });
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }

                    let n_ty = Rc::new(Ty::Pointer {
                        sub_ty: ty.clone_loc(loc.clone()),
                        loc: loc.clone(),
                    });
                    (
                        n_ty.clone(),
                        Some(TyExpr::NewAlloc {
                            ty: ty.clone(),
                            args: vec![ty_init_expr.unwrap()],
                        }),
                    )
                }
                Ty::Slice { sub_ty, .. } => {
                    // this can take the length and capacity of the backing array
                    // since slices require a backing array. This allows us overcome the
                    // explicit need for a growable vector type
                    if args.is_empty() {
                        // we will use the default allocating slice
                        let n_ty = ty.clone_loc(loc.clone());
                        return (
                            n_ty.clone(),
                            Some(TyExpr::SliceDefaultAlloc { ty: ty.clone() }),
                        );
                    }

                    // we expect 1 argument which is the capacity of the the new slice
                    // typed usize.
                    if args.len() > 1 {
                        // we cannot have more than 1 argument
                        state.push_err(CheckerError::Expected(
                            format!(
                                "at most 1 expression, which is the capacity of the newly allocated '{}' but found {} expressions.",
                                ty.as_str(),
                                args.len()
                            ),
                            loc.clone(),
                            None,
                        ));
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }

                    let uint_ty = Ty::get_uint_ty(loc.clone());

                    let (ty_cap, ty_cap_expr) = check_expr(&args[0], &Some(uint_ty.clone()), state);

                    if ty_cap.is_error_ty() {
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }

                    if !types_are_eq(&ty, &uint_ty) {
                        state.push_err(CheckerError::TypeMismatch {
                            loc: args[0].get_source_ref(),
                            expected: uint_ty.as_str(),
                            found: ty_cap.as_str(),
                        });
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }

                    let n_ty = ty.clone_loc(loc.clone());
                    (
                        n_ty.clone(),
                        Some(TyExpr::SliceSizedAlloc {
                            ty: ty.clone(),
                            cap: Rc::new(ty_cap_expr.unwrap()),
                        }),
                    )
                }
                Ty::StaticArray { sub_ty, size, .. } => {
                    // arrays unlike slices have to be initialized completely, since they are static
                    // (since we will explicitly need to pass an initializer list that is used to
                    // init the underlying array, unlike Slices which will use an existing, initialized
                    // buffer or malloc and memsets its own buffer, bypassing the need to initialize).

                    // the array type has a size included with it, which we will use to verify
                    // that the number of initialization items is correct
                    let size_n = size.as_ref().unwrap().as_str().parse::<usize>();
                    if size_n.is_err() {
                        state.push_err(CheckerError::Expected(
                            format!(""),
                            size.as_ref().unwrap().get_source_ref(),
                            None,
                        ));
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }

                    // the number of items we are expecting in args
                    let size_n = size_n.unwrap();

                    if args.len() != size_n {
                        state.push_err(CheckerError::MismismatchStaticArrayLength {
                            exp: size_n.to_string(),
                            given: args.len().to_string(),
                            arr_loc: loc.clone(),
                        });
                    }

                    // now we can use the sub_ty of the array type to check each element in the
                    // args list
                    let expected_ty = sub_ty.clone();
                    let mut has_error = false;
                    let mut ty_args = vec![];
                    for arg in args {
                        let (ty_arg, ty_arg_expr) =
                            check_expr(arg, &Some(expected_ty.clone()), state);

                        if ty_arg.is_error_ty() {
                            has_error = true;
                            continue;
                        }

                        if !types_are_eq(&ty_arg, &expected_ty) {
                            state.push_err(CheckerError::TypeMismatch {
                                loc: arg.get_source_ref(),
                                expected: expected_ty.as_str(),
                                found: ty_arg.as_str(),
                            });
                            has_error = true;
                            continue;
                        }

                        ty_args.push(ty_arg_expr.unwrap());
                    }

                    if has_error {
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }

                    let n_ty = Rc::new(Ty::Pointer {
                        sub_ty: ty.clone_loc(loc.clone()),
                        loc: loc.clone(),
                    });
                    (
                        n_ty.clone(),
                        Some(TyExpr::NewArrayAlloc {
                            ty: ty.clone(),
                            init: ty_args,
                        }),
                    )
                }
                Ty::HashMap { key_ty, val_ty, .. } => {
                    todo!()
                }
                Ty::Struct { .. } => {
                    unreachable!(
                        "seman::check_expr(): a concrete struct type was somehow passed into new(): {}.",
                        ty.get_loc().as_str()
                    )
                }
                Ty::Trait { .. } => {
                    unreachable!(
                        "seman::check_expr(): a concrete trait type was somehow passed into new(): {}.",
                        ty.get_loc().as_str()
                    )
                }
                Ty::TraitImpl { .. } => todo!(),
                Ty::NamedType { name, .. } => {
                    // we will need to get the actual type that backs the named type
                    let actual_ty_info = state.env.lookup(name);
                    if actual_ty_info.is_none() {
                        state.push_err(CheckerError::InvalidType {
                            loc: ty.get_loc(),
                            type_name: name.clone(),
                        });
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }

                    // we want to find the init() void function in the Struct
                    let actual_ty_info = actual_ty_info.unwrap().clone();
                    let actual_ty = &*actual_ty_info.ty.borrow();

                    match actual_ty {
                        Ty::Struct { funcs, .. } => {
                            // look for init() func
                            if !funcs.contains_key(&"init".to_string()) {
                                state.push_err(CheckerError::StructHasNoInitFunction {
                                    given_ty: name.clone(),
                                    loc: ty.get_loc(),
                                });
                                return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                            }

                            let init_fn = &funcs[&"init".to_string()];
                            if let Ty::Func { params, ret, .. } = init_fn.as_ref() {
                                // make sure ret equals the struct ty
                                if !ret.is_void() {
                                    state.push_err(CheckerError::TypeMismatch {
                                        loc: ty.get_loc(),
                                        expected: "void".into(),
                                        found: ret.as_str(),
                                    });
                                    return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                                }

                                // check parameters
                                let fn_arity = params.len();
                                if fn_arity != args.len() {
                                    state.push_err(CheckerError::IncorrectFunctionArity {
                                        func: ty.as_str(),
                                        exp: fn_arity,
                                        given: args.len(),
                                        loc_given: loc.clone(),
                                    });
                                    return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                                }

                                let mut fn_ty_args = vec![];
                                for (param_ty, arg) in params.iter().zip(args.iter()) {
                                    let (arg_ty, arg_ty_expr) =
                                        check_expr(arg, &Some(param_ty.clone()), state);

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
                                let n_ty = Rc::new(Ty::Pointer {
                                    sub_ty: ty.clone_loc(loc.clone()),
                                    loc: loc.clone(),
                                });
                                (
                                    n_ty,
                                    Some(TyExpr::NewAlloc {
                                        ty: ty.clone(),
                                        args: fn_ty_args,
                                    }),
                                )
                            } else {
                                unreachable!(
                                    "seman::check_expr(): found init with a non-function type."
                                );
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                Ty::ErrorType { loc } => unreachable!(),
            }
        }
        Expr::HashMap { pairs, loc } => {
            match context_ty {
                Some(context_ty) => match context_ty.as_ref() {
                    Ty::HashMap { key_ty, val_ty, .. } => {
                        let expected_key_ty = key_ty.clone();
                        let expected_val_ty = val_ty.clone();
                        let mut pair_ty_exprs = vec![];
                        let mut had_error = false;
                        for pair in pairs {
                            let (ty_key, ty_key_expr) =
                                check_expr(&pair.key, &Some(expected_key_ty.clone()), state);
                            let (ty_val, ty_val_expr) =
                                check_expr(&pair.val, &Some(expected_val_ty.clone()), state);

                            if ty_key.is_error_ty() || ty_val.is_error_ty() {
                                had_error = true;
                                continue;
                            }

                            if !types_are_eq(&expected_key_ty, &ty_key) {
                                state.push_err(CheckerError::TypeMismatch {
                                    loc: pair.key.get_source_ref(),
                                    expected: expected_key_ty.as_str(),
                                    found: ty_key.as_str(),
                                });
                                had_error = true;
                                continue;
                            }

                            if !types_are_eq(&expected_val_ty, &ty_val) {
                                state.push_err(CheckerError::TypeMismatch {
                                    loc: pair.val.get_source_ref(),
                                    expected: expected_val_ty.as_str(),
                                    found: ty_val.as_str(),
                                });
                                had_error = true;
                                continue;
                            }

                            pair_ty_exprs.push(TyHashMapPair {
                                key: ty_key_expr.unwrap(),
                                val: ty_val_expr.unwrap(),
                            });
                        }

                        if had_error {
                            return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                        }

                        (
                            Rc::new(Ty::HashMap {
                                key_ty: expected_key_ty,
                                val_ty: expected_val_ty,
                                loc: loc.clone(),
                            }),
                            Some(TyExpr::HashMap {
                                pairs: pair_ty_exprs,
                            }),
                        )
                    }
                    _ => {
                        state.push_err(CheckerError::StaticArrayTypeCheckFailed {
                            given_ty: context_ty.as_str(),
                            arr_loc: loc.clone(),
                        });
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }
                },
                None => {
                    // we will use the type of the first pair to check all the pairs
                    // and type the hashmap
                    if pairs.is_empty() {
                        state.push_err(CheckerError::CannotInferTypeOfEmptyHashMap {
                            loc: loc.clone(),
                        });
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }

                    let (expected_key_ty, first_ty_key_expr) =
                        check_expr(&pairs[0].key, context_ty, state);
                    let (expected_val_ty, first_ty_val_expr) =
                        check_expr(&pairs[0].val, context_ty, state);
                    if expected_key_ty.is_error_ty() || expected_val_ty.is_error_ty() {
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }

                    let mut had_error = false;
                    let mut pair_ty_exprs = vec![TyHashMapPair {
                        key: first_ty_key_expr.unwrap(),
                        val: first_ty_val_expr.unwrap(),
                    }];
                    for index in 1..pairs.len() {
                        let cur_pair = &pairs[index];
                        let (cur_key_ty, cur_key_ty_expr) =
                            check_expr(&cur_pair.key, context_ty, state);
                        let (cur_val_ty, cur_val_ty_expr) =
                            check_expr(&cur_pair.val, context_ty, state);

                        if cur_key_ty.is_error_ty() || cur_val_ty.is_error_ty() {
                            had_error = true;
                            continue;
                        }

                        if !types_are_eq(&expected_key_ty, &cur_key_ty) {
                            state.push_err(CheckerError::TypeMismatch {
                                loc: cur_pair.key.get_source_ref(),
                                expected: expected_key_ty.as_str(),
                                found: cur_key_ty.as_str(),
                            });
                            had_error = true;
                            continue;
                        }

                        if !types_are_eq(&expected_val_ty, &cur_val_ty) {
                            state.push_err(CheckerError::TypeMismatch {
                                loc: cur_pair.val.get_source_ref(),
                                expected: expected_val_ty.as_str(),
                                found: cur_val_ty.as_str(),
                            });
                            had_error = true;
                            continue;
                        }

                        pair_ty_exprs.push(TyHashMapPair {
                            key: cur_key_ty_expr.unwrap(),
                            val: cur_val_ty_expr.unwrap(),
                        });
                    }

                    if had_error {
                        return (Rc::new(Ty::ErrorType { loc: loc.clone() }), None);
                    }

                    (
                        Rc::new(Ty::HashMap {
                            key_ty: expected_key_ty,
                            val_ty: expected_val_ty,
                            loc: loc.clone(),
                        }),
                        Some(TyExpr::HashMap {
                            pairs: pair_ty_exprs,
                        }),
                    )
                }
            }
        }
    }
}

pub fn check_ins_for_return(
    i: &Ins,
    context_ty: &Option<Rc<Ty>>,
    state: &mut State,
) -> Option<TyIns> {
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

pub fn check_ins(i: &Ins, context_ty: &Option<Rc<Ty>>, state: &mut State) -> Option<TyIns> {
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
                ty: Rc::new(RefCell::new((*expr_ty).clone())),
                refs: vec![loc.clone()],
                is_const: true,
                is_initialized: true,
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
                        ty: Rc::new(RefCell::new((*expr_ty).clone())),
                        refs: vec![loc.clone()],
                        is_const: false,
                        is_initialized: true,
                    };
                    state.env.add(name.as_str(), info);
                    ty_ins
                }
                (Some(ty), None) => {
                    let var_ty = if type_is_valid(ty).is_err() {
                        Rc::new(Ty::ErrorType { loc: loc.clone() })
                    } else {
                        ty.clone()
                    };
                    // TODO: verify that ty is an actual known type (builtin)
                    // or user defined
                    // For static arrays where the size and type are not specified
                    // we will need to either enforce providing them or
                    let info = NameInfo {
                        ty: Rc::new(RefCell::new((*var_ty).clone())),
                        refs: vec![loc.clone()],
                        is_const: false,
                        is_initialized: false,
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
                        ty: Rc::new(RefCell::new((*expr_ty).clone())),
                        refs: vec![loc.clone()],
                        is_const: false,
                        is_initialized: true,
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
        Ins::DeclFunc {
            name,
            params,
            ret_type,
            body,
            loc,
            is_const,
            ..
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
                        Rc::new(Ty::ErrorType {
                            loc: ret_type.get_loc(),
                        })
                    }
                    TypeValidErr::Incomplete => {
                        state.push_err(CheckerError::IncompleteType {
                            loc: e_loc,
                            type_name: ret_type.as_str(),
                        });
                        Rc::new(Ty::ErrorType {
                            loc: ret_type.get_loc(),
                        })
                    }
                },
            };

            if ret_ty.is_error_ty() {
                return None;
            }

            let mut fn_type_params = vec![];
            let mut pairs_to_register = vec![];
            let mut has_error = false;
            for param in params.into_iter() {
                let p_ty = match type_is_valid(&param.given_ty) {
                    Ok(_) => param.given_ty.clone(),
                    Err((e, e_loc)) => match e {
                        TypeValidErr::Invalid => {
                            state.push_err(CheckerError::InvalidType {
                                loc: e_loc,
                                type_name: param.given_ty.as_str(),
                            });
                            Rc::new(Ty::ErrorType {
                                loc: param.given_ty.get_loc(),
                            })
                        }
                        TypeValidErr::Incomplete => {
                            state.push_err(CheckerError::IncompleteType {
                                loc: e_loc,
                                type_name: param.given_ty.as_str(),
                            });
                            Rc::new(Ty::ErrorType {
                                loc: param.given_ty.get_loc(),
                            })
                        }
                    },
                };
                if p_ty.is_error_ty() {
                    has_error = true;
                }
                fn_type_params.push(p_ty.clone());
                pairs_to_register.push((
                    param.name.as_str(),
                    NameInfo {
                        ty: Rc::new(RefCell::new((*p_ty).clone())),
                        refs: vec![param.loc.clone()],
                        is_const: true,
                        is_initialized: true,
                    },
                ));
            }

            if has_error {
                return None;
            }

            let fn_type = Rc::new(RefCell::new(Ty::Func {
                params: fn_type_params,
                ret: ret_ty.clone(),
                loc: loc.clone(),
                is_const: *is_const,
            }));

            let fn_info = NameInfo {
                ty: fn_type,
                refs: vec![loc.clone()],
                is_const: true,
                is_initialized: true,
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
                    given_ty: Rc::new(param_info.ty.borrow().clone()),
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

            let is_const_method = if *is_const {
                // determine if we are inside a method:
                let mut fn_count_to_struct = 0;
                let mut in_struct = false;

                for scope in state.scope_stack.iter().rev() {
                    if matches!(scope, Scope::Func) {
                        fn_count_to_struct += 1;
                        continue;
                    }

                    if matches!(scope, Scope::Struct) {
                        in_struct = true;
                        break;
                    }
                }

                if in_struct && fn_count_to_struct == 1 {
                    true
                } else {
                    false
                }
            } else {
                false
            };

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
                is_const_method,
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
                    } else if !matches!(cond_ty.as_ref(), Ty::Bool { .. }) {
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
        Ins::DeclFuncDefinition {
            name,
            params,
            ret_type,
            is_const,
            loc,
        } => {
            let fn_info = state.env.shallow_lookup(&name.as_str());
            if fn_info.is_some() {
                state.push_err(CheckerError::NameAlreadyDefined {
                    loc: name.get_source_ref(),
                    name: name.as_str(),
                });
                return None;
            }

            // construct the function definition type and add it to the environment
            let ret_ty = match type_is_valid(ret_type) {
                Ok(_) => ret_type.clone(),
                Err((e, e_loc)) => match e {
                    TypeValidErr::Invalid => {
                        state.push_err(CheckerError::InvalidType {
                            loc: e_loc,
                            type_name: ret_type.as_str(),
                        });
                        Rc::new(Ty::ErrorType {
                            loc: ret_type.get_loc(),
                        })
                    }
                    TypeValidErr::Incomplete => {
                        state.push_err(CheckerError::IncompleteType {
                            loc: e_loc,
                            type_name: ret_type.as_str(),
                        });
                        Rc::new(Ty::ErrorType {
                            loc: ret_type.get_loc(),
                        })
                    }
                },
            };

            if ret_ty.is_error_ty() {
                return None;
            }

            let mut fn_type_params = vec![];
            let mut has_error = false;
            let mut ty_params = vec![];
            for param in params.into_iter() {
                let p_ty = match type_is_valid(&param.given_ty) {
                    Ok(_) => param.given_ty.clone(),
                    Err((e, e_loc)) => match e {
                        TypeValidErr::Invalid => {
                            state.push_err(CheckerError::InvalidType {
                                loc: e_loc,
                                type_name: param.given_ty.as_str(),
                            });
                            Rc::new(Ty::ErrorType {
                                loc: param.given_ty.get_loc(),
                            })
                        }
                        TypeValidErr::Incomplete => {
                            state.push_err(CheckerError::IncompleteType {
                                loc: e_loc,
                                type_name: param.given_ty.as_str(),
                            });
                            Rc::new(Ty::ErrorType {
                                loc: param.given_ty.get_loc(),
                            })
                        }
                    },
                };
                if p_ty.is_error_ty() {
                    has_error = true;
                }
                fn_type_params.push(p_ty.clone());
                ty_params.push(TyFnParam {
                    name: param.name.as_str(),
                    given_ty: p_ty.clone(),
                });
            }

            if has_error {
                return None;
            }

            let fn_type = Rc::new(RefCell::new(Ty::Func {
                params: fn_type_params,
                ret: ret_ty.clone(),
                loc: loc.clone(),
                is_const: *is_const,
            }));

            let fn_info = NameInfo {
                ty: fn_type,
                refs: vec![loc.clone()],
                is_const: true,
                is_initialized: true,
            };
            state.env.add(name.as_str(), fn_info);

            if state.is_in_x_scope(&Scope::Trait) {
                Some(TyIns::TraitFuncDefinition {
                    name: name.as_str(),
                    params: ty_params,
                    ret_ty: ret_type.clone(),
                    is_const: *is_const,
                })
            } else {
                unreachable!("Function Definition AST outside trait")
            }
        }
        Ins::DeclTrait {
            name,
            func_defs,
            loc,
        } => {
            // we will need to check the body and update the struct type under the name
            // as we get new information (fields, associated functions and methods)
            let name_info = state.env.lookup(&name.as_str());
            if name_info.is_some() {
                state.push_err(CheckerError::NameAlreadyDefined {
                    loc: loc.clone(),
                    name: name.as_str(),
                });
                return None;
            }

            let mut trait_ty = Rc::new(RefCell::new(Ty::Trait {
                name: name.as_str(),
                funcs: HashMap::new(),
                loc: loc.clone(),
            }));

            todo!()
        }
        Ins::DeclStruct {
            name,
            fields,
            funcs,
            loc,
        } => {
            // we will need to check the body and update the struct type under the name
            // as we get new information (fields, associated functions and methods)
            let name_info = state.env.lookup(&name.as_str());
            if name_info.is_some() {
                state.push_err(CheckerError::NameAlreadyDefined {
                    loc: loc.clone(),
                    name: name.as_str(),
                });
                return None;
            }

            let mut struct_ty = Rc::new(RefCell::new(Ty::Struct {
                name: name.as_str(),
                fields: HashMap::new(),
                funcs: HashMap::new(),
                loc: loc.clone(),
            }));

            let mut info = NameInfo {
                ty: struct_ty.clone(),
                refs: vec![name.get_source_ref()],
                is_const: true,
                is_initialized: true,
            };

            state.env.add(name.as_str(), info.clone());
            state.env.extend();
            state.scope_stack.push(Scope::Struct);
            // add instance variable called self
            let mut self_info = NameInfo {
                ty: struct_ty.clone(),
                refs: vec![name.get_source_ref()],
                is_const: false,
                is_initialized: true,
            };
            state.env.add("self".into(), self_info.clone());
            // this scope is temporary, only used for checking fields. Since accessing
            // fields without self is not allowed, it will be popped after checking fields
            state.env.extend();

            let mut ty_fs = vec![];
            for f in fields {
                let ty_f = check_ins(f, &None, state);

                if let Some(ty_f) = ty_f {
                    // we can get the type from the field
                    match &ty_f {
                        TyIns::Var { name: f_name, .. }
                        | TyIns::Constant { name: f_name, .. }=> {
                            let v_info = state.env.shallow_lookup(&f_name).unwrap();

                            if v_info.ty.borrow().is_error_ty() {
                                continue;
                            }
                            let mut b_struct_ty = &mut *(*struct_ty).borrow_mut();
                            if let Ty::Struct { fields, .. } = b_struct_ty {
                                fields.insert(f_name.clone(), Rc::new(v_info.ty.borrow().clone()));
                            }
                        }
                        _ => unreachable!("seman::check_ins(): found an unexpected instruction in fields vec for Struct."),
                    }
                    ty_fs.push(ty_f);
                }
            }

            state.env.pop();

            let mut ty_fns = vec![];
            for func in funcs {
                // determine if it is a const function. if it is make self const for the check
                let is_const = if let Ins::DeclFunc { is_const, .. } = func {
                    let self_inst = state.env.shallow_lookup("self").unwrap();
                    self_inst.is_const = *is_const;
                    *is_const
                } else {
                    false
                };

                let ty_fn = check_ins(func, &None, state);

                if let Some(ty_fn) = ty_fn {
                    match &ty_fn {
                        TyIns::Func { name: fn_name, .. } => {
                            let fn_info = state.env.shallow_lookup(&fn_name).unwrap();

                            if fn_info.ty.borrow().is_error_ty() {
                                continue;
                            }
                            let mut b_struct_ty = &mut *(*struct_ty).borrow_mut();
                            if let Ty::Struct { funcs, .. } = b_struct_ty {
                                funcs.insert(fn_name.clone(), Rc::new(fn_info.ty.borrow().clone()));
                            }
                        }
                        _ => unreachable!("seman::check_ins(): found an unexpected instruction in funcs vec for Struct."),
                    }
                    ty_fns.push(ty_fn);
                }
            }

            state.scope_stack.pop();
            state.env.pop();
            info.ty = struct_ty;
            state.env.add(name.as_str(), info);
            Some(TyIns::Struct {
                name: name.as_str(),
                fields: ty_fs,
                funcs: ty_fns,
            })
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
        Ins::AssignTo { target, value, loc } => {
            // determine if the target can be assigned to then do the assignment
            let (ty_targ, ty_targ_expr) = check_expr(target, &None, state);

            if ty_targ.is_error_ty() {
                return None;
            }

            let mut cur = target;
            let mut depth = 0;

            loop {
                match cur {
                    Expr::Ident { name, loc } => {
                        // check the environment to see if it is modifiable
                        // we will need to check the body and update the struct type under the name
                        // as we get new information (fields, associated functions and methods)
                        let name_info = state.env.lookup(&name).unwrap();
                        if name_info.is_const {
                            state.push_err(CheckerError::CannotAssignToImmutableTarget {
                                target: name.clone(),
                                loc: loc.clone(),
                            });
                            return None;
                        }

                        if depth == 0 {
                            name_info.is_initialized = true;
                        }
                        break;
                    }
                    Expr::IndexInto { target, index, loc } => {
                        cur = target;
                        depth += 1;
                    }
                    Expr::AccessMember { target, mem, loc } => {
                        // TODO: determine if the mem is a const struct field.
                        cur = target;
                        depth += 1;
                    }
                    _ => state.push_err(CheckerError::CannotAssignToTarget {
                        target: target.as_str(),
                        loc: cur.get_source_ref(),
                    }),
                }
            }

            let (ty_val, ty_val_expr) = check_expr(value, &Some(ty_targ.clone()), state);
            if ty_val.is_error_ty() {
                return None;
            }

            if !types_are_eq(&ty_targ, &ty_val) {
                state.push_err(CheckerError::TypeMismatch {
                    loc: loc.clone(),
                    expected: ty_targ.as_str(),
                    found: ty_val.as_str(),
                });
                return None;
            }

            Some(TyIns::AssignTo {
                target: ty_targ_expr.unwrap(),
                val: ty_val_expr.unwrap(),
            })
        }
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
                if state.is_in_x_scope(&Scope::Defer) {
                    // check if we are inside a function inside a defer
                    state.push_err(CheckerError::CannotReturnFromInsideADeferIns {
                        loc: loc.clone(),
                    });
                    return None;
                }
                let (expr_ty, ret_ty_expr) = check_expr(e, context_ty, state);
                if let Some(context_ty) = context_ty {
                    if !expr_ty.is_error_ty() && matches!(context_ty.as_ref(), Ty::Void { .. }) {
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
                    if !matches!(context_ty.as_ref(), Ty::Void { .. }) {
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

            if !matches!(output_ty.as_ref(), Ty::Str { .. }) {
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
        Ins::Defer { sub_ins, loc } => {
            // for defer, we just need to check the sub ins
            state.scope_stack.push(Scope::Defer);
            let ty_sub_ins = check_ins(sub_ins, context_ty, state);
            state.scope_stack.pop();
            if ty_sub_ins.is_none() {
                return None;
            }

            Some(TyIns::Defer {
                sub_ins: Box::new(ty_sub_ins.unwrap()),
            })
        }
        Ins::Break { loc } | Ins::Continue { loc } => {
            let first_major_scope = state.first_major_scope();
            let (control_ty, ty_ins) = if matches!(i, Ins::Break { .. }) {
                ("break".to_string(), TyIns::Break)
            } else {
                ("continue".to_string(), TyIns::Continue)
            };
            if !matches!(first_major_scope, Scope::Loop) {
                state.push_err(CheckerError::LoopControlInstructionOutsideLoop {
                    ty: control_ty,
                    loc: loc.clone(),
                });
            }
            Some(ty_ins)
        }
        Ins::ForInLoop {
            loop_var,
            loop_target,
            block,
            loc,
        } => {
            // we will check the loop target and use its type to determine the type
            // of the loop var, which will be a constant
            let (ty_targ, ty_targ_expr) = check_expr(loop_target, &None, state);

            if ty_targ.is_error_ty() {
                return None;
            }

            if !ty_targ.is_indexable() {
                state.push_err(CheckerError::InvalidForInLoopTargetType {
                    given_ty: ty_targ.as_str(),
                    loc: loop_target.get_source_ref(),
                });
                return None;
            }

            // create new scope to add loop variable(s) and check loop body with
            state.env.extend();
            state.scope_stack.push(Scope::Loop);
            let old_enter_new_scope = state.enter_new_scope;
            state.enter_new_scope = false;

            let mut is_destructured = false;
            match loop_var {
                Expr::Ident { name, loc } => {
                    // now we can grab the type of the loop constant
                    let loop_var_ty = match ty_targ.as_ref() {
                        Ty::Str { .. } => Ty::Char {
                            loc: loop_var.get_source_ref(),
                        },
                        Ty::StaticArray { sub_ty, loc, .. } | Ty::Slice { sub_ty, loc } => {
                            let n_sub_ty = sub_ty.clone_loc(loop_var.get_source_ref());
                            (*n_sub_ty).clone()
                        }
                        Ty::HashMap { .. } => {
                            state.push_err(CheckerError::Expected("a destructure list of 2 identifiers to bind the keys and values of the hashmap.".to_string(), loc.clone(), None));
                            return None;
                        }
                        _ => unreachable!(
                            "seman::check_ins(): Unexpected indexable type in for-in loop."
                        ),
                    };

                    let loop_var_info = NameInfo {
                        ty: Rc::new(RefCell::new(loop_var_ty)),
                        refs: vec![loop_var.get_source_ref()],
                        is_const: true,
                        is_initialized: true,
                    };
                    state.env.add(loop_var.as_str(), loop_var_info);

                    let new_body = check_ins(block, &None, state);

                    if new_body.is_none() {
                        return None;
                    }

                    // exit scope after checks
                    state.enter_new_scope = old_enter_new_scope;
                    state.scope_stack.pop();
                    state.env.pop();

                    Some(TyIns::ForInLoop {
                        var: loop_var.as_str(),
                        target: ty_targ_expr.unwrap(),
                        block: Box::new(new_body.unwrap()),
                    })
                }
                Expr::StaticArray { vals, loc } => {
                    is_destructured = true;
                    let (f_loop_var_ty, s_loop_var_ty) = match ty_targ.as_ref() {
                        Ty::HashMap { key_ty, val_ty, .. } => {
                            let n_key_ty = key_ty.clone_loc(vals[0].get_source_ref());
                            let n_val_ty = val_ty.clone_loc(vals[1].get_source_ref());
                            ((*n_key_ty).clone(), (*n_val_ty).clone())
                        }
                        _ => {
                            state.push_err(CheckerError::Expected("a hashmap to be the target of the for-in loop, given the destructure list.".to_string(), loc.clone(), None));
                            return None;
                        }
                    };

                    let f_loop_var_info = NameInfo {
                        ty: Rc::new(RefCell::new(f_loop_var_ty)),
                        refs: vec![vals[0].get_source_ref()],
                        is_const: true,
                        is_initialized: true,
                    };

                    let s_loop_var_info = NameInfo {
                        ty: Rc::new(RefCell::new(s_loop_var_ty)),
                        refs: vec![vals[1].get_source_ref()],
                        is_const: true,
                        is_initialized: true,
                    };

                    state.env.add(vals[0].as_str(), f_loop_var_info);
                    state.env.add(vals[1].as_str(), s_loop_var_info);

                    let new_body = check_ins(block, &None, state);

                    if new_body.is_none() {
                        return None;
                    }

                    // exit scope after checks
                    state.enter_new_scope = old_enter_new_scope;
                    state.scope_stack.pop();
                    state.env.pop();

                    Some(TyIns::ForInLoopDestructured {
                        vars: vec![vals[0].as_str(), vals[1].as_str()],
                        target: ty_targ_expr.unwrap(),
                        block: Box::new(new_body.unwrap()),
                    })
                }
                _ => unreachable!(),
            }
        }
        Ins::InfiniteLoop { block, loc } => {
            // we simply have to enter a new scope and check the body
            // create new scope to add loop variable and check loop body with
            state.env.extend();
            state.scope_stack.push(Scope::Loop);
            let old_enter_new_scope = state.enter_new_scope;
            state.enter_new_scope = false;

            let new_body = check_ins(block, &None, state);

            if new_body.is_none() {
                return None;
            }

            // exit scope after checks
            state.enter_new_scope = old_enter_new_scope;
            state.scope_stack.pop();
            state.env.pop();

            Some(TyIns::WhileLoop {
                block: Box::new(new_body.unwrap()),
                cond: TyExpr::Bool { val: true },
            })
        }
        Ins::WhileLoop {
            cond,
            post_code,
            block,
            loc,
        } => {
            // first we need to make sure that cond is typed bool
            let (cond_ty, cond_ty_expr) = check_expr(
                cond,
                &Some(Rc::new(Ty::Bool {
                    loc: cond.get_source_ref(),
                })),
                state,
            );

            if cond_ty.is_error_ty() {
                return None;
            }

            if !matches!(cond_ty.as_ref(), Ty::Bool { .. }) {
                state.push_err(CheckerError::TypeMismatch {
                    loc: cond.get_source_ref(),
                    expected: "bool".into(),
                    found: cond_ty.as_str(),
                });
                return None;
            }

            // we can check the post before we check the body
            let ty_post_code = if let Some(post_code) = post_code {
                let deferred_post_code = Ins::Defer {
                    sub_ins: post_code.clone(),
                    loc: post_code.get_source_ref(),
                };
                check_ins(&deferred_post_code, context_ty, state)
            } else {
                None
            };

            // then we can enter a new scope, check body and post code
            state.env.extend();
            state.scope_stack.push(Scope::Loop);
            let old_enter_new_scope = state.enter_new_scope;
            state.enter_new_scope = false;

            let new_body = check_ins(block, &None, state);

            if new_body.is_none() {
                return None;
            }

            let mut new_body = new_body.unwrap();

            if let TyIns::Block { code } = &mut new_body {
                // we can now typecheck the post_code and append it to code
                if let Some(ty_post_code) = ty_post_code {
                    // put the deferred code into the first statement of the block
                    code.insert(0, ty_post_code);
                }
            }

            // exit scope after checks
            state.enter_new_scope = old_enter_new_scope;
            state.scope_stack.pop();
            state.env.pop();

            // create a while loop instruction with cond and the concatenation
            // of body and post code
            Some(TyIns::WhileLoop {
                cond: cond_ty_expr.unwrap(),
                block: Box::new(new_body),
            })
        }
        Ins::RegLoop {
            init,
            loop_cond,
            update,
            block,
            loc,
        } => {
            // first we will need to enter a new scope, and call check_ins on the init
            // instruction
            state.env.extend();
            state.scope_stack.push(Scope::Loop);
            let old_enter_new_scope = state.enter_new_scope;
            state.enter_new_scope = false;
            let ty_init = check_ins(init, context_ty, state);

            if ty_init.is_none() {
                return None;
            }

            // then we will check to make sure the loop condition has a boolean type
            let (cond_ty, cond_ty_expr) = check_expr(loop_cond, context_ty, state);
            if cond_ty.is_error_ty() {
                return None;
            }

            if !matches!(cond_ty.as_ref(), Ty::Bool { .. }) {
                state.push_err(CheckerError::TypeMismatch {
                    loc: loop_cond.get_source_ref(),
                    expected: "bool".into(),
                    found: cond_ty.as_str(),
                });
            }

            // then we will check the update instruction
            let ty_update = check_ins(update, context_ty, state);

            if ty_update.is_none() {
                return None;
            }

            // we can finally check the body and be done
            let new_body = check_ins(block, context_ty, state);

            if new_body.is_none() {
                return None;
            }

            state.enter_new_scope = old_enter_new_scope;
            state.scope_stack.pop();
            state.env.pop();

            Some(TyIns::RegLoop {
                init: Box::new(ty_init.unwrap()),
                cond: cond_ty_expr.unwrap(),
                update: Box::new(ty_update.unwrap()),
                block: Box::new(new_body.unwrap()),
            })
        }
        Ins::Free { target, loc } => {
            // we have to make sure target is of pointer type
            let (ty_targ, ty_targ_expr) = check_expr(target, &None, state);

            if ty_targ.is_error_ty() {
                return None;
            }

            if !ty_targ.is_pointer() {
                state.push_err(CheckerError::CannotFreeNonPtrType {
                    given_ty: ty_targ.as_str(),
                    loc: target.get_source_ref(),
                });
                return None;
            }

            // we can now return a free instruction
            Some(TyIns::Free {
                target: Box::new(ty_targ_expr.unwrap()),
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
