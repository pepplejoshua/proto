#![allow(unused)]

use std::{cell::Cell, collections::HashMap, rc::Rc};

use crate::{
    parser::{
        ast::{Expr, FnParam, Ins},
        type_signature::Ty,
    },
    source::{
        errors::SemanError,
        source::{SourceFile, SourceRef},
    },
};

use super::typed_ast::{SignedInteger, TypedExpr, TypedIns, UnsignedInteger};

#[derive(Debug, Clone)]
enum SymInfo {
    Fn {
        params: Vec<FnParam>,
        return_type: Rc<Ty>,
        is_global: bool,
        def_loc: Rc<SourceRef>,
        ref_count: Cell<usize>,
    },
    Variable {
        ty: Rc<Ty>,
        is_mutable: bool,
        def_loc: Rc<SourceRef>,
        ref_count: Cell<usize>,
    },
    TypeAlias {
        target_ty: Rc<Ty>,
        def_loc: Rc<SourceRef>,
        ref_count: Cell<usize>,
    },
}

impl SymInfo {
    fn get_loc(&self) -> Rc<SourceRef> {
        match self {
            SymInfo::Fn { def_loc, .. }
            | SymInfo::Variable { def_loc, .. }
            | SymInfo::TypeAlias { def_loc, .. } => def_loc.clone(),
        }
    }

    fn as_str(&self) -> String {
        match self {
            SymInfo::Fn {
                params,
                return_type,
                is_global,
                def_loc,
                ..
            } => format!(
                "\\({}) {}",
                params
                    .iter()
                    .map(|param| { format!("{} {}", param.name.as_str(), param.given_ty.as_str()) })
                    .collect::<Vec<String>>()
                    .join(", "),
                return_type.as_str()
            ),
            SymInfo::Variable {
                ty,
                is_mutable,
                def_loc,
                ..
            } => format!(
                "{} | {}",
                if *is_mutable { "var" } else { "const " },
                ty.as_str()
            ),
            SymInfo::TypeAlias {
                target_ty, def_loc, ..
            } => todo!(),
        }
    }
}

/// A scope can exist at any level (global, function, block)
#[derive(Debug)]
struct Scope {
    symbols: HashMap<String, SymInfo>,
    parent_idx: Option<usize>,
}

impl Scope {
    /// Create a new root scope (typically global)
    fn new_root() -> Self {
        Self {
            symbols: HashMap::new(),
            parent_idx: None,
        }
    }

    /// Create a new child scope
    fn new_child(parent_idx: usize) -> Self {
        Self {
            symbols: HashMap::new(),
            parent_idx: Some(parent_idx),
        }
    }

    /// Add a symbol to current scope
    fn add_symbol(&mut self, name: String, info: SymInfo) -> Result<(), String> {
        if self.symbols.contains_key(&name) {
            return Err(format!(
                "Symbol '{}' already declared in current scope",
                name
            ));
        }
        self.symbols.insert(name, info);
        Ok(())
    }
}

pub struct SemanticAnalyzer {
    scopes: Vec<Scope>,
    current_scope_idx: usize,
    errors: Vec<SemanError>,
    main_src_file: SourceFile,
}

impl SemanticAnalyzer {
    fn new(main_src_file: SourceFile) -> Self {
        let mut scopes = vec![Scope::new_root()];
        Self {
            scopes,
            current_scope_idx: 0,
            errors: vec![],
            main_src_file,
        }
    }

    fn lookup(&mut self, name: &str) -> Option<&SymInfo> {
        let mut current_idx = self.current_scope_idx;

        loop {
            // Check current scope
            if let Some(symbol) = self.scopes[current_idx].symbols.get(name) {
                return Some(symbol);
            }

            // Move to parent scope if it exists
            match self.scopes[current_idx].parent_idx {
                Some(parent_idx) => current_idx = parent_idx,
                // Hit global scope (no parent) without finding symbol
                None => return None,
            }
        }
    }

    fn shallow_lookup(&self, name: &str) -> Option<&SymInfo> {
        self.scopes[self.current_scope_idx].symbols.get(name)
    }

    fn lookup_function(&mut self, name: &str) -> Option<(Vec<FnParam>, Rc<Ty>)> {
        let sym_info = self.lookup(name);
        if let Some(SymInfo::Fn {
            params,
            return_type,
            ..
        }) = sym_info
        {
            Some((params.clone(), return_type.clone()))
        } else {
            None
        }
    }

    fn lookup_variable(&mut self, name: &str) -> Option<(Rc<Ty>, bool)> {
        let sym_info = self.lookup(name);
        if let Some(SymInfo::Variable { ty, is_mutable, .. }) = sym_info {
            Some((ty.clone(), *is_mutable))
        } else {
            None
        }
    }

    fn name_is_in_current_scope(&self, name: &str) -> bool {
        self.scopes[self.current_scope_idx]
            .symbols
            .contains_key(name)
    }

    fn get_name_definition_loc(&mut self, name: &str) -> Option<Rc<SourceRef>> {
        match self.lookup(name) {
            Some(SymInfo::Fn { def_loc, .. })
            | Some(SymInfo::Variable { def_loc, .. })
            | Some(SymInfo::TypeAlias { def_loc, .. }) => Some(def_loc.clone()),
            None => None,
        }
    }

    fn report_error(&mut self, err: SemanError) {
        self.errors.push(err);
    }

    fn enter_scope(&mut self) {
        let new_scope = Scope::new_child(self.current_scope_idx);
        self.scopes.push(new_scope);
        self.current_scope_idx = self.scopes.len() - 1;
    }

    fn exit_scope(&mut self) {
        if let Some(parent_idx) = self.scopes[self.current_scope_idx].parent_idx {
            self.current_scope_idx = parent_idx;
        }
    }

    fn collect_declarations(&mut self, program: &[Ins]) {
        for ins in program {
            match ins {
                Ins::DeclFunc {
                    name,
                    params,
                    ret_ty,
                    body,
                    loc,
                    ..
                } => {
                    if let Expr::Identifier {
                        name: func_name, ..
                    } = name
                    {
                        if let Err(e) = self.scopes[self.current_scope_idx].add_symbol(
                            func_name.to_string(),
                            SymInfo::Fn {
                                params: params.clone(),
                                return_type: ret_ty.clone(),
                                is_global: true,
                                def_loc: loc.clone(),
                                ref_count: Cell::new(0),
                            },
                        ) {
                            self.report_error(SemanError::NameAlreadyDefined {
                                loc: loc.clone(),
                                name: func_name.to_string(),
                            });
                        }
                    }
                }
                Ins::DeclVariable {
                    name,
                    ty,
                    init_val,
                    is_mutable,
                    loc,
                    ..
                } => {
                    if let Expr::Identifier { name: var_name, .. } = name {
                        if let Some(var_ty) = ty {
                            if let Err(e) = self.scopes[self.current_scope_idx].add_symbol(
                                var_name.to_string(),
                                SymInfo::Variable {
                                    ty: var_ty.clone(),
                                    is_mutable: *is_mutable,
                                    def_loc: loc.clone(),
                                    ref_count: Cell::new(0),
                                },
                            ) {
                                self.report_error(SemanError::NameAlreadyDefined {
                                    loc: loc.clone(),
                                    name: var_name.to_string(),
                                });
                            }
                        } else {
                            self.report_error(SemanError::TypeMismatch {
                                loc: loc.clone(),
                                expected: "an explicit type".into(),
                                found: "none".into(),
                            });
                        }
                    }
                }
                Ins::DeclTypeAlias { name, ty, loc, .. } => {
                    todo!()
                }
                // ignore other instructions in Pass 1
                _ => {}
            }
        }
    }

    fn types_are_compatible(lhs: &Rc<Ty>, rhs: &Rc<Ty>) -> bool {
        match (lhs.as_ref(), rhs.as_ref()) {
            (
                Ty::Signed {
                    size: l_size,
                    is_int: l_is_int,
                    ..
                },
                Ty::Signed {
                    size: r_size,
                    is_int: r_is_int,
                    ..
                },
            ) => l_size == r_size && l_is_int == r_is_int,
            (
                Ty::Unsigned {
                    size: l_size,
                    is_uint: l_is_uint,
                    ..
                },
                Ty::Unsigned {
                    size: r_size,
                    is_uint: r_is_uint,
                    ..
                },
            ) => l_size == r_size && l_is_uint == r_is_uint,
            (Ty::Float { size, .. }, Ty::Float { size: r_size, .. }) => size == r_size,
            (Ty::Char { .. }, Ty::Char { .. }) => true,
            (Ty::Str { .. }, Ty::Str { .. }) => true,
            (Ty::Void { .. }, Ty::Void { .. }) => true,
            _ => todo!(),
        }
    }

    fn is_integer_type(ty: &Ty) -> bool {
        matches!(ty, Ty::Signed { is_int: true, .. })
    }

    fn is_unsigned_integer_type(&self, ty: &Ty) -> bool {
        matches!(ty, Ty::Unsigned { is_uint: true, .. })
    }

    fn get_default_value(&self, ty: &Rc<Ty>, loc: &Rc<SourceRef>) -> TypedExpr {
        match ty.as_ref() {
            Ty::Signed { size, is_int, .. } => {
                if *is_int {
                    TypedExpr::SignedInt {
                        value: SignedInteger::Int(0),
                        ty: ty.clone(),
                        loc: loc.clone(),
                    }
                } else {
                    match size {
                        8 => TypedExpr::SignedInt {
                            value: SignedInteger::I8(0),
                            ty: ty.clone(),
                            loc: loc.clone(),
                        },
                        16 => TypedExpr::SignedInt {
                            value: SignedInteger::I16(0),
                            ty: ty.clone(),
                            loc: loc.clone(),
                        },
                        32 => TypedExpr::SignedInt {
                            value: SignedInteger::I32(0),
                            ty: ty.clone(),
                            loc: loc.clone(),
                        },
                        64 => TypedExpr::SignedInt {
                            value: SignedInteger::I64(0),
                            ty: ty.clone(),
                            loc: loc.clone(),
                        },
                        _ => unreachable!("Unexpected integer size [get_default_value()]"),
                    }
                }
            }
            Ty::Unsigned { size, is_uint, .. } => {
                if *is_uint {
                    TypedExpr::UnsignedInt {
                        value: UnsignedInteger::Uint(0),
                        ty: ty.clone(),
                        loc: loc.clone(),
                    }
                } else {
                    match size {
                        8 => TypedExpr::UnsignedInt {
                            value: UnsignedInteger::U8(0),
                            ty: ty.clone(),
                            loc: loc.clone(),
                        },
                        16 => TypedExpr::UnsignedInt {
                            value: UnsignedInteger::U16(0),
                            ty: ty.clone(),
                            loc: loc.clone(),
                        },
                        32 => TypedExpr::UnsignedInt {
                            value: UnsignedInteger::U32(0),
                            ty: ty.clone(),
                            loc: loc.clone(),
                        },
                        64 => TypedExpr::UnsignedInt {
                            value: UnsignedInteger::U64(0),
                            ty: ty.clone(),
                            loc: loc.clone(),
                        },
                        _ => unreachable!("Unexpected unsigned integer size [get_default_value()]"),
                    }
                }
            }
            Ty::Float { size, loc } => TypedExpr::Float {
                value: 0.0,
                ty: ty.clone(),
                loc: loc.clone(),
            },
            Ty::Str { loc } => TypedExpr::Str {
                value: "".into(),
                ty: ty.clone(),
                loc: loc.clone(),
            },
            Ty::Char { loc } => TypedExpr::Char {
                value: '\0',
                ty: ty.clone(),
                loc: loc.clone(),
            },
            Ty::Void { loc } => todo!(),
            Ty::Bool { loc } => TypedExpr::Bool {
                value: false,
                ty: ty.clone(),
                loc: loc.clone(),
            },
            Ty::Func {
                params,
                ret,
                loc,
                is_const,
            } => todo!(),
            Ty::StaticArray { sub_ty, size, loc } => todo!(),
            Ty::Slice { sub_ty, loc } => todo!(),
            Ty::Optional { sub_ty, loc } => todo!(),
            Ty::Struct {
                fields,
                static_funcs,
                methods,
                loc,
            } => todo!(),
            Ty::Pointer { sub_ty, loc } => todo!(),
            Ty::AccessMemberType { target, mem, loc } => todo!(),
            Ty::Tuple { sub_tys, loc } => todo!(),
            Ty::ErrorType { loc } => {
                unreachable!("Trying to make default value for error type [get_default_value()]")
            }
            Ty::NamedType { name, loc } => todo!(),
        }
    }

    fn validate_program(&mut self, program: &[Ins]) -> Vec<TypedIns> {
        // validate each top-level instruction
        let mut typed_program = vec![];
        for ins in program {
            let typed_ins = self.validate_instruction(ins);
            typed_program.push(typed_ins);
        }

        let mut additional_errors = vec![];

        // check if a main function exists and has the correct type signature
        match self.lookup("main") {
            Some(SymInfo::Fn {
                params,
                return_type,
                def_loc,
                ..
            }) => {
                if !params.is_empty() {
                    additional_errors.push(SemanError::TypeMismatch {
                        loc: def_loc.clone(),
                        expected: "\\() int".into(),
                        found: format!("a function with {} parameters", params.len()),
                    });
                }

                if !Self::is_integer_type(&return_type) {
                    additional_errors.push(SemanError::TypeMismatch {
                        loc: return_type.get_loc(),
                        expected: "int".into(),
                        found: return_type.as_str(),
                    });
                }
            }
            Some(sym_info) => {
                additional_errors.push(SemanError::Expected(
                    "a function named 'main' to serve as the entry of the program".into(),
                    sym_info.get_loc(),
                    None,
                ));
            }
            None => {
                self.report_error(SemanError::NoMainFunctionProvided {
                    filename: self.main_src_file.path.clone(),
                });
            }
        }
        self.errors.extend(additional_errors);
        typed_program
    }

    fn validate_expression(
        &mut self,
        expr: &Expr,
        parent_ty: Option<&Rc<Ty>>,
    ) -> (Rc<Ty>, TypedExpr) {
        match expr {
            Expr::Integer { content, loc } => {
                let (ty, typed_integer) = if let Some(parent_ty) = parent_ty {
                    match parent_ty.as_ref() {
                        Ty::Signed { size, is_int, .. } => {
                            if *is_int {
                                let res = content.parse::<isize>();
                                match res {
                                    Ok(val) => (
                                        parent_ty.clone(),
                                        TypedExpr::SignedInt {
                                            value: SignedInteger::Int(val),
                                            ty: parent_ty.clone(),
                                            loc: loc.clone(),
                                        },
                                    ),
                                    Err(_) => {
                                        self.report_error(SemanError::IntegerTypeCheckFailed {
                                            loc: loc.clone(),
                                            number: content.to_string(),
                                            given_type: parent_ty.as_str(),
                                        });
                                        (
                                            Rc::new(Ty::ErrorType { loc: loc.clone() }),
                                            TypedExpr::Error,
                                        )
                                    }
                                }
                            } else {
                                match size {
                                    8 => {
                                        let res = content.parse::<i8>();
                                        match res {
                                            Ok(val) => (
                                                parent_ty.clone(),
                                                TypedExpr::SignedInt {
                                                    value: SignedInteger::I8(val),
                                                    ty: parent_ty.clone(),
                                                    loc: loc.clone(),
                                                },
                                            ),
                                            Err(_) => {
                                                self.report_error(
                                                    SemanError::IntegerTypeCheckFailed {
                                                        loc: loc.clone(),
                                                        number: content.to_string(),
                                                        given_type: parent_ty.as_str(),
                                                    },
                                                );
                                                (
                                                    Rc::new(Ty::ErrorType { loc: loc.clone() }),
                                                    TypedExpr::Error,
                                                )
                                            }
                                        }
                                    }
                                    16 => {
                                        let res = content.parse::<i16>();
                                        match res {
                                            Ok(val) => (
                                                parent_ty.clone(),
                                                TypedExpr::SignedInt {
                                                    value: SignedInteger::I16(val),
                                                    ty: parent_ty.clone(),
                                                    loc: loc.clone(),
                                                },
                                            ),
                                            Err(_) => {
                                                self.report_error(
                                                    SemanError::IntegerTypeCheckFailed {
                                                        loc: loc.clone(),
                                                        number: content.to_string(),
                                                        given_type: parent_ty.as_str(),
                                                    },
                                                );
                                                (
                                                    Rc::new(Ty::ErrorType { loc: loc.clone() }),
                                                    TypedExpr::Error,
                                                )
                                            }
                                        }
                                    }
                                    32 => {
                                        let res = content.parse::<i32>();
                                        match res {
                                            Ok(val) => (
                                                parent_ty.clone(),
                                                TypedExpr::SignedInt {
                                                    value: SignedInteger::I32(val),
                                                    ty: parent_ty.clone(),
                                                    loc: loc.clone(),
                                                },
                                            ),
                                            Err(_) => {
                                                self.report_error(
                                                    SemanError::IntegerTypeCheckFailed {
                                                        loc: loc.clone(),
                                                        number: content.to_string(),
                                                        given_type: parent_ty.as_str(),
                                                    },
                                                );
                                                (
                                                    Rc::new(Ty::ErrorType { loc: loc.clone() }),
                                                    TypedExpr::Error,
                                                )
                                            }
                                        }
                                    }
                                    64 => {
                                        let res = content.parse::<i64>();
                                        match res {
                                            Ok(val) => (
                                                parent_ty.clone(),
                                                TypedExpr::SignedInt {
                                                    value: SignedInteger::I64(val),
                                                    ty: parent_ty.clone(),
                                                    loc: loc.clone(),
                                                },
                                            ),
                                            Err(_) => {
                                                self.report_error(
                                                    SemanError::IntegerTypeCheckFailed {
                                                        loc: loc.clone(),
                                                        number: content.to_string(),
                                                        given_type: parent_ty.as_str(),
                                                    },
                                                );
                                                (
                                                    Rc::new(Ty::ErrorType { loc: loc.clone() }),
                                                    TypedExpr::Error,
                                                )
                                            }
                                        }
                                    }
                                    _ => unreachable!(
                                        "Unexpected signed integer size [validate_expression()]"
                                    ),
                                }
                            }
                        }
                        Ty::Unsigned { size, is_uint, .. } => {
                            if *is_uint {
                                let res = content.parse::<usize>();
                                match res {
                                    Ok(val) => (
                                        parent_ty.clone(),
                                        TypedExpr::UnsignedInt {
                                            value: UnsignedInteger::Uint(val),
                                            ty: parent_ty.clone(),
                                            loc: loc.clone(),
                                        },
                                    ),
                                    Err(_) => {
                                        self.report_error(SemanError::IntegerTypeCheckFailed {
                                            loc: loc.clone(),
                                            number: content.to_string(),
                                            given_type: parent_ty.as_str(),
                                        });
                                        (
                                            Rc::new(Ty::ErrorType { loc: loc.clone() }),
                                            TypedExpr::Error,
                                        )
                                    }
                                }
                            } else {
                                match size {
                                    8 => {
                                        let res = content.parse::<u8>();
                                        match res {
                                            Ok(val) => (
                                                parent_ty.clone(),
                                                TypedExpr::UnsignedInt {
                                                    value: UnsignedInteger::U8(val),
                                                    ty: parent_ty.clone(),
                                                    loc: loc.clone(),
                                                },
                                            ),
                                            Err(_) => {
                                                self.report_error(
                                                    SemanError::IntegerTypeCheckFailed {
                                                        loc: loc.clone(),
                                                        number: content.to_string(),
                                                        given_type: parent_ty.as_str(),
                                                    },
                                                );
                                                (
                                                    Rc::new(Ty::ErrorType { loc: loc.clone() }),
                                                    TypedExpr::Error,
                                                )
                                            }
                                        }
                                    }
                                    16 => {
                                        let res = content.parse::<u16>();
                                        match res {
                                            Ok(val) => (
                                                parent_ty.clone(),
                                                TypedExpr::UnsignedInt {
                                                    value: UnsignedInteger::U16(val),
                                                    ty: parent_ty.clone(),
                                                    loc: loc.clone(),
                                                },
                                            ),
                                            Err(_) => {
                                                self.report_error(
                                                    SemanError::IntegerTypeCheckFailed {
                                                        loc: loc.clone(),
                                                        number: content.to_string(),
                                                        given_type: parent_ty.as_str(),
                                                    },
                                                );
                                                (
                                                    Rc::new(Ty::ErrorType { loc: loc.clone() }),
                                                    TypedExpr::Error,
                                                )
                                            }
                                        }
                                    }
                                    32 => {
                                        let res = content.parse::<u32>();
                                        match res {
                                            Ok(val) => (
                                                parent_ty.clone(),
                                                TypedExpr::UnsignedInt {
                                                    value: UnsignedInteger::U32(val),
                                                    ty: parent_ty.clone(),
                                                    loc: loc.clone(),
                                                },
                                            ),
                                            Err(_) => {
                                                self.report_error(
                                                    SemanError::IntegerTypeCheckFailed {
                                                        loc: loc.clone(),
                                                        number: content.to_string(),
                                                        given_type: parent_ty.as_str(),
                                                    },
                                                );
                                                (
                                                    Rc::new(Ty::ErrorType { loc: loc.clone() }),
                                                    TypedExpr::Error,
                                                )
                                            }
                                        }
                                    }
                                    64 => {
                                        let res = content.parse::<u64>();
                                        match res {
                                            Ok(val) => (
                                                parent_ty.clone(),
                                                TypedExpr::UnsignedInt {
                                                    value: UnsignedInteger::U64(val),
                                                    ty: parent_ty.clone(),
                                                    loc: loc.clone(),
                                                },
                                            ),
                                            Err(_) => {
                                                self.report_error(
                                                    SemanError::IntegerTypeCheckFailed {
                                                        loc: loc.clone(),
                                                        number: content.to_string(),
                                                        given_type: parent_ty.as_str(),
                                                    },
                                                );
                                                (
                                                    Rc::new(Ty::ErrorType { loc: loc.clone() }),
                                                    TypedExpr::Error,
                                                )
                                            }
                                        }
                                    }
                                    _ => unreachable!(
                                        "Unexpected signed integer size [validate_expression()]"
                                    ),
                                }
                            }
                        }
                        Ty::Float { size, .. } => match size {
                            32 => {
                                let res = content.parse::<f32>();
                                match res {
                                    Ok(float) => (
                                        parent_ty.clone(),
                                        TypedExpr::Float {
                                            value: float as f64,
                                            ty: parent_ty.clone(),
                                            loc: loc.clone(),
                                        },
                                    ),
                                    Err(_) => {
                                        self.report_error(SemanError::FloatTypeCheckFailed {
                                            loc: loc.clone(),
                                            number: content.to_string(),
                                            given_type: parent_ty.as_str(),
                                        });
                                        (
                                            Rc::new(Ty::ErrorType { loc: loc.clone() }),
                                            TypedExpr::Error,
                                        )
                                    }
                                }
                            }
                            64 => {
                                let res = content.parse::<f64>();
                                match res {
                                    Ok(float) => (
                                        parent_ty.clone(),
                                        TypedExpr::Float {
                                            value: float,
                                            ty: parent_ty.clone(),
                                            loc: loc.clone(),
                                        },
                                    ),
                                    Err(_) => {
                                        self.report_error(SemanError::FloatTypeCheckFailed {
                                            loc: loc.clone(),
                                            number: content.to_string(),
                                            given_type: parent_ty.as_str(),
                                        });
                                        (
                                            Rc::new(Ty::ErrorType { loc: loc.clone() }),
                                            TypedExpr::Error,
                                        )
                                    }
                                }
                            }
                            _ => unreachable!("Unexpected float size [validate_expression()]"),
                        },
                        _ => {
                            self.report_error(SemanError::TypeMismatch {
                                loc: loc.clone(),
                                expected:
                                    "a numerical type (i8..i64 | int, u8..u64 | uint, f32 | f64)."
                                        .into(),
                                found: parent_ty.as_str(),
                            });
                            (
                                Rc::new(Ty::ErrorType { loc: loc.clone() }),
                                TypedExpr::Error,
                            )
                        }
                    }
                } else {
                    let res = content.parse::<isize>();
                    let int_ty = Ty::get_int_ty(loc.clone());
                    match res {
                        Ok(val) => (
                            int_ty.clone(),
                            TypedExpr::SignedInt {
                                value: SignedInteger::Int(val),
                                ty: int_ty.clone(),
                                loc: loc.clone(),
                            },
                        ),
                        Err(_) => {
                            self.report_error(SemanError::IntegerTypeDefaultInferenceFailed {
                                loc: loc.clone(),
                                number: content.to_string(),
                            });
                            (
                                Rc::new(Ty::ErrorType { loc: loc.clone() }),
                                TypedExpr::Error,
                            )
                        }
                    }
                };
                (ty, typed_integer)
            }
            Expr::Float { content, loc } => {
                let (ty, typed_float) = if let Some(parent_ty) = parent_ty {
                    match parent_ty.as_ref() {
                        Ty::Float { size, .. } => match size {
                            32 => {
                                let res = content.parse::<f32>();
                                match res {
                                    Ok(float) => (
                                        parent_ty.clone(),
                                        TypedExpr::Float {
                                            value: float as f64,
                                            ty: parent_ty.clone(),
                                            loc: loc.clone(),
                                        },
                                    ),
                                    Err(_) => {
                                        self.report_error(SemanError::FloatTypeCheckFailed {
                                            loc: loc.clone(),
                                            number: content.to_string(),
                                            given_type: parent_ty.as_str(),
                                        });
                                        (
                                            Rc::new(Ty::ErrorType { loc: loc.clone() }),
                                            TypedExpr::Error,
                                        )
                                    }
                                }
                            }
                            64 => {
                                let res = content.parse::<f64>();
                                match res {
                                    Ok(float) => (
                                        parent_ty.clone(),
                                        TypedExpr::Float {
                                            value: float,
                                            ty: parent_ty.clone(),
                                            loc: loc.clone(),
                                        },
                                    ),
                                    Err(_) => {
                                        self.report_error(SemanError::FloatTypeCheckFailed {
                                            loc: loc.clone(),
                                            number: content.to_string(),
                                            given_type: parent_ty.as_str(),
                                        });
                                        (
                                            Rc::new(Ty::ErrorType { loc: loc.clone() }),
                                            TypedExpr::Error,
                                        )
                                    }
                                }
                            }
                            _ => unreachable!("Unexpected float size [validate_expression()]"),
                        },
                        _ => {
                            self.report_error(SemanError::TypeMismatch {
                                loc: loc.clone(),
                                expected: "a numerical type (f32 | f64).".into(),
                                found: parent_ty.as_str(),
                            });
                            (
                                Rc::new(Ty::ErrorType { loc: loc.clone() }),
                                TypedExpr::Error,
                            )
                        }
                    }
                } else {
                    let res = content.parse::<f32>();
                    let f32_type = Rc::new(Ty::Float {
                        size: 32,
                        loc: loc.clone(),
                    });
                    match res {
                        Ok(val) => (
                            f32_type.clone(),
                            TypedExpr::Float {
                                value: val as f64,
                                ty: f32_type,
                                loc: loc.clone(),
                            },
                        ),
                        Err(_) => {
                            self.report_error(SemanError::FloatTypeDefaultInferenceFailed {
                                loc: loc.clone(),
                                number: content.to_string(),
                            });
                            (
                                Rc::new(Ty::ErrorType { loc: loc.clone() }),
                                TypedExpr::Error,
                            )
                        }
                    }
                };
                (ty, typed_float)
            }
            Expr::Str { content, loc } => todo!(),
            Expr::Char { content, loc } => todo!(),
            Expr::Bool { val, loc } => todo!(),
            Expr::Tuple { items, loc } => todo!(),
            Expr::StaticArray { ty, items, loc } => todo!(),
            Expr::Identifier { name, loc } => {
                // look up the identifier and update the reference number of the name
                match self.lookup(name) {
                    Some(SymInfo::Variable { ty, ref_count, .. }) => {
                        ref_count.set(ref_count.get() + 1);
                        (
                            ty.clone(),
                            TypedExpr::Identifier {
                                name: name.to_string(),
                                ty: ty.clone(),
                                loc: loc.clone(),
                            },
                        )
                    }
                    Some(SymInfo::Fn {
                        params,
                        return_type,
                        is_global,
                        def_loc,
                        ref_count,
                    }) => {
                        ref_count.set(ref_count.get() + 1);
                        let param_tys: Vec<Rc<Ty>> =
                            params.iter().map(|param| param.given_ty.clone()).collect();
                        let fn_ty = Rc::new(Ty::Func {
                            params: param_tys,
                            ret: return_type.clone(),
                            loc: loc.clone(),
                            is_const: true,
                        });
                        (
                            fn_ty.clone(),
                            TypedExpr::Identifier {
                                name: name.to_string(),
                                ty: fn_ty,
                                loc: loc.clone(),
                            },
                        )
                    }
                    Some(SymInfo::TypeAlias {
                        target_ty,
                        def_loc,
                        ref_count,
                    }) => {
                        todo!()
                    }
                    None => {
                        self.report_error(SemanError::ReferenceToUndefinedName {
                            loc: loc.clone(),
                            var_name: name.to_string(),
                        });
                        (
                            Rc::new(Ty::ErrorType { loc: loc.clone() }),
                            TypedExpr::Error,
                        )
                    }
                }
            }
            Expr::UnaryOp { op, expr, loc } => todo!(),
            Expr::BinOp {
                op,
                left,
                right,
                loc,
            } => todo!(),
            Expr::ConditionalExpr {
                cond,
                then,
                otherwise,
                loc,
            } => todo!(),
            Expr::CallFn { func, args, loc } => {
                let (func_ty, typed_func) = self.validate_expression(func, None);

                match func_ty.as_ref() {
                    Ty::Func {
                        params: param_tys,
                        ret: return_ty,
                        ..
                    } => {
                        // validate arguments
                        if args.len() != param_tys.len() {
                            self.report_error(SemanError::IncorrectFunctionArity {
                                expected: param_tys.len(),
                                given: args.len(),
                                loc: loc.clone(),
                            });
                            return (
                                Rc::new(Ty::ErrorType { loc: loc.clone() }),
                                TypedExpr::Error,
                            );
                        }

                        let mut typed_args = vec![];
                        for (arg_expr, param_ty) in args.iter().zip(param_tys.iter()) {
                            let (arg_ty, typed_arg) =
                                self.validate_expression(arg_expr, Some(param_ty));

                            if !Self::types_are_compatible(&arg_ty, param_ty) {
                                self.report_error(SemanError::TypeMismatch {
                                    loc: arg_expr.get_source_ref(),
                                    expected: param_ty.as_str(),
                                    found: arg_ty.as_str(),
                                });
                            }
                            typed_args.push(Rc::new(typed_arg));
                        }

                        let typed_call_expr = TypedExpr::CallFn {
                            func: Rc::new(typed_func),
                            args: typed_args,
                            ty: return_ty.clone(),
                            loc: loc.clone(),
                        };
                        (return_ty.clone(), typed_call_expr)
                    }
                    _ => {
                        self.report_error(SemanError::ExpectedFunctionType {
                            found: func_ty.as_str(),
                            loc: func.get_source_ref(),
                        });
                        (
                            Rc::new(Ty::ErrorType { loc: loc.clone() }),
                            TypedExpr::Error,
                        )
                    }
                }
            }
            Expr::GroupedExpr { inner, loc } => todo!(),
            Expr::IndexInto { target, index, loc } => todo!(),
            Expr::MakeSlice {
                target,
                start,
                end,
                loc,
            } => todo!(),
            Expr::AccessMember { target, mem, loc } => todo!(),
            Expr::OptionalExpr { val, loc } => todo!(),
            Expr::ComptimeExpr { val, loc } => todo!(),
            Expr::Lambda {
                params,
                ret_type,
                body,
                loc,
            } => todo!(),
            Expr::DerefPtr { target, loc } => todo!(),
            Expr::MakePtrFromAddrOf { target, loc } => todo!(),
            Expr::InitializerList { target, pairs, loc } => todo!(),
            Expr::ErrorExpr { loc } => {
                unreachable!("ErrorExpr found in program [validate_expression()]")
            }
        }
    }

    fn validate_instruction(&mut self, ins: &Ins) -> TypedIns {
        match ins {
            Ins::DeclVariable {
                name,
                ty,
                init_val,
                is_mutable,
                loc,
                is_public,
            } => {
                if !is_mutable {
                    // constant variable
                    if let Some(init_expr) = init_val {
                        let (init_ty, typed_init_value) = if let Some(var_ty) = ty {
                            self.validate_expression(init_expr, Some(var_ty))
                        } else {
                            self.validate_expression(init_expr, None)
                        };

                        match typed_init_value {
                            TypedExpr::Error => TypedIns::Error,
                            _ => {
                                // add to current scope if we are in a local scope
                                if self.current_scope_idx != 0 {
                                    if let Expr::Identifier { name: var_name, .. } = name {
                                        if let Err(_) = self.scopes[self.current_scope_idx]
                                            .add_symbol(
                                                var_name.to_string(),
                                                SymInfo::Variable {
                                                    ty: init_ty.clone(),
                                                    is_mutable: false,
                                                    def_loc: loc.clone(),
                                                    ref_count: Cell::new(0),
                                                },
                                            )
                                        {
                                            self.report_error(SemanError::NameAlreadyDefined {
                                                loc: loc.clone(),
                                                name: var_name.to_string(),
                                            });
                                        }
                                    }
                                }

                                TypedIns::DeclVariable {
                                    name: name.as_str(),
                                    ty: init_ty.clone(),
                                    init_value: typed_init_value,
                                    is_mutable: false,
                                    loc: loc.clone(),
                                }
                            }
                        }
                    } else {
                        unreachable!("Constant variable declared without init value [validate_instruction()]")
                    }
                } else {
                    // mutable variable
                    match (ty, init_val) {
                        (Some(var_ty), Some(init_expr)) => {
                            // has both type and initializer
                            let (init_ty, typed_init) =
                                self.validate_expression(init_expr, Some(var_ty));
                            if matches!(typed_init, TypedExpr::Error) {
                                TypedIns::Error
                            } else {
                                // add to current scope if we are in a local scope
                                if self.current_scope_idx != 0 {
                                    if let Expr::Identifier { name: var_name, .. } = name {
                                        if let Err(_) = self.scopes[self.current_scope_idx]
                                            .add_symbol(
                                                var_name.to_string(),
                                                SymInfo::Variable {
                                                    ty: init_ty.clone(),
                                                    is_mutable: true,
                                                    def_loc: loc.clone(),
                                                    ref_count: Cell::new(0),
                                                },
                                            )
                                        {
                                            self.report_error(SemanError::NameAlreadyDefined {
                                                loc: loc.clone(),
                                                name: var_name.to_string(),
                                            });
                                        }
                                    }
                                }

                                TypedIns::DeclVariable {
                                    name: name.as_str(),
                                    ty: var_ty.clone(),
                                    init_value: typed_init,
                                    is_mutable: true,
                                    loc: loc.clone(),
                                }
                            }
                        }
                        (Some(var_ty), None) => {
                            // has type but no initializer - use default
                            let default_value = self.get_default_value(var_ty, loc);
                            // add to current scope if we are in a local scope
                            if self.current_scope_idx != 0 {
                                if let Expr::Identifier { name: var_name, .. } = name {
                                    if let Err(_) = self.scopes[self.current_scope_idx].add_symbol(
                                        var_name.to_string(),
                                        SymInfo::Variable {
                                            ty: var_ty.clone(),
                                            is_mutable: true,
                                            def_loc: loc.clone(),
                                            ref_count: Cell::new(0),
                                        },
                                    ) {
                                        self.report_error(SemanError::NameAlreadyDefined {
                                            loc: loc.clone(),
                                            name: var_name.to_string(),
                                        });
                                    }
                                }
                            }

                            TypedIns::DeclVariable {
                                name: name.as_str(),
                                ty: var_ty.clone(),
                                init_value: default_value,
                                is_mutable: true,
                                loc: loc.clone(),
                            }
                        }
                        (None, Some(init_expr)) => {
                            // type inference from initializer
                            let (init_ty, typed_init) = self.validate_expression(init_expr, None);
                            if matches!(typed_init, TypedExpr::Error) {
                                TypedIns::Error
                            } else {
                                // add to current scope if we are in a local scope
                                if self.current_scope_idx != 0 {
                                    if let Expr::Identifier { name: var_name, .. } = name {
                                        if let Err(_) = self.scopes[self.current_scope_idx]
                                            .add_symbol(
                                                var_name.to_string(),
                                                SymInfo::Variable {
                                                    ty: init_ty.clone(),
                                                    is_mutable: true,
                                                    def_loc: loc.clone(),
                                                    ref_count: Cell::new(0),
                                                },
                                            )
                                        {
                                            self.report_error(SemanError::NameAlreadyDefined {
                                                loc: loc.clone(),
                                                name: var_name.to_string(),
                                            });
                                        }
                                    }
                                }

                                TypedIns::DeclVariable {
                                    name: name.as_str(),
                                    ty: init_ty,
                                    init_value: typed_init,
                                    is_mutable: true,
                                    loc: loc.clone(),
                                }
                            }
                        }
                        (None, None) => {
                            unreachable!("Mutable variable declared without init value and explicit type [validate_instruction()]")
                        }
                    }
                }
            }
            Ins::DeclFunc {
                name,
                params,
                ret_ty,
                body,
                loc,
                is_public,
            } => {
                if self.current_scope_idx == 0 {
                    // global function - already collected relevant information
                } else {
                    // local function
                    if let Expr::Identifier {
                        name: func_name, ..
                    } = name
                    {
                        self.scopes[self.current_scope_idx].add_symbol(
                            func_name.to_string(),
                            SymInfo::Fn {
                                params: params.clone(),
                                return_type: ret_ty.clone(),
                                is_global: false,
                                def_loc: loc.clone(),
                                ref_count: Cell::new(0),
                            },
                        );
                    }
                }

                self.enter_scope();
                for param in params {
                    if let Expr::Identifier {
                        name: param_name, ..
                    } = &param.name
                    {
                        if let Err(_) = self.scopes[self.current_scope_idx].add_symbol(
                            param_name.to_string(),
                            SymInfo::Variable {
                                ty: param.given_ty.clone(),
                                is_mutable: param.is_mutable,
                                def_loc: param.loc.clone(),
                                ref_count: Cell::new(0),
                            },
                        ) {
                            self.report_error(SemanError::NameAlreadyDefined {
                                loc: param.loc.clone(),
                                name: param_name.to_string(),
                            });
                        }
                    }
                }
                let typed_body = self.validate_instruction(body);
                self.exit_scope();

                TypedIns::DeclFunc {
                    name: name.as_str(),
                    params: params.clone(),
                    ret_ty: ret_ty.clone(),
                    body: Rc::new(typed_body),
                    loc: loc.clone(),
                }
            }
            Ins::DeclTypeAlias {
                name,
                ty,
                loc,
                is_public,
            } => todo!(),
            Ins::Defer { sub_ins, loc } => todo!(),
            Ins::Block { code, loc } => {
                self.enter_scope();
                let mut typed_code = vec![];
                for ins in code {
                    let typed_ins = self.validate_instruction(ins);
                    typed_code.push(typed_ins);
                }
                self.exit_scope();
                TypedIns::Block {
                    code: typed_code,
                    loc: loc.clone(),
                }
            }
            Ins::AssignTo { target, value, loc } => todo!(),
            Ins::ExprIns { expr, loc } => todo!(),
            Ins::IfConditional {
                conds_and_code,
                loc,
            } => todo!(),
            Ins::Return { expr, loc } => todo!(),
            Ins::SingleLineComment { content, loc } => todo!(),
            Ins::PrintIns {
                is_println,
                output,
                loc,
            } => todo!(),
            Ins::Break { loc } => todo!(),
            Ins::Continue { loc } => todo!(),
            Ins::ForInLoop {
                loop_var,
                loop_target,
                block,
                loc,
            } => todo!(),
            Ins::InfiniteLoop { block, loc } => todo!(),
            Ins::WhileLoop {
                cond,
                post_code,
                block,
                loc,
            } => todo!(),
            Ins::RegLoop {
                init,
                loop_cond,
                update,
                block,
                loc,
            } => todo!(),
            Ins::ErrorIns { loc } => {
                unreachable!("ErrorIns found in program [validate_instruction()]")
            }
        }
    }

    pub fn analyze_program(
        program: &[Ins],
        main_src_file: SourceFile,
    ) -> Result<Vec<TypedIns>, Vec<SemanError>> {
        let mut analyzer = Self::new(main_src_file);

        // pass 1: collect all top level declarations
        analyzer.collect_declarations(program);
        if !analyzer.errors.is_empty() {
            return Err(analyzer.errors);
        }
        for (name, name_info) in analyzer.scopes[0].symbols.iter() {
            println!("{name}: {}", name_info.as_str())
        }

        // pass 2: validate everything in the program
        let typed_program = analyzer.validate_program(program);
        if !analyzer.errors.is_empty() {
            return Err(analyzer.errors);
        }

        Ok(typed_program)
    }
}
