#![allow(unused)]

use std::{cell::Cell, collections::HashMap, rc::Rc};

use crate::{
    parser::{
        ast::{BinOpType, Expr, FnParam, Ins, UnaryOpType},
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
                if *is_mutable { "var" } else { "const" },
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
    current_fn_ret_ty: Option<Rc<Ty>>,
}

impl SemanticAnalyzer {
    fn new(main_src_file: SourceFile) -> Self {
        let mut scopes = vec![Scope::new_root()];
        Self {
            scopes,
            current_scope_idx: 0,
            errors: vec![],
            main_src_file,
            current_fn_ret_ty: None,
        }
    }

    fn lookup(&self, name: &str) -> Option<&SymInfo> {
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

    fn lookup_function(&self, name: &str) -> Option<(Vec<FnParam>, Rc<Ty>)> {
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

    fn lookup_variable(&self, name: &str) -> Option<(Rc<Ty>, bool)> {
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

    fn get_name_definition_loc(&self, name: &str) -> Option<Rc<SourceRef>> {
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

    fn type_check(lhs: &Rc<Ty>, rhs: &Rc<Ty>, loc: Rc<SourceRef>) -> Rc<Ty> {
        match (lhs.as_ref(), rhs.as_ref()) {
            // case 1: types are equal
            (Ty::SignedInt { size: l_size, .. }, Ty::SignedInt { size: r_size, .. })
                if l_size == r_size =>
            {
                lhs.clone_loc(loc)
            }
            (Ty::UntypedInt { .. }, Ty::UntypedInt { .. }) => lhs.clone_loc(loc),
            (Ty::UnsignedInt { size: l_size, .. }, Ty::UnsignedInt { size: r_size, .. })
                if l_size == r_size =>
            {
                lhs.clone_loc(loc)
            }
            (Ty::Float { size: l_size, .. }, Ty::Float { size: r_size, .. })
                if l_size == r_size =>
            {
                lhs.clone_loc(loc)
            }
            (Ty::Char { .. }, Ty::Char { .. }) => lhs.clone_loc(loc),
            (Ty::Str { .. }, Ty::Str { .. }) => lhs.clone_loc(loc),
            (Ty::Void { .. }, Ty::Void { .. }) => lhs.clone_loc(loc),

            // case 2: safe implicit cast
            (Ty::UntypedInt { literal, .. }, Ty::SignedInt { size, .. }) => {
                let max_value = match size {
                    8 => i8::MAX as usize,
                    16 => i16::MAX as usize,
                    32 => i32::MAX as usize,
                    64 => i64::MAX as usize,
                    _ => unreachable!("Unexpected signed integer size [type_check()]"),
                };
                if *literal <= max_value {
                    rhs.clone_loc(loc)
                } else {
                    Rc::new(Ty::ErrorType { loc })
                }
            }
            (Ty::SignedInt { size, .. }, Ty::UntypedInt { literal, .. }) => {
                let max_value = match size {
                    8 => i8::MAX as usize,
                    16 => i16::MAX as usize,
                    32 => i32::MAX as usize,
                    64 => i64::MAX as usize,
                    _ => unreachable!("Unexpected signed integer size [type_check()]"),
                };
                if *literal <= max_value {
                    lhs.clone_loc(loc)
                } else {
                    Rc::new(Ty::ErrorType { loc })
                }
            }
            (Ty::UntypedInt { literal, .. }, Ty::UnsignedInt { size, .. }) => {
                let max_value = match size {
                    8 => u8::MAX as usize,
                    16 => u16::MAX as usize,
                    32 => u32::MAX as usize,
                    64 => u64::MAX as usize,
                    _ => unreachable!("Unexpected unsigned integer size [type_check()]"),
                };
                if *literal <= max_value {
                    rhs.clone_loc(loc)
                } else {
                    Rc::new(Ty::ErrorType { loc })
                }
            }
            (Ty::UnsignedInt { size, .. }, Ty::UntypedInt { literal, .. }) => {
                let max_value = match size {
                    8 => u8::MAX as usize,
                    16 => u16::MAX as usize,
                    32 => u32::MAX as usize,
                    64 => u64::MAX as usize,
                    _ => unreachable!("Unexpected unsigned integer size [type_check()]"),
                };
                if *literal <= max_value {
                    lhs.clone_loc(loc)
                } else {
                    Rc::new(Ty::ErrorType { loc })
                }
            }

            // case 3: type mismatch
            _ => Rc::new(Ty::ErrorType { loc }),
        }
    }

    fn is_untyped_integer_type(&self, ty: &Ty) -> bool {
        matches!(ty, Ty::UntypedInt { .. })
    }

    fn get_default_value(&self, ty: &Rc<Ty>, loc: &Rc<SourceRef>) -> TypedExpr {
        match ty.as_ref() {
            Ty::UntypedInt { literal, .. } => unreachable!("Generating a default value for an untyped int should not happen [get_default_value()]"),
            Ty::SignedInt { size, .. } => {
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
            Ty::UnsignedInt { size, .. } => {
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
            Ty::NamedType { name, loc } => todo!("NamedType {{ {name} }}"),
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

                if !matches!(return_type.as_ref(), Ty::SignedInt { size: 32, .. }) {
                    additional_errors.push(SemanError::TypeMismatch {
                        loc: return_type.get_loc(),
                        expected: "i32".into(),
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

    fn check_binary_arithmetic_overflow(
        left: usize,
        right: usize,
        op: BinOpType,
        loc: &Rc<SourceRef>,
    ) -> Result<usize, SemanError> {
        match op {
            BinOpType::Add => left
                .checked_add(right)
                .ok_or(SemanError::ArithmeticOverflow {
                    loc: loc.clone(),
                    operation: format!("{left} + {right}"),
                    reason: "addition will overflow".into(),
                }),
            BinOpType::Mult => left
                .checked_mul(right)
                .ok_or(SemanError::ArithmeticOverflow {
                    loc: loc.clone(),
                    operation: format!("{left} * {right}",),
                    reason: "multiplication would overflow".into(),
                }),
            BinOpType::Sub => {
                if right > left {
                    // for subtraction that will result in a negative number,
                    // it will be handled separately as it might be valid with a
                    // signed type
                    return Ok(0);
                }
                left.checked_sub(right)
                    .ok_or(SemanError::ArithmeticOverflow {
                        loc: loc.clone(),
                        operation: format!("{left} - {right}"),
                        reason: "subtraction would overflow".into(),
                    })
            }
            BinOpType::Div => {
                if right == 0 {
                    return Err(SemanError::DivisionByZero { loc: loc.clone() });
                }
                left.checked_div(right)
                    .ok_or(SemanError::ArithmeticOverflow {
                        loc: loc.clone(),
                        operation: format!("{left} / {right}"),
                        reason: "division would overflow".into(),
                    })
            }
            BinOpType::Mod => {
                if right == 0 {
                    return Err(SemanError::DivisionByZero { loc: loc.clone() });
                }
                left.checked_rem(right)
                    .ok_or(SemanError::ArithmeticOverflow {
                        loc: loc.clone(),
                        operation: format!("{left} % {right}"),
                        reason: "modulo would overflow".into(),
                    })
            }
            _ => unreachable!(
                "Unexpected binary operator for arithmetic '{}'.",
                op.as_str()
            ),
        }
    }

    fn validate_expression(&mut self, expr: &Expr, parent_ty: Option<&Rc<Ty>>) -> TypedExpr {
        match expr {
            Expr::Integer { content, loc } => {
                let typed_integer = if let Some(parent_ty) = parent_ty {
                    match parent_ty.as_ref() {
                        Ty::SignedInt { size, .. } => match size {
                            8 => {
                                let res = content.parse::<i8>();
                                match res {
                                    Ok(val) => TypedExpr::SignedInt {
                                        value: SignedInteger::I8(val),
                                        ty: parent_ty.clone(),
                                        loc: loc.clone(),
                                    },
                                    Err(_) => {
                                        self.report_error(SemanError::IntegerTypeCheckFailed {
                                            loc: loc.clone(),
                                            number: content.to_string(),
                                            given_type: parent_ty.as_str(),
                                        });
                                        TypedExpr::Error { loc: loc.clone() }
                                    }
                                }
                            }
                            16 => {
                                let res = content.parse::<i16>();
                                match res {
                                    Ok(val) => TypedExpr::SignedInt {
                                        value: SignedInteger::I16(val),
                                        ty: parent_ty.clone(),
                                        loc: loc.clone(),
                                    },
                                    Err(_) => {
                                        self.report_error(SemanError::IntegerTypeCheckFailed {
                                            loc: loc.clone(),
                                            number: content.to_string(),
                                            given_type: parent_ty.as_str(),
                                        });
                                        TypedExpr::Error { loc: loc.clone() }
                                    }
                                }
                            }
                            32 => {
                                let res = content.parse::<i32>();
                                match res {
                                    Ok(val) => TypedExpr::SignedInt {
                                        value: SignedInteger::I32(val),
                                        ty: parent_ty.clone(),
                                        loc: loc.clone(),
                                    },
                                    Err(_) => {
                                        self.report_error(SemanError::IntegerTypeCheckFailed {
                                            loc: loc.clone(),
                                            number: content.to_string(),
                                            given_type: parent_ty.as_str(),
                                        });
                                        TypedExpr::Error { loc: loc.clone() }
                                    }
                                }
                            }
                            64 => {
                                let res = content.parse::<i64>();
                                match res {
                                    Ok(val) => TypedExpr::SignedInt {
                                        value: SignedInteger::I64(val),
                                        ty: parent_ty.clone(),
                                        loc: loc.clone(),
                                    },
                                    Err(_) => {
                                        self.report_error(SemanError::IntegerTypeCheckFailed {
                                            loc: loc.clone(),
                                            number: content.to_string(),
                                            given_type: parent_ty.as_str(),
                                        });
                                        TypedExpr::Error { loc: loc.clone() }
                                    }
                                }
                            }
                            _ => unreachable!(
                                "Unexpected signed integer size [validate_expression()]"
                            ),
                        },
                        Ty::UnsignedInt { size, .. } => match size {
                            8 => {
                                let res = content.parse::<u8>();
                                match res {
                                    Ok(val) => TypedExpr::UnsignedInt {
                                        value: UnsignedInteger::U8(val),
                                        ty: parent_ty.clone(),
                                        loc: loc.clone(),
                                    },
                                    Err(_) => {
                                        self.report_error(SemanError::IntegerTypeCheckFailed {
                                            loc: loc.clone(),
                                            number: content.to_string(),
                                            given_type: parent_ty.as_str(),
                                        });
                                        TypedExpr::Error { loc: loc.clone() }
                                    }
                                }
                            }
                            16 => {
                                let res = content.parse::<u16>();
                                match res {
                                    Ok(val) => TypedExpr::UnsignedInt {
                                        value: UnsignedInteger::U16(val),
                                        ty: parent_ty.clone(),
                                        loc: loc.clone(),
                                    },
                                    Err(_) => {
                                        self.report_error(SemanError::IntegerTypeCheckFailed {
                                            loc: loc.clone(),
                                            number: content.to_string(),
                                            given_type: parent_ty.as_str(),
                                        });
                                        TypedExpr::Error { loc: loc.clone() }
                                    }
                                }
                            }
                            32 => {
                                let res = content.parse::<u32>();
                                match res {
                                    Ok(val) => TypedExpr::UnsignedInt {
                                        value: UnsignedInteger::U32(val),
                                        ty: parent_ty.clone(),
                                        loc: loc.clone(),
                                    },
                                    Err(_) => {
                                        self.report_error(SemanError::IntegerTypeCheckFailed {
                                            loc: loc.clone(),
                                            number: content.to_string(),
                                            given_type: parent_ty.as_str(),
                                        });
                                        TypedExpr::Error { loc: loc.clone() }
                                    }
                                }
                            }
                            64 => {
                                let res = content.parse::<u64>();
                                match res {
                                    Ok(val) => TypedExpr::UnsignedInt {
                                        value: UnsignedInteger::U64(val),
                                        ty: parent_ty.clone(),
                                        loc: loc.clone(),
                                    },
                                    Err(_) => {
                                        self.report_error(SemanError::IntegerTypeCheckFailed {
                                            loc: loc.clone(),
                                            number: content.to_string(),
                                            given_type: parent_ty.as_str(),
                                        });
                                        TypedExpr::Error { loc: loc.clone() }
                                    }
                                }
                            }
                            _ => unreachable!(
                                "Unexpected signed integer size [validate_expression()]"
                            ),
                        },
                        Ty::Float { size, .. } => match size {
                            32 => {
                                let res = content.parse::<f32>();
                                match res {
                                    Ok(float) => TypedExpr::Float {
                                        value: float as f64,
                                        ty: parent_ty.clone(),
                                        loc: loc.clone(),
                                    },
                                    Err(_) => {
                                        self.report_error(SemanError::FloatTypeCheckFailed {
                                            loc: loc.clone(),
                                            number: content.to_string(),
                                            given_type: parent_ty.as_str(),
                                        });
                                        TypedExpr::Error { loc: loc.clone() }
                                    }
                                }
                            }
                            64 => {
                                let res = content.parse::<f64>();
                                match res {
                                    Ok(float) => TypedExpr::Float {
                                        value: float,
                                        ty: parent_ty.clone(),
                                        loc: loc.clone(),
                                    },
                                    Err(_) => {
                                        self.report_error(SemanError::FloatTypeCheckFailed {
                                            loc: loc.clone(),
                                            number: content.to_string(),
                                            given_type: parent_ty.as_str(),
                                        });
                                        TypedExpr::Error { loc: loc.clone() }
                                    }
                                }
                            }
                            _ => unreachable!("Unexpected float size [validate_expression()]"),
                        },
                        _ => {
                            self.report_error(SemanError::TypeMismatch {
                                loc: loc.clone(),
                                expected: "a numerical type (i8..i64, u8..u64, f32 | f64).".into(),
                                found: parent_ty.as_str(),
                            });
                            TypedExpr::Error { loc: loc.clone() }
                        }
                    }
                } else {
                    let res = content.parse::<usize>();
                    match res {
                        Ok(val) => {
                            let untyped_int_ty = Rc::new(Ty::UntypedInt {
                                literal: val,
                                loc: loc.clone(),
                            });
                            TypedExpr::UntypedInt {
                                value: val,
                                ty: untyped_int_ty,
                                loc: loc.clone(),
                            }
                        }
                        Err(_) => {
                            self.report_error(SemanError::IntegerTypeDefaultInferenceFailed {
                                loc: loc.clone(),
                                number: content.to_string(),
                            });
                            TypedExpr::Error { loc: loc.clone() }
                        }
                    }
                };
                typed_integer
            }
            Expr::Float { content, loc } => {
                let typed_float = if let Some(parent_ty) = parent_ty {
                    match parent_ty.as_ref() {
                        Ty::Float { size, .. } => match size {
                            32 => {
                                let res = content.parse::<f32>();
                                match res {
                                    Ok(float) => TypedExpr::Float {
                                        value: float as f64,
                                        ty: parent_ty.clone(),
                                        loc: loc.clone(),
                                    },
                                    Err(_) => {
                                        self.report_error(SemanError::FloatTypeCheckFailed {
                                            loc: loc.clone(),
                                            number: content.to_string(),
                                            given_type: parent_ty.as_str(),
                                        });
                                        TypedExpr::Error { loc: loc.clone() }
                                    }
                                }
                            }
                            64 => {
                                let res = content.parse::<f64>();
                                match res {
                                    Ok(float) => TypedExpr::Float {
                                        value: float,
                                        ty: parent_ty.clone(),
                                        loc: loc.clone(),
                                    },
                                    Err(_) => {
                                        self.report_error(SemanError::FloatTypeCheckFailed {
                                            loc: loc.clone(),
                                            number: content.to_string(),
                                            given_type: parent_ty.as_str(),
                                        });
                                        TypedExpr::Error { loc: loc.clone() }
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
                            TypedExpr::Error { loc: loc.clone() }
                        }
                    }
                } else {
                    let res = content.parse::<f32>();
                    let f32_type = Rc::new(Ty::Float {
                        size: 32,
                        loc: loc.clone(),
                    });
                    match res {
                        Ok(val) => TypedExpr::Float {
                            value: val as f64,
                            ty: f32_type,
                            loc: loc.clone(),
                        },
                        Err(_) => {
                            self.report_error(SemanError::FloatTypeDefaultInferenceFailed {
                                loc: loc.clone(),
                                number: content.to_string(),
                            });
                            TypedExpr::Error { loc: loc.clone() }
                        }
                    }
                };
                typed_float
            }
            Expr::Str { content, loc } => {
                let str_ty = Rc::new(Ty::Str { loc: loc.clone() });
                TypedExpr::Str {
                    value: content.to_string(),
                    ty: str_ty,
                    loc: loc.clone(),
                }
            }
            Expr::Char { content, loc } => {
                let char_ty = Rc::new(Ty::Char { loc: loc.clone() });
                TypedExpr::Char {
                    value: content.chars().next().unwrap_or('\0'),
                    ty: char_ty,
                    loc: loc.clone(),
                }
            }
            Expr::Bool { val, loc } => {
                let bool_ty = Rc::new(Ty::Bool { loc: loc.clone() });
                TypedExpr::Bool {
                    value: *val,
                    ty: bool_ty,
                    loc: loc.clone(),
                }
            }
            Expr::Tuple { items, loc } => todo!(),
            Expr::StaticArray { ty, items, loc } => todo!(),
            Expr::Identifier { name, loc } => {
                // look up the identifier and update the reference number of the name
                match self.lookup(name) {
                    Some(SymInfo::Variable { ty, ref_count, .. }) => {
                        ref_count.set(ref_count.get() + 1);
                        TypedExpr::Identifier {
                            name: name.to_string(),
                            ty: ty.clone(),
                            loc: loc.clone(),
                        }
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
                        TypedExpr::Identifier {
                            name: name.to_string(),
                            ty: fn_ty,
                            loc: loc.clone(),
                        }
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
                        TypedExpr::Error { loc: loc.clone() }
                    }
                }
            }
            Expr::UnaryOp { op, expr, loc } => {
                let typed_expr = self.validate_expression(expr, parent_ty);
                let expr_ty = typed_expr.get_ty();

                match op {
                    UnaryOpType::Not => {
                        if expr_ty.is_bool_ty() {
                            TypedExpr::UnaryOp {
                                op: *op,
                                expr: Rc::new(typed_expr),
                                ty: expr_ty.clone_loc(loc.clone()),
                                loc: loc.clone(),
                            }
                        } else {
                            self.report_error(SemanError::InvalidUseOfUnaryOperator {
                                loc: loc.clone(),
                                op: op.as_str(),
                                operand_ty: expr_ty.as_str(),
                                tip: Some(
                                    "The logical not (!) operator works on operands of the 'bool' type."
                                        .to_string(),
                                ),
                            });
                            TypedExpr::Error { loc: loc.clone() }
                        }
                    }
                    UnaryOpType::Negate => match expr_ty.as_ref() {
                        Ty::SignedInt { .. } | Ty::Float { .. } => {
                            // signed integers can be negated directly
                            TypedExpr::UnaryOp {
                                op: *op,
                                expr: Rc::new(typed_expr),
                                ty: expr_ty.clone_loc(loc.clone()),
                                loc: loc.clone(),
                            }
                        }
                        Ty::UntypedInt { literal, .. } => {
                            if let Some(parent_ty) = parent_ty {
                                // if parent type is provided, try to use it
                                match parent_ty.as_ref() {
                                    Ty::SignedInt { size, .. } => {
                                        // get min/max range for the target signed type
                                        let (min, max) = match size {
                                            8 => (i8::MIN as isize, i8::MAX as isize),
                                            16 => (i16::MIN as isize, i16::MAX as isize),
                                            32 => (i32::MIN as isize, i32::MAX as isize),
                                            64 => (i64::MIN as isize, i64::MAX as isize),
                                            _ => unreachable!("Unexpected signed integer size."),
                                        };

                                        // check if the negated value is within range
                                        let negated = -(*literal as isize);
                                        if negated >= min && negated <= max {
                                            TypedExpr::UnaryOp {
                                                op: *op,
                                                expr: Rc::new(typed_expr),
                                                ty: parent_ty.clone_loc(loc.clone()),
                                                loc: loc.clone(),
                                            }
                                        } else {
                                            self.report_error(SemanError::IntegerTypeCheckFailed {
                                                loc: loc.clone(),
                                                number: format!("-{literal}"),
                                                given_type: parent_ty.as_str(),
                                            });
                                            TypedExpr::Error { loc: loc.clone() }
                                        }
                                    }
                                    _ => {
                                        self.report_error(SemanError::TypeMismatch {
                                            loc: loc.clone(),
                                            expected: "a signed number type (i8..i64)".into(),
                                            found: parent_ty.as_str(),
                                        });
                                        TypedExpr::Error { loc: loc.clone() }
                                    }
                                }
                            } else {
                                // no parent type, default to platform max size (i32 or i64)
                                let (min, max) = match Ty::get_platform_size() {
                                    32 => (i32::MIN as isize, i32::MAX as isize),
                                    64 => (i64::MIN as isize, i64::MAX as isize),
                                    _ => {
                                        unreachable!("Unexpected platform size (not 32 or 64 bit).")
                                    }
                                };

                                let negated = -(*literal as isize);
                                if negated >= min && negated <= max {
                                    TypedExpr::UnaryOp {
                                        op: *op,
                                        expr: Rc::new(typed_expr),
                                        ty: Ty::get_int_ty(loc.clone()),
                                        loc: loc.clone(),
                                    }
                                } else {
                                    self.report_error(SemanError::IntegerTypeCheckFailed {
                                        loc: loc.clone(),
                                        number: format!("-{literal}"),
                                        given_type: format!("i{}", Ty::get_platform_size()),
                                    });
                                    TypedExpr::Error { loc: loc.clone() }
                                }
                            }
                        }
                        _ => {
                            self.report_error(SemanError::InvalidUseOfUnaryOperator {
                                loc: loc.clone(),
                                op: op.as_str(),
                                operand_ty: expr_ty.as_str(),
                                tip: None,
                            });
                            TypedExpr::Error { loc: loc.clone() }
                        }
                    },
                }
            }
            Expr::BinOp {
                op,
                left,
                right,
                loc,
            } => {
                let typed_left = self.validate_expression(left, parent_ty);
                let typed_right = self.validate_expression(right, parent_ty);
                let (left_ty, right_ty) = (typed_left.get_ty(), typed_right.get_ty());

                match op {
                    BinOpType::Add
                    | BinOpType::Sub
                    | BinOpType::Mult
                    | BinOpType::Div
                    | BinOpType::Mod => match (left_ty.as_ref(), right_ty.as_ref()) {
                        (Ty::SignedInt { .. }, Ty::SignedInt { .. })
                        | (Ty::UnsignedInt { .. }, Ty::UnsignedInt { .. })
                        | (Ty::Float { .. }, Ty::Float { .. }) => {
                            let validate_ty = Self::type_check(&left_ty, &right_ty, loc.clone());
                            if validate_ty.is_error_ty() {
                                self.report_error(SemanError::TypeMismatch {
                                    loc: loc.clone(),
                                    expected: left_ty.as_str(),
                                    found: right_ty.as_str(),
                                });
                                return TypedExpr::Error { loc: loc.clone() };
                            }

                            TypedExpr::BinOp {
                                op: *op,
                                left: Rc::new(typed_left),
                                right: Rc::new(typed_right),
                                ty: validate_ty,
                                loc: loc.clone(),
                            }
                        }
                        (
                            Ty::UntypedInt { literal, .. },
                            Ty::UntypedInt {
                                literal: r_literal, ..
                            },
                        ) => {
                            let result = match Self::check_binary_arithmetic_overflow(
                                *literal, *r_literal, *op, loc,
                            ) {
                                Ok(0) if matches!(op, BinOpType::Sub) => {
                                    // special case: subtraction
                                    if r_literal > literal {
                                        // would result in a negative number so we see if it can
                                        // be coerced into a contextual parent_ty
                                        if let Some(parent_ty) = parent_ty {
                                            match parent_ty.as_ref() {
                                                Ty::SignedInt { size, .. } => {
                                                    let diff = r_literal - literal;
                                                    let max = match size {
                                                        8 => i8::MAX as usize,
                                                        16 => i16::MAX as usize,
                                                        32 => i32::MAX as usize,
                                                        64 => i64::MAX as usize,
                                                        _ => {
                                                            unreachable!("Unexpected integer size.")
                                                        }
                                                    };

                                                    if diff <= max {
                                                        return TypedExpr::BinOp {
                                                            op: *op,
                                                            left: Rc::new(typed_left),
                                                            right: Rc::new(typed_right),
                                                            ty: parent_ty.clone_loc(loc.clone()),
                                                            loc: loc.clone(),
                                                        };
                                                    } else {
                                                        self.report_error(
                                                            SemanError::IntegerTypeCheckFailed {
                                                                loc: loc.clone(),
                                                                number: format!(
                                                                    "{} - {}",
                                                                    literal, r_literal
                                                                ),
                                                                given_type: parent_ty.as_str(),
                                                            },
                                                        );
                                                        return TypedExpr::Error {
                                                            loc: loc.clone(),
                                                        };
                                                    }
                                                }
                                                _ => {
                                                    self.report_error(
                                                        SemanError::IntegerTypeCheckFailed {
                                                            loc: loc.clone(),
                                                            number: format!(
                                                                "{} - {}",
                                                                literal, r_literal
                                                            ),
                                                            given_type: parent_ty.as_str(),
                                                        },
                                                    );
                                                    return TypedExpr::Error { loc: loc.clone() };
                                                }
                                            }
                                        } else {
                                            // no parent type and result would be negative
                                            // so we will coerce it into a signed integer
                                            // type of the current platform size
                                            let platform_signed_ty = Rc::new(Ty::SignedInt {
                                                size: Ty::get_platform_size(),
                                                loc: loc.clone(),
                                            });

                                            return TypedExpr::BinOp {
                                                op: *op,
                                                left: Rc::new(typed_left),
                                                right: Rc::new(typed_right),
                                                ty: platform_signed_ty,
                                                loc: loc.clone(),
                                            };
                                        }
                                    } else {
                                        // resilt is really 0
                                        0
                                    }
                                }
                                Ok(result) => result,
                                Err(err) => {
                                    self.report_error(err);
                                    return TypedExpr::Error { loc: loc.clone() };
                                }
                            };

                            // if parent type is provided, try to use it
                            if let Some(parent_ty) = parent_ty {
                                match parent_ty.as_ref() {
                                    Ty::SignedInt { size, .. } => {
                                        let max = match size {
                                            8 => i8::MAX as usize,
                                            16 => i16::MAX as usize,
                                            32 => i32::MAX as usize,
                                            64 => i64::MAX as usize,
                                            _ => unreachable!("Unexpected integer size."),
                                        };

                                        if result <= max {
                                            return TypedExpr::BinOp {
                                                op: *op,
                                                left: Rc::new(typed_left),
                                                right: Rc::new(typed_right),
                                                ty: parent_ty.clone_loc(loc.clone()),
                                                loc: loc.clone(),
                                            };
                                        }
                                    }
                                    Ty::UnsignedInt { size, .. } => {
                                        let max = match size {
                                            8 => u8::MAX as usize,
                                            16 => u16::MAX as usize,
                                            32 => u32::MAX as usize,
                                            64 => u64::MAX as usize,
                                            _ => unreachable!("Unexpected integer size."),
                                        };

                                        if result <= max {
                                            return TypedExpr::BinOp {
                                                op: *op,
                                                left: Rc::new(typed_left),
                                                right: Rc::new(typed_right),
                                                ty: parent_ty.clone_loc(loc.clone()),
                                                loc: loc.clone(),
                                            };
                                        }
                                    }
                                    _ => {}
                                }
                            }

                            // no parent_ty or it doesn't fit properly into the parent type,
                            // so keep it as an untyped int
                            let result_ty = Rc::new(Ty::UntypedInt {
                                literal: result,
                                loc: loc.clone(),
                            });

                            TypedExpr::BinOp {
                                op: *op,
                                left: Rc::new(typed_left),
                                right: Rc::new(typed_right),
                                ty: result_ty,
                                loc: loc.clone(),
                            }
                        }
                        (Ty::UntypedInt { .. }, Ty::SignedInt { .. })
                        | (Ty::UntypedInt { .. }, Ty::UnsignedInt { .. })
                        | (Ty::SignedInt { .. }, Ty::UntypedInt { .. })
                        | (Ty::UnsignedInt { .. }, Ty::UntypedInt { .. }) => {
                            let left_is_untyped = left_ty.is_untyped_int_ty();
                            // convert the untyped int to match concrete type
                            let validated_ty = Self::type_check(&left_ty, &right_ty, loc.clone());
                            if validated_ty.is_error_ty() {
                                self.report_error(SemanError::TypeMismatch {
                                    loc: loc.clone(),
                                    expected: if left_is_untyped {
                                        right_ty.as_str()
                                    } else {
                                        left_ty.as_str()
                                    },
                                    found: if left_is_untyped {
                                        left_ty.as_str()
                                    } else {
                                        right_ty.as_str()
                                    },
                                });
                                return TypedExpr::Error { loc: loc.clone() };
                            }

                            TypedExpr::BinOp {
                                op: *op,
                                left: Rc::new(typed_left),
                                right: Rc::new(typed_right),
                                ty: validated_ty,
                                loc: loc.clone(),
                            }
                        }
                        (Ty::Char { .. }, Ty::Char { .. }) if matches!(op, BinOpType::Add) => {
                            let ty = Rc::new(Ty::Str { loc: loc.clone() });
                            TypedExpr::BinOp {
                                op: *op,
                                left: Rc::new(typed_left),
                                right: Rc::new(typed_right),
                                ty,
                                loc: loc.clone(),
                            }
                        }
                        (Ty::Str { .. }, Ty::Str { .. }) if matches!(op, BinOpType::Add) => {
                            let ty = Rc::new(Ty::Str { loc: loc.clone() });
                            TypedExpr::BinOp {
                                op: *op,
                                left: Rc::new(typed_left),
                                right: Rc::new(typed_right),
                                ty,
                                loc: loc.clone(),
                            }
                        }
                        (Ty::Str { .. }, Ty::Char { .. }) if matches!(op, BinOpType::Add) => {
                            let ty = Rc::new(Ty::Str { loc: loc.clone() });
                            TypedExpr::BinOp {
                                op: *op,
                                left: Rc::new(typed_left),
                                right: Rc::new(typed_right),
                                ty,
                                loc: loc.clone(),
                            }
                        }
                        _ => {
                            self.report_error(SemanError::InvalidUseOfBinaryOperator {
                                loc: loc.clone(),
                                op: op.as_str(),
                                left: left_ty.as_str(),
                                right: right_ty.as_str(),
                            });
                            TypedExpr::Error { loc: loc.clone() }
                        }
                    },
                    BinOpType::And | BinOpType::Or => {
                        if left_ty.is_bool_ty() && right_ty.is_bool_ty() {
                            todo!()
                        } else {
                            self.report_error(SemanError::InvalidUseOfBinaryOperator {
                                loc: loc.clone(),
                                op: op.as_str(),
                                left: left_ty.as_str(),
                                right: right_ty.as_str(),
                            });
                            TypedExpr::Error { loc: loc.clone() }
                        }
                    }
                    BinOpType::Eq
                    | BinOpType::Neq
                    | BinOpType::Gt
                    | BinOpType::Lt
                    | BinOpType::GtEq
                    | BinOpType::LtEq => {
                        let validated_ty = Self::type_check(&left_ty, &right_ty, loc.clone());
                        if validated_ty.is_error_ty() {
                            self.report_error(SemanError::TypeMismatch {
                                loc: loc.clone(),
                                expected: left_ty.as_str(),
                                found: right_ty.as_str(),
                            });
                        }

                        let op_check_cond = if matches!(*op, BinOpType::Eq | BinOpType::Neq) {
                            // comparison operations can be performed between:
                            // - numerical values of the same type
                            // - string values
                            // - character values
                            // - boolean values
                            validated_ty.is_num_ty()
                                || matches!(
                                    validated_ty.as_ref(),
                                    Ty::Str { .. } | Ty::Char { .. } | Ty::Bool { .. }
                                )
                        } else {
                            // ordering operations can be performed between:
                            // - numerical values of the same type
                            // - string values
                            // - character values
                            validated_ty.is_num_ty()
                                || matches!(validated_ty.as_ref(), Ty::Str { .. } | Ty::Char { .. })
                        };

                        if !op_check_cond {
                            self.report_error(SemanError::InvalidUseOfBinaryOperator {
                                loc: loc.clone(),
                                op: op.as_str(),
                                left: left_ty.as_str(),
                                right: right_ty.as_str(),
                            });
                            TypedExpr::Error { loc: loc.clone() }
                        } else {
                            TypedExpr::BinOp {
                                op: *op,
                                left: Rc::new(typed_left),
                                right: Rc::new(typed_right),
                                ty: Rc::new(Ty::Bool { loc: loc.clone() }),
                                loc: loc.clone(),
                            }
                        }
                    }
                }
            }
            Expr::ConditionalExpr {
                cond,
                then,
                otherwise,
                loc,
            } => {
                let typed_cond = self.validate_expression(cond, None);
                let cond_ty = typed_cond.get_ty();

                if !cond_ty.is_error_ty() && !cond_ty.is_bool_ty() {
                    self.report_error(SemanError::TypeMismatch {
                        loc: cond.get_source_ref(),
                        expected: "bool".into(),
                        found: cond_ty.as_str(),
                    });

                    // still check the body since there might be errors
                    // with a mismatch of the branch expression types
                }

                let typed_then = self.validate_expression(then, None);
                let then_ty = typed_then.get_ty();
                let typed_otherwise = self.validate_expression(otherwise, None);
                let otherwise_ty = typed_otherwise.get_ty();

                let validated_ty = Self::type_check(&then_ty, &otherwise_ty, loc.clone());

                if validated_ty.is_error_ty() {
                    self.report_error(SemanError::TypeMismatch {
                        loc: loc.clone(),
                        expected: then_ty.as_str(),
                        found: otherwise_ty.as_str(),
                    });

                    return TypedExpr::Error { loc: loc.clone() };
                }

                TypedExpr::ConditionalExpr {
                    cond: Rc::new(typed_cond),
                    then: Rc::new(typed_then),
                    otherwise: Rc::new(typed_otherwise),
                    ty: validated_ty,
                    loc: loc.clone(),
                }
            }
            Expr::CallFn { func, args, loc } => {
                let typed_func = self.validate_expression(func, None);

                match typed_func.get_ty().as_ref() {
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
                            return TypedExpr::Error { loc: loc.clone() };
                        }

                        let mut typed_args = vec![];
                        for (arg_expr, param_ty) in args.iter().zip(param_tys.iter()) {
                            let typed_arg = self.validate_expression(arg_expr, Some(param_ty));

                            let validated_ty =
                                Self::type_check(&param_ty, &typed_arg.get_ty(), loc.clone());
                            if let Ty::ErrorType { .. } = validated_ty.as_ref() {
                                self.report_error(SemanError::TypeMismatch {
                                    loc: arg_expr.get_source_ref(),
                                    expected: param_ty.as_str(),
                                    found: typed_arg.get_ty().as_str(),
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
                        typed_call_expr
                    }
                    _ => {
                        self.report_error(SemanError::ExpectedFunctionType {
                            found: typed_func.get_ty().as_str(),
                            loc: func.get_source_ref(),
                        });
                        TypedExpr::Error { loc: loc.clone() }
                    }
                }
            }
            Expr::GroupedExpr { inner, loc } => {
                let typed_inner = self.validate_expression(inner, parent_ty);
                TypedExpr::GroupedExpr {
                    inner: Rc::new(typed_inner),
                    loc: loc.clone(),
                }
            }
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

    fn validate_target_expression(&mut self, expr: &Expr) -> TypedExpr {
        match expr {
            Expr::Identifier { name, loc } => {
                // Look up the identifier and update the reference number of the name
                match self.lookup(name) {
                    Some(SymInfo::Variable { ty, ref_count, .. }) => {
                        ref_count.set(ref_count.get() + 1);
                        TypedExpr::Identifier {
                            name: name.to_string(),
                            ty: ty.clone(),
                            loc: loc.clone(),
                        }
                    }
                    _ => {
                        self.report_error(SemanError::ReferenceToUndefinedName {
                            loc: loc.clone(),
                            var_name: name.to_string(),
                        });
                        TypedExpr::Error { loc: loc.clone() }
                    }
                }
            }
            Expr::AccessMember { target, mem, loc } => {
                todo!()
            }
            Expr::IndexInto { target, index, loc } => {
                todo!()
            }
            _ => {
                self.report_error(SemanError::CannotAssignToTarget {
                    loc: expr.get_source_ref(),
                });
                TypedExpr::Error {
                    loc: expr.get_source_ref(),
                }
            }
        }
    }

    fn is_mutable_target(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Identifier { name, .. } => match self.lookup(name) {
                Some(SymInfo::Variable { is_mutable, .. }) => *is_mutable,
                _ => false,
            },
            Expr::AccessMember { target, mem, .. } => self.is_mutable_target(target),
            Expr::IndexInto { target, index, .. } => self.is_mutable_target(target),
            _ => false,
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
                        let typed_init_value = if let Some(var_ty) = ty {
                            self.validate_expression(init_expr, Some(var_ty))
                        } else {
                            self.validate_expression(init_expr, None)
                        };
                        let init_ty = typed_init_value.get_ty();

                        match typed_init_value {
                            TypedExpr::Error { loc } => TypedIns::Error,
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
                            let typed_init = self.validate_expression(init_expr, Some(var_ty));
                            let init_ty = typed_init.get_ty();
                            if typed_init.is_error_expr() {
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
                            let typed_init = self.validate_expression(init_expr, None);
                            let init_ty = typed_init.get_ty();
                            if typed_init.is_error_expr() {
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
                // set the return type expected for this function
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

                let prev_ret_ty = self.current_fn_ret_ty.replace(ret_ty.clone());
                let typed_body = self.validate_instruction(body);
                self.current_fn_ret_ty = prev_ret_ty;
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
            Ins::AssignTo { target, value, loc } => {
                // validate the target expression
                let typed_target = self.validate_target_expression(target);
                let target_ty = typed_target.get_ty();

                // ensure the target is mutable
                if !self.is_mutable_target(target) {
                    self.report_error(SemanError::CannotAssignToImmutableTarget {
                        target: target.as_str(),
                        loc: loc.clone(),
                    });

                    return TypedIns::Error;
                }

                // validate the value expression
                let typed_val = self.validate_expression(value, Some(&target_ty));
                let val_ty = typed_val.get_ty();

                // ensure the types are compatible
                let validated_ty = Self::type_check(&target_ty, &val_ty, loc.clone());
                if let Ty::ErrorType { loc: err_loc } = validated_ty.as_ref() {
                    self.report_error(SemanError::TypeMismatch {
                        loc: err_loc.clone(),
                        expected: target_ty.as_str(),
                        found: val_ty.as_str(),
                    });
                    return TypedIns::Error;
                }

                TypedIns::AssignTo {
                    target: typed_target,
                    value: typed_val,
                    loc: loc.clone(),
                }
            }
            Ins::ExprIns { expr, loc } => {
                let typed_expr = self.validate_expression(expr, None);
                TypedIns::ExprIns {
                    expr: typed_expr,
                    loc: loc.clone(),
                }
            }
            Ins::IfConditional {
                conds_and_code,
                loc,
            } => {
                let mut typed_conds_and_code = vec![];
                for (cond, code) in conds_and_code {
                    let typed_cond = if let Some(cond) = cond {
                        Some(self.validate_expression(cond, None))
                    } else {
                        None
                    };

                    let typed_code = self.validate_instruction(code);

                    typed_conds_and_code.push((typed_cond, typed_code));
                }
                TypedIns::IfConditional {
                    conds_and_code: typed_conds_and_code,
                    loc: loc.clone(),
                }
            }
            Ins::Return { expr, loc } => match self.current_fn_ret_ty.clone() {
                Some(expected_ty) => match expr {
                    Some(return_expr) => {
                        // return with value
                        let typed_expr = self.validate_expression(return_expr, Some(&expected_ty));
                        let expr_ty = typed_expr.get_ty();

                        let validated_ty = Self::type_check(&expected_ty, &expr_ty, loc.clone());
                        if validated_ty.is_error_ty() {
                            self.report_error(SemanError::MismatchingReturnType {
                                exp: expected_ty.as_str(),
                                given: expr_ty.as_str(),
                                loc_given: loc.clone(),
                            });
                            TypedIns::Error
                        } else {
                            TypedIns::Return {
                                expr: Some(typed_expr),
                                loc: loc.clone(),
                            }
                        }
                    }
                    None => {
                        // empty return
                        if matches!(expected_ty.as_ref(), Ty::Void { .. }) {
                            TypedIns::Return {
                                expr: None,
                                loc: loc.clone(),
                            }
                        } else {
                            self.report_error(SemanError::MismatchingReturnType {
                                exp: expected_ty.as_str(),
                                given: "void".into(),
                                loc_given: loc.clone(),
                            });
                            TypedIns::Error
                        }
                    }
                },
                None => {
                    // return statement outside of function context
                    self.report_error(SemanError::ReturnOutsideFunction { loc: loc.clone() });
                    TypedIns::Error
                }
            },
            Ins::SingleLineComment { content, loc } => TypedIns::SingleLineComment {
                content: content.clone(),
                loc: loc.clone(),
            },
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
        // for (name, name_info) in analyzer.scopes[0].symbols.iter() {
        //     println!("{name}: {}", name_info.as_str())
        // }

        // pass 2: validate everything in the program
        let typed_program = analyzer.validate_program(program);
        if !analyzer.errors.is_empty() {
            return Err(analyzer.errors);
        }

        Ok(typed_program)
    }
}
