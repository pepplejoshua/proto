#![allow(unused)]

use std::{collections::HashMap, rc::Rc};

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

use super::typed_ast::{TypedExpr, TypedIns};

#[derive(Debug, Clone)]
enum SymInfo {
    Fn {
        params: Vec<FnParam>,
        return_type: Rc<Ty>,
        is_global: bool,
        def_loc: Rc<SourceRef>,
    },
    Variable {
        ty: Rc<Ty>,
        is_mutable: bool,
        def_loc: Rc<SourceRef>,
    },
    TypeAlias {
        target_ty: Rc<Ty>,
        def_loc: Rc<SourceRef>,
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
            } => format!(
                "{} | {}",
                if *is_mutable { "var" } else { "const " },
                ty.as_str()
            ),
            SymInfo::TypeAlias { target_ty, def_loc } => todo!(),
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

    fn is_integer_type(&self, ty: &Ty) -> bool {
        matches!(ty, Ty::Signed { is_int: true, .. })
    }

    fn is_unsigned_integer_type(&self, ty: &Ty) -> bool {
        matches!(ty, Ty::Unsigned { is_uint: true, .. })
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

                if !self.is_integer_type(&return_type) {
                    additional_errors.push(SemanError::TypeMismatch {
                        loc: return_type.get_loc(),
                        expected: "int".into(),
                        found: return_type.as_str(),
                    });
                }
            }
            Some(sym_info) => {
                self.report_error(SemanError::Expected(
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

    fn validate_expression(&mut self, expr: &Expr, parent_ty: Option<&Rc<Ty>>) -> (Ty, TypedExpr) {
        match expr {
            Expr::Integer { content, loc } => todo!(),
            Expr::Decimal { content, loc } => todo!(),
            Expr::Str { content, loc } => todo!(),
            Expr::Char { content, loc } => todo!(),
            Expr::Bool { val, loc } => todo!(),
            Expr::Tuple { items, loc } => todo!(),
            Expr::StaticArray { ty, items, loc } => todo!(),
            Expr::TypeAsExpr { ty } => todo!(),
            Expr::Identifier { name, loc } => todo!(),
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
            Expr::CallFn { func, args, loc } => todo!(),
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
            } => todo!(),
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
                        if self.shallow_lookup(&(param_name.to_string())).is_some() {
                            self.report_error(SemanError::NameAlreadyDefined {
                                loc: param.loc.clone(),
                                name: param_name.to_string(),
                            });
                            continue;
                        }
                        self.scopes[self.current_scope_idx].add_symbol(
                            param_name.to_string(),
                            SymInfo::Variable {
                                ty: param.given_ty.clone(),
                                is_mutable: param.is_mutable,
                                def_loc: param.loc.clone(),
                            },
                        );
                    }
                }
                let typed_body = self.validate_instruction(body);
                self.exit_scope();

                TypedIns::DeclFunc {
                    name: name.as_str(),
                    params: params.clone(),
                    ret_ty: ret_ty.clone(),
                    body: Rc::new(typed_body),
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
                TypedIns::Block { code: typed_code }
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
        let mut analyzer = SemanticAnalyzer::new(main_src_file);

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
