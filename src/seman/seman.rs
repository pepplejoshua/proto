#![allow(unused)]

use std::{collections::HashMap, rc::Rc};

use crate::{
    parser::{
        ast::{Expr, FnParam, Ins},
        type_signature::Ty,
    },
    source::{errors::SemanError, source::SourceRef},
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
}

impl SemanticAnalyzer {
    fn new() -> Self {
        let mut scopes = vec![Scope::new_root()];
        Self {
            scopes,
            current_scope_idx: 0,
            errors: vec![],
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

    fn lookup_function(&self, name: &str) -> Option<(Vec<FnParam>, Rc<Ty>)> {
        todo!()
    }

    fn lookup_variable(&self, name: &str) -> Option<(Rc<Ty>, bool)> {
        todo!()
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
                // Ignore other instructions in Pass 1
                _ => {}
            }
        }
    }

    fn validate_program(&mut self, program: &[Ins]) {
        todo!()
    }

    fn validate_expression(&mut self, expr: &Expr) -> (Ty, TypedExpr) {
        todo!()
    }

    fn validate_instruction(&mut self, ins: &Ins) -> TypedIns {
        todo!()
    }

    pub fn analyze_program(program: &[Ins]) -> Result<Vec<TypedIns>, Vec<SemanError>> {
        let mut analyzer = SemanticAnalyzer::new();

        // pass 1: collect all top level declarations
        analyzer.collect_declarations(program);
        if !analyzer.errors.is_empty() {
            return Err(analyzer.errors);
        }
        for (name, name_info) in analyzer.scopes[0].symbols.iter() {
            println!("{name}: {}", name_info.as_str())
        }

        // pass 2: validate everything in the program
        analyzer.validate_program(program);
        if !analyzer.errors.is_empty() {
            return Err(analyzer.errors);
        }

        todo!()
    }
}
