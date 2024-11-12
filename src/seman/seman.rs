#![allow(unused)]

use std::{collections::HashMap, rc::Rc};

use crate::{
    parser::{
        ast::{Expr, FnParam, Ins},
        type_signature::Ty,
    },
    source::{errors::SemanError, source::SourceRef},
};

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

struct SemanticAnalyzer {
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
}
