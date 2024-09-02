#![allow(unused)]

use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::ast::{Expr, Ins};
use crate::source::source::SourceRef;
use crate::types::signature::{Ty, TypeId};

pub struct SemanticAnalyzer {
    global_scope: HashMap<String, SymbolInfo>,
    type_table: TypeTable,
    unresolved_symbols: Vec<String>,
}

struct SymbolInfo {
    name: String,
    ty: Option<TypeId>,
    loc: Rc<SourceRef>,
    declaration: Rc<Ins>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer {
            global_scope: HashMap::new(),
            type_table: TypeTable::new(),
            unresolved_symbols: Vec::new(),
        }
    }

    pub fn analyze(&mut self, ast: &[Ins]) -> Result<(), Vec<SemanticError>> {
        // Find the main function and start analysis from there
        if let Some(main_func) = self.find_main_function(ast) {
            self.analyze_function(main_func)?;
        } else {
            return Err(vec![SemanticError::NoMainFunction]);
        }

        // Check for any remaining unresolved symbols
        if !self.unresolved_symbols.is_empty() {
            return Err(vec![SemanticError::UnresolvedSymbols(
                self.unresolved_symbols.clone(),
            )]);
        }

        Ok(())
    }

    fn find_main_function(&self, ast: &[Ins]) -> Option<&Ins> {
        ast.iter().find(|ins| {
            if let Ins::DeclConst { name, .. } = ins {
                if let Expr::Identifier { loc } = name {
                    // You might need to adjust this based on how you store identifiers
                    loc.as_str() == "main"
                } else {
                    false
                }
            } else {
                false
            }
        })
    }

    fn analyze_function(&mut self, func: &Ins) -> Result<(), Vec<SemanticError>> {
        // Implement function analysis logic here
        // This should recursively analyze the function body
        todo!()
    }

    fn resolve_symbol(&mut self, name: &str) -> Result<&SymbolInfo, SemanticError> {
        if let Some(symbol) = self.global_scope.get(name) {
            Ok(symbol)
        } else {
            // Symbol not found, add to unresolved list and return an error
            self.unresolved_symbols.push(name.to_string());
            Err(SemanticError::UnresolvedSymbol(name.to_string()))
        }
    }

    fn check_type(&mut self, expr: &Expr) -> Result<TypeId, SemanticError> {
        // Implement type checking logic here
        todo!()
    }
}

#[derive(Debug)]
enum SemanticError {
    NoMainFunction,
    UnresolvedSymbol(String),
    UnresolvedSymbols(Vec<String>),
    TypeMismatch(TypeId, TypeId),
    // Add more error types as needed
}
