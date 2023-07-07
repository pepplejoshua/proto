use std::fmt::format;

use crate::frontend::errors::NameResolutionError;

#[derive(Clone, PartialEq, Eq)]
pub enum SymbolKind {
    Binding,
    Struct { field_names: Vec<String> },
    Function,
}

pub struct Symbol {
    identifier: String,
    kind: SymbolKind,
}

pub struct ResolutionTable {
    symbols: Vec<Symbol>,
}

impl ResolutionTable {
    pub fn new() -> Self {
        Self {
            symbols: Vec::new(),
        }
    }

    pub fn add_symbol(&mut self, identifier: String, kind: SymbolKind) -> Result<(), String> {
        if self.has_symbol(&identifier, &kind) {
            return Err(format!(
                "Could not add symbol '{identifier}' as it was already defined"
            ));
        }

        self.symbols.push(Symbol { identifier, kind });

        Ok(())
    }

    pub fn has_symbol(&self, identifier: &String, kind: &SymbolKind) -> bool {
        for symbol in &self.symbols {
            if symbol.kind == *kind && symbol.identifier == *identifier {
                return true;
            }
        }

        false
    }
}
