use super::types::PIRTypes;
use crate::frontend::{ast::TypeReference, source::SourceRef};

#[allow(dead_code)]
#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
pub enum SemanticAnalysisError {
    UndefinedSymbol(String, SourceRef),
    RedefinitionOfSymbol(SourceRef),
    UndefinedType(String, SourceRef),
    UseOfSymbolBeforeInitialization(String, SourceRef),
    TypeMismatch(String, String, SourceRef, SourceRef),
}

// A symbol represents a variable, constant, a function or a module
// It is used to keep track of some information about the symbol
// while it is in scope
#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct Symbol {
    pub identifier: String,             // identifier of the symbol
    pub definition_loc: Option<usize>,  // index of the instruction where the symbol is defined
    pub depth: usize,                   // depth of the scope where the symbol is defined
    pub associated_type: Option<usize>, // index of the type for the symbol in defined_types array
    pub been_initialized: bool,         // whether the symbol has been initialized
    pub is_mutable: bool,               // whether the symbol is mutable
}

#[allow(dead_code)]
pub struct SymbolTable {
    defined_types: Vec<PIRTypes>,
    defined_names: Vec<Symbol>,
    pub depth: usize,
}

#[allow(dead_code)]
impl SymbolTable {
    pub fn new() -> Self {
        let mut sym_table = SymbolTable {
            defined_types: Vec::new(),
            defined_names: Vec::new(),
            depth: 0,
        };

        // register primitive types
        // - i8, i16, i32, i64, isize
        let signed_int_names = vec!["i8", "i16", "i32", "i64", "isize"];
        for name in signed_int_names {
            sym_table.register_type(PIRTypes::BuiltIn {
                name: name.to_string(),
            });
        }

        let unsigned_int_names = vec!["u8", "u16", "u32", "u64", "usize"];
        for name in unsigned_int_names {
            sym_table.register_type(PIRTypes::BuiltIn {
                name: name.to_string(),
            });
        }

        let remnants = vec!["bool", "char", "void", "str"];
        for name in remnants {
            sym_table.register_type(PIRTypes::BuiltIn {
                name: name.to_string(),
            });
        }

        sym_table.enter_scope();
        return sym_table;
    }

    pub fn register_sym(&mut self, sym: Symbol) -> usize {
        // println!(
        //     "{}. Registering symbol: {:#?}",
        //     self.defined_names.len(),
        //     sym
        // );
        self.defined_names.push(sym);
        self.defined_names.len() - 1
    }

    pub fn register_type(&mut self, ty: PIRTypes) -> usize {
        // println!("{}. Registering type: {:#?}", self.defined_types.len(), ty);
        self.defined_types.push(ty);
        self.defined_types.len() - 1
    }

    pub fn type_at_loc(&self, loc: usize) -> PIRTypes {
        self.defined_types[loc].clone()
    }

    pub fn enter_scope(&mut self) {
        self.depth += 1;
    }

    pub fn exit_scope(&mut self) {
        self.depth -= 1;
        // remove all symbols that are defined in this scope
        self.defined_names.retain(|sym| sym.depth < self.depth);
        self.defined_types.retain(|ty| ty.get_depth() < self.depth);
    }

    pub fn shallow_type_exists(&self, type_name: &str) -> bool {
        // only search for the type in the current scope
        // search for the type walking backwards
        for defined_type in self.defined_types.iter().rev() {
            if defined_type.get_depth() < self.depth {
                break;
            }

            if defined_type.get_name() == type_name {
                return true;
            }
        }
        return false;
    }

    pub fn shallow_sym_exists(&self, sym: &str) -> bool {
        // only search for the symbol in the current scope
        // search for the symbol walking backwards
        for defined_sym in self.defined_names.iter().rev() {
            if defined_sym.depth < self.depth {
                break;
            }

            if defined_sym.identifier == sym {
                return true;
            }
        }
        return false;
    }

    pub fn type_exists(&self, type_name: &str) -> bool {
        // search for the type walking backwards
        for defined_type in self.defined_types.iter().rev() {
            if defined_type.get_name() == type_name {
                return true;
            }
        }
        return false;
    }

    pub fn sym_exists(&self, sym: &str) -> bool {
        // search for the symbol walking backwards
        for defined_sym in self.defined_names.iter().rev() {
            if defined_sym.identifier == sym {
                return true;
            }
        }
        return false;
    }

    pub fn get_type_loc(
        &self,
        ty: &str,
        ty_ref: SourceRef,
    ) -> Result<usize, SemanticAnalysisError> {
        // search for the type walking backwards
        for (loc, defined_type) in self.defined_types.iter().rev().enumerate() {
            if defined_type.get_name() == ty {
                return Ok(self.defined_types.len() - loc - 1);
            }
        }
        return Err(SemanticAnalysisError::UndefinedType(ty.to_string(), ty_ref));
    }

    pub fn get_sym(&self, sym: &str) -> Option<&Symbol> {
        // search for the symbol walking backwards
        for defined_sym in self.defined_names.iter().rev() {
            if defined_sym.identifier == sym {
                return Some(defined_sym);
            }
        }
        return None;
    }

    pub fn register_uninitialized_sym(&mut self, sym: &str, is_mutable: bool) {
        let new_sym = Symbol {
            identifier: sym.to_string(),
            definition_loc: None,
            depth: self.depth,
            associated_type: None,
            been_initialized: false,
            is_mutable, // uninitialized type
        };

        self.register_sym(new_sym);
    }

    pub fn compare_types(
        &self,
        l_type: &PIRTypes,
        l_type_ref: SourceRef,
        r_type: &PIRTypes,
        r_type_ref: SourceRef,
    ) -> Result<(), SemanticAnalysisError> {
        if l_type == r_type {
            Ok(())
        } else {
            Err(SemanticAnalysisError::TypeMismatch(
                l_type.get_name(),
                r_type.get_name(),
                l_type_ref,
                r_type_ref,
            ))
        }
    }

    pub fn loc_compare_types(
        &self,
        l_type: &usize,
        l_type_ref: SourceRef,
        r_type: &usize,
        r_type_ref: SourceRef,
    ) -> Result<(), SemanticAnalysisError> {
        if l_type == r_type {
            Ok(())
        } else {
            Err(SemanticAnalysisError::TypeMismatch(
                self.type_at_loc(*l_type).as_str(),
                self.type_at_loc(*r_type).as_str(),
                l_type_ref,
                r_type_ref,
            ))
        }
    }

    pub fn update_sym_type(&mut self, sym: String, ty: usize) {
        for defined_sym in self.defined_names.iter_mut().rev() {
            if defined_sym.identifier == sym {
                defined_sym.associated_type = Some(ty);
                break;
            }
        }
    }

    pub fn make_type_reference(&self, type_loc: usize) -> TypeReference {
        let ty = &self.defined_types[type_loc];
        match ty {
            PIRTypes::BuiltIn { name } => TypeReference::IdentifierType(name.clone(), None),
            PIRTypes::Struct { name, .. } => TypeReference::IdentifierType(name.clone(), None),
            PIRTypes::Function { .. } => {
                unreachable!("Function type should not be used as a type reference")
            }
        }
    }
}
