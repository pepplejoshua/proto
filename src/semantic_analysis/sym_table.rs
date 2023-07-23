use crate::frontend::source::SourceRef;

#[allow(dead_code)]
#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
pub enum SemanticAnalysisError {
    UndefinedSymbol(SourceRef),
    RedefinitionOfSymbol(SourceRef),
    TypeNotDefined(String, SourceRef),
    TypeMismatch(String, String, SourceRef),
}

#[allow(dead_code)]
#[derive(Clone, PartialEq, Eq)]
pub enum TypeKind {
    UserDefined,
    Primitive,
    Uninitialized,
}

#[allow(dead_code)]
pub struct Symbol {
    identifier: String,             // name of the symbol
    definition_loc: Option<usize>,  // index of the instruction where the symbol is defined
    depth: usize,                   // depth of the scope where the symbol is defined
    associated_type: Option<usize>, // indexes defined_types array
}

#[allow(dead_code)]
#[derive(Clone, PartialEq, Eq)]
pub struct Type {
    pub identifier: String,
    pub definition_loc: Option<usize>,
    pub depth: usize,
    pub kind: TypeKind,
}

#[allow(dead_code)]
pub struct SymbolTable {
    defined_types: Vec<Type>,
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

        sym_table.register_type(Type {
            identifier: "uninitialized".to_string(),
            definition_loc: None,
            depth: 0,
            kind: TypeKind::Uninitialized,
        });

        // register primitive types
        // - i8, i16, i32, i64, isize
        let signed_int_names = vec!["i8", "i16", "i32", "i64", "isize"];
        for name in signed_int_names {
            sym_table.register_type(Type {
                identifier: name.to_string(),
                definition_loc: None,
                depth: 0,
                kind: TypeKind::Primitive,
            });
        }

        let unsigned_int_names = vec!["u8", "u16", "u32", "u64", "usize"];
        for name in unsigned_int_names {
            sym_table.register_type(Type {
                identifier: name.to_string(),
                definition_loc: None,
                depth: 0,
                kind: TypeKind::Primitive,
            });
        }

        let remnants = vec!["bool", "char", "void", "str"];
        for name in remnants {
            sym_table.register_type(Type {
                identifier: name.to_string(),
                definition_loc: None,
                depth: 0,
                kind: TypeKind::Primitive,
            });
        }

        sym_table.enter_scope();
        return sym_table;
    }

    pub fn register_sym(&mut self, sym: Symbol) {
        self.defined_names.push(sym);
    }

    pub fn register_type(&mut self, ty: Type) {
        self.defined_types.push(ty);
    }

    pub fn enter_scope(&mut self) {
        self.depth += 1;
    }

    pub fn exit_scope(&mut self) {
        self.depth -= 1;
        // remove all symbols that are defined in this scope
        self.defined_names.retain(|sym| sym.depth < self.depth);
        self.defined_types.retain(|ty| ty.depth < self.depth);
    }

    pub fn shallow_type_exists(&self, ty: &str) -> bool {
        // only search for the type in the current scope
        // search for the type walking backwards
        for defined_type in self.defined_types.iter().rev() {
            if defined_type.depth < self.depth {
                break;
            }

            if defined_type.identifier == ty {
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

    pub fn type_exists(&self, ty: &str) -> bool {
        // search for the type walking backwards
        for defined_type in self.defined_types.iter().rev() {
            if defined_type.identifier == ty {
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

    pub fn get_type(&self, ty: &str) -> Option<&Type> {
        // search for the type walking backwards
        for defined_type in self.defined_types.iter().rev() {
            if defined_type.identifier == ty {
                return Some(defined_type);
            }
        }
        return None;
    }

    pub fn get_type_loc(&self, ty: &str) -> Option<usize> {
        // search for the type walking backwards
        for (loc, defined_type) in self.defined_types.iter().rev().enumerate() {
            if defined_type.identifier == ty {
                return Some(loc);
            }
        }
        return None;
    }

    pub fn register_uninitialized_sym(&mut self, sym: &str) {
        let new_sym = Symbol {
            identifier: sym.to_string(),
            definition_loc: None,
            depth: self.depth,
            associated_type: Some(0), // uninitialized type
        };

        self.register_sym(new_sym);
    }
}
