use crate::frontend::source::SourceRef;

#[allow(dead_code)]
pub enum SemanticAnalysisError {
    UndefinedSymbol(SourceRef),
    RedefinitionOfSymbol(SourceRef),
}

#[derive(Clone, PartialEq, Eq)]
pub enum TypeKind {
    UserDefined,
    Primitive,
}

pub struct Symbol {
    identifier: String,             // name of the symbol
    definition_loc: Option<usize>,  // index of the instruction where the symbol is defined
    depth: usize,                   // depth of the scope where the symbol is defined
    associated_type: Option<usize>, // indexes defined_types array
}

pub struct Type {
    identifier: String,
    definition_loc: Option<usize>,
    depth: usize,
    kind: TypeKind,
}

pub struct SymbolTable {
    defined_types: Vec<Type>,
    defined_names: Vec<Symbol>,
    depth: usize,
}

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
        return sym_table;
    }

    pub fn register_sym(&mut self, sym: Symbol) {
        self.defined_names.push(sym);
    }

    pub fn register_type(&mut self, ty: Type) {
        self.defined_types.push(ty);
    }
}
