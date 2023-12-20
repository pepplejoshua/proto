use crate::frontend::bcode::Index;

// captures information about the environment relevant for
// type checking, name resolution, and code generation
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Env {
    pub names: Vec<(Index, Index)>,     // (name_i, type_i)
    pub functions: Vec<(Index, Index)>, // (name_i, type_i)
    pub types: Vec<Index>,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct EIndex(pub usize);

impl Env {
    pub fn new() -> Self {
        Env {
            names: Vec::new(),
            functions: Vec::new(),
            types: Vec::new(),
        }
    }
}
