#![allow(unused)]

use std::{collections::HashMap, rc::Rc};

use crate::{parser::ast::Ins, source::source::SourceRef};

use super::{tast::TyIns, type_table::TypeId};

#[derive(Debug, Clone)]
pub enum SymbolInfo {
    Resolved {
        ty: TypeId,
        def_loc: Rc<SourceRef>,
        mutable: bool,
    },
    Unresolved {
        ins: Rc<Ins>,
        mutable: bool,
    },
}

#[derive(Debug, Clone)]
pub struct SymbolScope {
    pub parent: Option<usize>,
    pub names: HashMap<Rc<String>, Rc<SymbolInfo>>,
    pub scope_id: usize,
    pub gen_ins: Vec<TyIns>,
}

impl SymbolScope {
    pub fn new(parent: Option<usize>, scope_id: usize) -> Self {
        SymbolScope {
            parent,
            names: HashMap::new(),
            scope_id,
            gen_ins: vec![],
        }
    }

    pub fn insert(&mut self, name: Rc<String>, info: Rc<SymbolInfo>) {
        self.names.insert(name, info);
    }

    pub fn insert_ins(&mut self, ins: TyIns) {
        self.gen_ins.push(ins);
    }
}
