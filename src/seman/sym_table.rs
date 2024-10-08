#![allow(unused)]

use std::{collections::HashMap, rc::Rc};

use crate::{parser::ast::Ins, source::source::SourceRef};

use super::{
    tast::TyIns,
    type_table::{TypeId, TypeInst},
};

#[derive(Debug, Clone)]
pub enum SymbolInfo {
    Resolved {
        ty_inst: TypeInst,
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
    pub is_ood_scope: bool,
}

impl SymbolScope {
    pub fn new(parent: Option<usize>, scope_id: usize, is_ood_scope: bool) -> Self {
        SymbolScope {
            parent,
            names: HashMap::new(),
            scope_id,
            is_ood_scope,
            gen_ins: vec![],
        }
    }

    pub fn insert(&mut self, name: Rc<String>, info: Rc<SymbolInfo>) {
        self.names.insert(name, info);
    }

    pub fn insert_ins(&mut self, ins: TyIns) {
        self.gen_ins.push(ins);
    }

    pub fn display(&self) {
        for (name, info) in self.names.iter() {
            match info.as_ref() {
                SymbolInfo::Resolved {
                    ty_inst,
                    def_loc,
                    mutable,
                } => {
                    println!(
                        "{name}: {}TypeId({})",
                        if *mutable { "" } else { "const " },
                        ty_inst.id
                    )
                }
                SymbolInfo::Unresolved { ins, mutable } => {
                    println!("{name}: {}Unresolved", if *mutable { "" } else { "const " })
                }
            }
        }
    }
}
