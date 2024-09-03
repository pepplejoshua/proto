#![allow(unused)]

use std::{collections::HashMap, rc::Rc};

use crate::source::source::SourceRef;

use super::type_table::TypeId;

pub struct SymbolInfo {
    name: Rc<String>,
    ty: TypeId,
    loc: Vec<Rc<SourceRef>>,
}

pub struct SymbolTable {
    pub names: HashMap<Rc<String>, SymbolInfo>,
}
