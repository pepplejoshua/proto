#![allow(unused)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::ast::{Expr, Ins};
use crate::parser::type_signature::Ty;
use crate::seman::type_table::TypeTable;
use crate::source::errors::SemanError;
use crate::source::source::{SourceFile, SourceRef};

use super::sym_table::{SymbolInfo, SymbolScope};

pub struct CheckerState {
    type_table: TypeTable,
    scope_stack: Vec<SymbolScope>,
    current_scope: usize,
    ongoing_resolution_stack: Vec<Rc<String>>,
    main_file_str: Rc<String>,
    files: HashMap<Rc<String>, Rc<SourceFile>>,
}

impl CheckerState {
    pub fn new(main_file: Rc<SourceFile>) -> CheckerState {
        CheckerState {
            type_table: TypeTable::new(),
            scope_stack: vec![SymbolScope::new(None, 0, true)],
            current_scope: 0,
            ongoing_resolution_stack: vec![],
            main_file_str: main_file.path.clone(),
            files: HashMap::from([(main_file.path.clone(), main_file)]),
        }
    }

    pub fn check_main_file(&mut self, top_lvl: Vec<Ins>) -> Result<(), Vec<SemanError>> {
        let ins_ids = self.preload_scope_with_names(top_lvl);
        let has_main_fn = ins_ids.iter().any(|id| **id == "main");

        if !has_main_fn {
            return Err(vec![SemanError::NoMainFunctionProvided {
                filename: self.main_file_str.clone(),
            }]);
        }

        for id in ins_ids {
            match &self.scope_stack[0].names[&id].as_ref().clone() {
                SymbolInfo::Resolved { .. } => {
                    println!("already resolved {id}")
                }
                SymbolInfo::Unresolved { ins, mutable } => {
                    println!("resolving {id}");
                    self.check_ins(ins);
                }
            }
        }
        Ok(())
    }

    pub fn preload_scope_with_names(&mut self, instructions: Vec<Ins>) -> Vec<Rc<String>> {
        let mut ins_ids = vec![];
        for ins in instructions.into_iter() {
            let ins_file = self.get_sourcefile_str_from_loc(&ins);
            let ins_id = Rc::new(ins.get_id(&self.files[&ins_file]).unwrap());
            ins_ids.push(ins_id.clone());
            let is_mutable = matches!(ins, Ins::DeclVar { .. });
            self.scope_stack[self.current_scope].insert(
                ins_id,
                Rc::new(SymbolInfo::Unresolved {
                    ins: Rc::new(ins),
                    mutable: is_mutable,
                }),
            );
        }
        ins_ids
    }

    pub fn get_sourcefile_str_from_loc(&self, ins: &Ins) -> Rc<String> {
        return self.files[&ins.get_source_ref().file].path.clone();
    }

    pub fn shallow_name_search(&self, name: &String) -> Option<Rc<SymbolInfo>> {
        for (stored_name, info) in &self.scope_stack[self.current_scope].names {
            if stored_name.as_ref() == name {
                return Some(info.clone());
            }
        }
        None
    }

    pub fn check_ins(&mut self, ins: &Ins) {
        match ins {
            Ins::DeclConst {
                name,
                ty,
                init_val,
                loc,
            } => todo!(),
            Ins::PubDecl { ins, loc } => todo!(),
            Ins::DeclVar {
                name,
                ty,
                init_val,
                loc,
            } => todo!(),
            Ins::DeclFunc {
                name,
                params,
                ret_ty,
                body,
                loc,
            } => {
                todo!()
            }
            Ins::DeclTypeAlias { name, ty, loc } => todo!(),
            Ins::Defer { sub_ins, loc } => todo!(),
            Ins::Block { code, loc } => todo!(),
            Ins::AssignTo { target, value, loc } => todo!(),
            Ins::ExprIns { expr, loc } => todo!(),
            Ins::IfConditional {
                conds_and_code,
                loc,
            } => todo!(),
            Ins::Return { expr, loc } => todo!(),
            Ins::SingleLineComment { loc } => todo!(),
            Ins::PrintIns {
                is_println,
                output,
                loc,
            } => todo!(),
            Ins::Break { loc } => todo!(),
            Ins::Continue { loc } => todo!(),
            Ins::ForInLoop {
                loop_var,
                loop_target,
                block,
                loc,
            } => todo!(),
            Ins::InfiniteLoop { block, loc } => todo!(),
            Ins::WhileLoop {
                cond,
                post_code,
                block,
                loc,
            } => todo!(),
            Ins::RegLoop {
                init,
                loop_cond,
                update,
                block,
                loc,
            } => todo!(),
            Ins::ErrorIns { loc } => todo!(),
        }
    }

    pub fn resolve_identifier(&mut self, id: String) {
        todo!()
    }

    pub fn check_expr(&mut self, expr: &Expr) {
        todo!()
    }

    pub fn check_type(&mut self, ty: &Ty) {
        todo!()
    }
}
