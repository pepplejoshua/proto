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
    pub seman_errors: Vec<SemanError>,
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
            seman_errors: vec![],
            files: HashMap::from([(main_file.path.clone(), main_file)]),
        }
    }

    fn report_error(&mut self, err: SemanError) {
        self.seman_errors.push(err);
    }

    pub fn check_main_file(&mut self, top_lvl: Vec<Ins>) {
        let ins_ids = self.preload_scope_with_names(top_lvl);
        let has_main_fn = ins_ids.iter().any(|id| **id == "main");

        if !has_main_fn {
            self.report_error(SemanError::NoMainFunctionProvided {
                filename: self.main_file_str.clone(),
            });
            return;
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

        self.scope_stack[self.current_scope].display();
        self.type_table.display();
    }

    pub fn preload_scope_with_names(&mut self, instructions: Vec<Ins>) -> Vec<Rc<String>> {
        let mut ins_ids = vec![];
        for ins in instructions.into_iter() {
            let ins_file = self.get_sourcefile_str_from_loc(&ins.get_source_ref());
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

    pub fn get_sourcefile_str_from_loc(&self, loc: &SourceRef) -> Rc<String> {
        return self.files[&loc.file].path.clone();
    }

    pub fn get_sourcefile_from_loc(&self, loc: &SourceRef) -> Rc<SourceFile> {
        return self.files[&loc.file].clone();
    }

    pub fn shallow_name_search(&self, name: &String) -> Option<Rc<SymbolInfo>> {
        for (stored_name, info) in &self.scope_stack[self.current_scope].names {
            if stored_name.as_ref() == name {
                return Some(info.clone());
            }
        }
        None
    }

    pub fn check_decl_func(&mut self, func: &Ins) {
        if let Ins::DeclFunc {
            name,
            params,
            ret_ty,
            body,
            loc,
        } = func
        {
            let name_str =
                Rc::new(name.as_str(&self.get_sourcefile_from_loc(&name.get_source_ref())));
            self.ongoing_resolution_stack.push(name_str.clone());
            let name_info = self.shallow_name_search(
                &name.as_str(&self.get_sourcefile_from_loc(&name.get_source_ref())),
            );
            // check the name of the function to make sure it is undeclared (non-ood scope)
            // or unresolved (ood scope)
            if self.scope_stack[self.current_scope].is_ood_scope {
                if name_info.is_none() {
                    // report error
                    unreachable!("name not pre-declared in ood scope. at {}", loc.as_str());
                }
                let name_info = name_info.unwrap();
                // make sure it is an unresolved name
                if let SymbolInfo::Resolved { .. } = name_info.as_ref() {
                    unreachable!(
                        "name pre-declared and already resolved. at {}",
                        loc.as_str()
                    )
                }
            } else {
                if name_info.is_some() {
                    // report error
                    self.report_error(SemanError::NameAlreadyDefined {
                        loc: loc.clone(),
                        name: name_str.as_ref().clone(),
                    });
                    return;
                }
            }
            let mut param_tys = vec![];
            for param in params.iter() {
                param_tys.push(param.given_ty.clone());
            }
            let fn_ty = Ty::Func {
                params: param_tys,
                ret: ret_ty.clone(),
                loc: loc.clone(),
                is_const: true,
            };

            let fn_ty_inst = self.type_table.intern_type(Rc::new(fn_ty));
            self.scope_stack[self.current_scope].insert(
                name_str,
                Rc::new(SymbolInfo::Resolved {
                    ty_inst: fn_ty_inst,
                    def_loc: loc.clone(),
                    mutable: false,
                }),
            );
            self.ongoing_resolution_stack.pop();
        }
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
                self.check_decl_func(ins);
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
