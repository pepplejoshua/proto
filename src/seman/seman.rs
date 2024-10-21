#![allow(unused)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::ast::{Expr, FnParam, Ins};
use crate::parser::type_signature::Ty;
use crate::seman::type_table::TypeTable;
use crate::source::errors::SemanError;
use crate::source::source::{SourceFile, SourceRef};

use super::sym_table::{SymbolInfo, SymbolScope};
use super::tast::{TyExpr, TyExprId, TyIns, TyInsId};
use super::type_table::{TypeId, TypeInst};

pub struct CodeBundle {
    expressions: Vec<TyExpr>,
    instructions: Vec<TyIns>,
}

pub struct CheckerState {
    type_table: TypeTable,
    scope_stack: Vec<SymbolScope>,
    current_scope: usize,
    ongoing_resolution_stack: Vec<Rc<String>>,
    pub seman_errors: Vec<SemanError>,
    cur_fn_return_type: Option<TypeInst>,
    main_file_str: Rc<String>,
    files: HashMap<Rc<String>, Rc<SourceFile>>,
    generated_code: CodeBundle,
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
            cur_fn_return_type: None,
            generated_code: CodeBundle {
                expressions: vec![TyExpr::ErrorExpr],
                instructions: vec![TyIns::ErrorIns],
            },
        }
    }

    fn enter_scope(&mut self, is_ood_scope: bool) {
        self.scope_stack.push(SymbolScope::new(
            Some(self.current_scope),
            self.scope_stack.len(),
            is_ood_scope,
        ));
        self.current_scope = self.scope_stack.len() - 1;
    }

    fn exit_scope(&mut self) {
        let last_scope = self.scope_stack.pop().unwrap();
        self.current_scope = last_scope.parent.unwrap();
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
                    self.check_ins(ins, true);
                }
            }
        }

        self.scope_stack[0].display();
        self.type_table.display();
    }

    fn preload_scope_with_names(&mut self, instructions: Vec<Ins>) -> Vec<Rc<String>> {
        let mut ins_ids = vec![];
        for ins in instructions.into_iter() {
            let ins_file = self.get_sourcefile_name_from_loc(&ins.get_source_ref());
            let ins_id = Rc::new(ins.get_id(&self.files[&ins_file]).unwrap());
            if self.scope_stack[self.current_scope]
                .names
                .contains_key(&ins_id)
            {
                // we need to report a duplicate name
                // report error
                self.report_error(SemanError::NameAlreadyDefined {
                    loc: ins.get_source_ref(),
                    name: ins_id.as_ref().clone(),
                });
                continue;
            }
            ins_ids.push(ins_id.clone());
            let is_mutable = if let Ins::DeclVariable { is_mutable, .. } = ins {
                is_mutable
            } else {
                false
            };
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

    fn get_sourcefile_name_from_loc(&self, loc: &SourceRef) -> Rc<String> {
        return self.files[&loc.file].path.clone();
    }

    fn get_sourcefile_from_loc(&self, loc: &SourceRef) -> Rc<SourceFile> {
        return self.files[&loc.file].clone();
    }

    fn shallow_name_search(&self, name: &String) -> Option<Rc<SymbolInfo>> {
        for (stored_name, info) in &self.scope_stack[self.current_scope].names {
            if stored_name.as_ref() == name {
                return Some(info.clone());
            }
        }
        None
    }

    fn expr_as_str(&self, expr: &Expr) -> Rc<String> {
        Rc::new(expr.as_str())
    }

    fn intern_typed_expr(&mut self, t_expr: TyExpr) -> TyExprId {
        let t_expr_id = self.generated_code.expressions.len();
        self.generated_code.expressions.push(t_expr);
        t_expr_id
    }

    fn intern_typed_ins(&mut self, t_ins: TyIns) -> TyInsId {
        let t_ins_id = self.generated_code.instructions.len();
        self.generated_code.instructions.push(t_ins);
        t_ins_id
    }

    pub fn check_ins(&mut self, ins: &Ins, enter_scope: bool) -> TyInsId {
        match ins {
            Ins::DeclVariable {
                name,
                ty,
                init_val,
                loc,
                is_mutable,
                is_public,
            } => {
                // we will typecheck the value assigned
                todo!()
            }
            Ins::DeclFunc {
                name,
                params,
                ret_ty,
                body,
                loc,
                is_public,
            } => {
                // track the name of the function in the list of ongoing resolutions
                // TODO: can we not just check if a name is in the list of ongoing resolutions, instead of
                // doing the weird ood_scope checking?
                let name_str = self.expr_as_str(&name);
                self.ongoing_resolution_stack.push(name_str.clone());

                let name_info = self.shallow_name_search(&self.expr_as_str(&name));
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

                        return todo!();
                    }
                }

                // track the scope in which to insert the information about the function being checked since
                // we are entering a new scope to register the params as well as check the body,
                let fn_parent_scope = self.current_scope;
                self.enter_scope(false);
                let mut param_tys = vec![];
                for param in params.iter() {
                    let param_ty = param.given_ty.clone();
                    let param_ty_inst = self.type_table.intern_type(param_ty.clone());
                    let param_info = Rc::new(SymbolInfo::Resolved {
                        ty_inst: param_ty_inst,
                        def_loc: param.loc.clone(),
                        mutable: param.is_mutable,
                    });
                    let param_name_str = self.expr_as_str(&param.name);
                    let param_src_file =
                        self.scope_stack[self.current_scope].insert(param_name_str, param_info);
                    param_tys.push(param_ty);
                }
                let fn_ret_ty_inst = self.type_table.intern_type(ret_ty.clone());
                let prev_fn_ret_ty = self.cur_fn_return_type.clone();
                self.cur_fn_return_type = Some(fn_ret_ty_inst);
                let fn_ty = Ty::Func {
                    params: param_tys,
                    ret: ret_ty.clone(),
                    loc: loc.clone(),
                    is_const: true,
                };

                let fn_ty_inst = self.type_table.intern_type(Rc::new(fn_ty));
                self.scope_stack[fn_parent_scope].insert(
                    name_str,
                    Rc::new(SymbolInfo::Resolved {
                        ty_inst: fn_ty_inst,
                        def_loc: loc.clone(),
                        mutable: false,
                    }),
                );

                let block_code_id = self.check_ins(body, false);

                // check the body of the function and make sure all return values are of the type the function expects
                self.exit_scope();
                self.cur_fn_return_type = prev_fn_ret_ty;
                self.ongoing_resolution_stack.pop();
                todo!()
            }
            Ins::DeclTypeAlias {
                name,
                ty,
                loc,
                is_public,
            } => todo!(),
            Ins::Defer { sub_ins, loc } => todo!(),
            Ins::Block { code, loc } => {
                if enter_scope {
                    self.enter_scope(false);
                }

                for ins in code.iter() {
                    let ty_ins_id = self.check_ins(ins, true);
                }

                if enter_scope {
                    self.exit_scope();
                }
                todo!()
            }
            Ins::AssignTo { target, value, loc } => todo!(),
            Ins::ExprIns { expr, loc } => todo!(),
            Ins::IfConditional {
                conds_and_code,
                loc,
            } => todo!(),
            Ins::Return { expr, loc } => todo!(),
            Ins::SingleLineComment { content, loc } => todo!(),
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
            Ins::ErrorIns { loc } => unreachable!(),
        }
    }

    pub fn resolve_identifier(&mut self, id: String) {
        todo!()
    }

    pub fn check_expr(&mut self, expr: &Expr) -> (TyExprId, TypeId) {
        todo!()
    }

    pub fn check_type(&mut self, ty: &Ty) {
        todo!()
    }
}
