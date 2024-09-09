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

pub struct LazyBoi {
    current_scope: Rc<RefCell<SymbolScope>>,
    types: TypeTable,
    ongoing_resolutions_stack: Vec<String>,
    source_files: HashMap<Rc<String>, Rc<SourceFile>>,
}

pub fn process_file(src: Rc<SourceFile>, instructions: Vec<Ins>) -> Vec<SemanError> {
    let mut lazy_boi = LazyBoi::new(src);
    lazy_boi.check_file(instructions, true)
}

impl LazyBoi {
    pub fn new(source_file: Rc<SourceFile>) -> Self {
        LazyBoi {
            current_scope: SymbolScope::new(None),
            types: TypeTable::new(),
            ongoing_resolutions_stack: vec![],
            source_files: HashMap::from([(source_file.path.clone(), source_file)]),
        }
    }

    pub fn enter_scope(&mut self) {
        let new_scope = self.current_scope.borrow().create_child();
        self.current_scope = new_scope;
    }

    pub fn exit_scope(&mut self) {
        if let Some(parent) = self.current_scope.clone().borrow().parent.clone() {
            self.current_scope = parent;
        } else {
            // Handle error: trying to exit the global scope
            panic!("Attempting to exit the global scope");
        }
    }

    pub fn shallow_find_symbol_info(&self, name: &String) -> Option<Rc<SymbolInfo>> {
        self.current_scope.borrow().shallow_lookup(name)
    }

    pub fn deep_find_symbol_info(&self, name: &String) -> Option<Rc<SymbolInfo>> {
        self.current_scope.borrow().deep_lookup(name)
    }

    pub fn check_file(&mut self, instructions: Vec<Ins>, is_main_file: bool) -> Vec<SemanError> {
        let rc_instructions = instructions
            .into_iter()
            .map(|ins| Rc::new(ins))
            .collect::<Vec<_>>();
        let mut name_order = vec![];
        let mut has_main = false;
        for ins in rc_instructions.iter() {
            if let Some(name) = ins.get_id(&self.source_files[ins.get_source_ref().file.as_ref()]) {
                if is_main_file && name == "main" {
                    has_main = true;
                }
                name_order.push(name.clone());
                let info = Rc::new(SymbolInfo::Unresolved {
                    mutable: !matches!(ins.as_ref(), Ins::DeclConst { .. }),
                    ins: ins.clone(),
                });
                self.current_scope.borrow_mut().insert(Rc::new(name), info);
            } else {
                unreachable!("Instruction without an ID");
            }
        }

        // check the instructions in the order they appear
        for name in name_order {
            let info = self.deep_find_symbol_info(&name).unwrap();
            if let SymbolInfo::Unresolved { ins, .. } = info.as_ref() {
                self.check_instruction(ins);
            } else {
            }
        }
        todo!()
    }

    pub fn check_instruction(&mut self, ins: &Rc<Ins>) {
        match ins.as_ref() {
            Ins::DeclConst {
                name,
                ty,
                init_val,
                loc,
            } => todo!(),
            Ins::DeclVar {
                name,
                ty,
                init_val,
                loc,
            } => todo!(),
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

    pub fn check_expr(&mut self, expr: &Rc<Expr>) {
        match expr.as_ref() {
            Expr::Identifier { loc } => {
                let name = expr.as_str(&self.source_files[loc.file.as_ref()]);
                let info = self.deep_find_symbol_info(&name).unwrap();
                if let SymbolInfo::Resolved { ty, .. } = info.as_ref() {
                    // we don't need to check the type. We can just return it
                } else {
                    // we can perform demand driven resolution here
                    // if the resolution failed, we can return an error
                    self.ongoing_resolutions_stack.push(name);
                    self.check_instruction(ins)
                }
            }
            Expr::Underscore { loc } => todo!(),
            Expr::Integer { loc } => todo!(),
            Expr::Decimal { loc } => todo!(),
            Expr::Str { loc } => todo!(),
            Expr::Char { loc } => todo!(),
            Expr::Bool { loc } => todo!(),
            Expr::Tuple { items, loc } => todo!(),
            Expr::StaticArray { ty, items, loc } => todo!(),
            Expr::Struct { body, loc } => todo!(),
            Expr::TypeAsExpr { ty } => todo!(),
            Expr::UnaryOp { op, expr, loc } => todo!(),
            Expr::BinOp {
                op,
                left,
                right,
                loc,
            } => todo!(),
            Expr::ConditionalExpr {
                cond,
                then,
                otherwise,
                loc,
            } => todo!(),
            Expr::CallFn { func, args, loc } => todo!(),
            Expr::GroupedExpr { inner, loc } => todo!(),
            Expr::IndexInto { target, index, loc } => todo!(),
            Expr::MakeSlice {
                target,
                start,
                end,
                loc,
            } => todo!(),
            Expr::AccessMember { target, mem, loc } => todo!(),
            Expr::OptionalExpr { val, loc } => todo!(),
            Expr::ComptimeExpr { val, loc } => todo!(),
            Expr::Lambda {
                params,
                ret_type,
                body,
                loc,
            } => todo!(),
            Expr::DerefPtr { target, loc } => todo!(),
            Expr::MakePtrFromAddrOf { target, loc } => todo!(),
            Expr::InitializerList { pairs, loc } => todo!(),
            Expr::ErrorExpr { loc } => todo!(),
        }
    }

    pub fn check_type(&mut self, ty: &Rc<Ty>) {
        match ty.as_ref() {
            Ty::Type { loc } => todo!(),
            Ty::Signed { size, is_int, loc } => todo!(),
            Ty::Unsigned { size, is_uint, loc } => todo!(),
            Ty::Float { size, loc } => todo!(),
            Ty::Str { loc } => todo!(),
            Ty::Char { loc } => todo!(),
            Ty::Void { loc } => todo!(),
            Ty::Bool { loc } => todo!(),
            Ty::Func {
                params,
                ret,
                loc,
                is_const,
            } => todo!(),
            Ty::StaticArray { sub_ty, size, loc } => todo!(),
            Ty::Slice { sub_ty, loc } => todo!(),
            Ty::Optional { sub_ty, loc } => todo!(),
            Ty::Struct {
                fields,
                static_funcs,
                methods,
                loc,
            } => todo!(),
            Ty::NamedType { loc } => todo!(),
            Ty::AccessMemberType { target, mem, loc } => todo!(),
            Ty::TypeFunc { func, args, loc } => todo!(),
            Ty::Pointer { sub_ty, loc } => todo!(),
            Ty::Tuple { sub_tys, loc } => todo!(),
            Ty::ErrorType { loc } => todo!(),
        }
    }
}
