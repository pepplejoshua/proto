#![allow(dead_code)]
#![allow(unused_variables)]

use crate::{
    parser::pcode::{ExprLoc, Ins, PCode},
    symbol_info::symbol_info::SymbolTable,
    types::signature::{Sig, Type},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CheckerScope {
    Global,
    Function,
    Struct,
    Mod,
    Block,
    Loop,
    Directive,
}

#[derive(Clone)]
pub struct Checker {
    pub pcode: PCode, // parsed code
    pub sym_table: SymbolTable,
    scope: CheckerScope,
}

impl Checker {
    pub fn new(pcode: PCode) -> Checker {
        Checker {
            pcode,
            sym_table: SymbolTable::new(),
            scope: CheckerScope::Global,
        }
    }

    fn verify_type(&self, ty: &Type) -> bool {
        match ty.tag {
            Sig::Identifier => {
                if let Some(name) = &ty.name {
                    self.sym_table.check_name(name)
                } else {
                    panic!("Identifier type has no name");
                }
            }
            Sig::Infer => true,
            Sig::Bool => true,
            Sig::Char => true,
            Sig::Void => true,
            Sig::Str => true,
            Sig::I8 => true,
            Sig::I16 => true,
            Sig::I32 => true,
            Sig::I64 => true,
            Sig::Int => true,
            Sig::U8 => true,
            Sig::U16 => true,
            Sig::U32 => true,
            Sig::U64 => true,
            Sig::UInt => true,
            Sig::Function => true,
            Sig::ErrorType => false,
        }
    }

    pub fn collect_info(&mut self) {
        let pcode = self.pcode.clone();
        let top_level = pcode.top_level;
        for stmt in top_level {
            match stmt {
                Ins::NewConstant { name, ty, val, loc } => {
                    // check if the constant is already defined
                    if self.sym_table.check_name(&name) {
                        panic!("constant {} is already defined", name);
                    }

                    // collect info about the init value
                    let val_ty = self.check_expr(&val);

                    let mut needs_next_pass = false;

                    // if we cannot verify the type of `ty`, we will use the
                    // type of the init expression. if we cannot get that type,
                    // we will use the Sig::Infer type
                    if !self.verify_type(&ty) {}

                    // if we don't know the type of the init expression yet
                    // we will use the Sig::Infer type so we can infer the
                    // type of the constant later in the second pass
                    if val_ty.tag == Sig::Infer {
                        self.sym_table.register(name.clone(), val_ty);
                    } else {
                        // if we know the type of the init expression we can
                        // insert the constant into the symbol table
                        self.sym_table.register(name, ty);
                    }
                }
                _ => todo!(),
            }
        }
        todo!()
    }

    fn check_expr(&mut self, expr_i: &ExprLoc, recv_ty: Option<&Type>) -> Type {
        todo!()
    }

    pub fn check(&mut self) {
        self.collect_info();
        todo!()
    }
}
