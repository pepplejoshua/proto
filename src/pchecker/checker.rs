#![allow(dead_code)]
#![allow(unused_variables)]

use crate::{
    parser::pcode::{Expr, ExprLoc, Ins, PCode},
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
    needs_next_pass: bool,
}

impl Checker {
    pub fn new(pcode: PCode) -> Checker {
        Checker {
            pcode,
            sym_table: SymbolTable::new(),
            scope: CheckerScope::Global,
            needs_next_pass: false,
        }
    }

    fn verify_type(&self, ty: &Type) -> bool {
        match ty.tag {
            Sig::Identifier => {
                if let Some(name) = &ty.name {
                    self.sym_table.check_name(name)
                } else {
                    panic!("Checker::verify_type: type has no name field");
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
                        panic!("Checker::collect_info: constant {} already defined", name);
                    }

                    // if we cannot verify the type of `ty`, we will use the
                    // type of the init expression. if we cannot get that type,
                    // we will use the Sig::Infer type
                    let mut needs_next_pass = false;
                    let mut sym_ty = ty.clone();
                    if !self.verify_type(&ty) {
                        needs_next_pass = true;
                    } else {
                        // we can check the expression using our verified type
                        let expr_ty = self.check_expr(&val, &ty);
                        if !self.verify_type(&expr_ty) {
                            needs_next_pass = true;
                        } else {
                            if ty.typecheck(&expr_ty) {
                                sym_ty = expr_ty;
                            } else {
                                sym_ty = Type {
                                    tag: Sig::ErrorType,
                                    name: None,
                                    sub_types: vec![],
                                    aux_type: None,
                                    loc: loc.clone(),
                                };
                            }
                        }
                    }

                    if needs_next_pass {
                        // we will need to check the expression again
                        // after we have collected all the type information
                        // we will add the constant to the symbol table
                        self.sym_table.register(
                            name,
                            Type {
                                tag: Sig::Infer,
                                name: None,
                                sub_types: vec![],
                                aux_type: None,
                                loc: ty.loc.clone(),
                            },
                        );
                        self.needs_next_pass = true;
                    } else {
                        // we can add the constant to the symbol table
                        self.sym_table.register(name, sym_ty);
                    }
                }
                _ => todo!(),
            }
        }
    }

    fn check_expr(&mut self, expr_i: &ExprLoc, recv_ty: &Type) -> Type {
        let expr = self.pcode.get_expr_mut(expr_i);

        match expr {
            Expr::Number {
                val,
                loc,
                ty: num_ty,
            } => {
                if matches!(recv_ty.tag, Sig::Infer) {
                    // if the recv_ty is Sig::Infer, we will try to convert the number to
                    // an int. If that fails, we will return an error type
                    let val_int = val.parse::<i64>();
                    if let Ok(_) = val_int {
                        return Type {
                            tag: Sig::Int,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        };
                    } else {
                        return Type {
                            tag: Sig::ErrorType,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        };
                    }
                } else {
                    // if we do have a type, we will check if the number can be
                    // converted to that type. If it can, we will return that type.
                    // If it cannot, we will return an error type
                    if recv_ty.tag.is_numerical_type() {
                        match recv_ty.tag {
                            Sig::I8 => {
                                let val_i8 = val.parse::<i8>();
                                if let Ok(_) = val_i8 {
                                    return Type {
                                        tag: Sig::I8,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    };
                                } else {
                                    return Type {
                                        tag: Sig::ErrorType,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    };
                                }
                            }
                            Sig::I16 => {
                                let val_i16 = val.parse::<i16>();
                                if let Ok(_) = val_i16 {
                                    return Type {
                                        tag: Sig::I16,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    };
                                } else {
                                    return Type {
                                        tag: Sig::ErrorType,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    };
                                }
                            }
                            Sig::I32 => {
                                let val_i32 = val.parse::<i32>();
                                if let Ok(_) = val_i32 {
                                    return Type {
                                        tag: Sig::I32,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    };
                                } else {
                                    return Type {
                                        tag: Sig::ErrorType,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    };
                                }
                            }
                            Sig::I64 => {
                                let val_i64 = val.parse::<i64>();
                                if let Ok(_) = val_i64 {
                                    return Type {
                                        tag: Sig::I64,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    };
                                } else {
                                    return Type {
                                        tag: Sig::ErrorType,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    };
                                }
                            }
                            Sig::Int => {
                                let val_int = val.parse::<i64>();
                                if let Ok(_) = val_int {
                                    return Type {
                                        tag: Sig::Int,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    };
                                } else {
                                    return Type {
                                        tag: Sig::ErrorType,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    };
                                }
                            }
                            Sig::U8 => {
                                let val_u8 = val.parse::<u8>();
                                if let Ok(_) = val_u8 {
                                    return Type {
                                        tag: Sig::U8,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    };
                                } else {
                                    return Type {
                                        tag: Sig::ErrorType,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    };
                                }
                            }
                            Sig::U16 => {
                                let val_u16 = val.parse::<u16>();
                                if let Ok(_) = val_u16 {
                                    return Type {
                                        tag: Sig::U16,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    };
                                } else {
                                    return Type {
                                        tag: Sig::ErrorType,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    };
                                }
                            }
                            Sig::U32 => {
                                let val_u32 = val.parse::<u32>();
                                if let Ok(_) = val_u32 {
                                    return Type {
                                        tag: Sig::U32,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    };
                                } else {
                                    return Type {
                                        tag: Sig::ErrorType,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    };
                                }
                            }
                            Sig::U64 => {
                                let val_u64 = val.parse::<u64>();
                                if let Ok(_) = val_u64 {
                                    return Type {
                                        tag: Sig::U64,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    };
                                } else {
                                    return Type {
                                        tag: Sig::ErrorType,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    };
                                }
                            }
                            Sig::UInt => {
                                let val_uint = val.parse::<u64>();
                                if let Ok(_) = val_uint {
                                    return Type {
                                        tag: Sig::UInt,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    };
                                } else {
                                    return Type {
                                        tag: Sig::ErrorType,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    };
                                }
                            }
                            _ => {
                                return Type {
                                    tag: Sig::ErrorType,
                                    name: None,
                                    sub_types: vec![],
                                    aux_type: None,
                                    loc: loc.clone(),
                                }
                            }
                        }
                    } else {
                        return Type {
                            tag: Sig::ErrorType,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        };
                    }
                }
            }
            Expr::Char { loc, .. } => {
                // we already know the type of the expression
                return Type {
                    tag: Sig::Char,
                    name: None,
                    sub_types: vec![],
                    aux_type: None,
                    loc: loc.clone(),
                };
            }
            Expr::Str { loc, .. } => {
                // we already know the type of the expression
                return Type {
                    tag: Sig::Str,
                    name: None,
                    sub_types: vec![],
                    aux_type: None,
                    loc: loc.clone(),
                };
            }
            Expr::Bool { loc, .. } => {
                // we already know the type of the expression
                return Type {
                    tag: Sig::Bool,
                    name: None,
                    sub_types: vec![],
                    aux_type: None,
                    loc: loc.clone(),
                };
            }
            _ => todo!("Checker::check_expr: implement more cases"),
        }
    }

    pub fn check(&mut self) {
        self.collect_info();
    }
}
