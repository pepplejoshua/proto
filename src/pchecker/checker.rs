#![allow(dead_code)]
#![allow(unused_variables)]

use crate::{
    parser::pcode::{Expr, ExprLoc, Ins, PCode},
    source::{
        errors::CheckerError,
        source::{SourceFile, SourceReporter},
    },
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Pass {
    First,
    Second,
}

#[derive(Clone)]
pub struct Checker {
    pub pcode: PCode, // parsed code
    pub sym_table: SymbolTable,
    scope: CheckerScope,
    needs_next_pass: bool,
    reporter: SourceReporter,
    pass: Pass,
}

impl Checker {
    pub fn new(pcode: PCode, src: SourceFile) -> Checker {
        Checker {
            pcode,
            sym_table: SymbolTable::new(),
            scope: CheckerScope::Global,
            needs_next_pass: false,
            reporter: SourceReporter::new(src),
            pass: Pass::First,
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
                Ins::Comment { comment, loc } => {
                    // do nothing
                }
                Ins::NewConstant { name, ty, val, loc } => {
                    // check if the constant is already defined
                    if self.sym_table.check_name(&name) {
                        panic!("Checker::collect_info: constant {} already defined", name);
                    }

                    let mut needs_next_pass = false;
                    let mut sym_ty = ty.clone();
                    if !self.verify_type(&ty) {
                        // if we cannot verify the type given to the constant,
                        // we will leave it to the next pass
                        needs_next_pass = true;
                    } else {
                        // we can check the expression using our verified type
                        // to infer the type of the expression if needed
                        let expr_ty = self.check_expr(&val, &ty);
                        if !self.verify_type(&expr_ty) {
                            // if we cannot verify the type of the expression,
                            // we will leave it to the next pass as well
                            needs_next_pass = true;
                        } else {
                            // if we can verify the type of the expression,
                            // we can use the type given to the constant to typecheck
                            // the expression type. Infer type will accept any type
                            if ty.typecheck(&expr_ty) {
                                sym_ty = expr_ty;
                            } else {
                                // if the type of the expression does not match the type
                                // given to the constant, we can set the type of the constant to an
                                // error type
                                // we can report an error here as well
                                let span = ty.loc.clone();
                                let expr_span = expr_ty.loc.clone();
                                let span = span.combine(expr_span);
                                let err = CheckerError::TypeMismatch {
                                    loc: span,
                                    expected: ty.as_str(),
                                    found: expr_ty.as_str(),
                                };
                                self.reporter.report_checker_error(err);
                                sym_ty = Type {
                                    tag: Sig::ErrorType,
                                    name: None,
                                    sub_types: vec![],
                                    aux_type: None,
                                    loc: loc.clone(),
                                };
                                // report mismatch of types error
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
        let expr = self.pcode.get_expr_c(expr_i);

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
                        let err = CheckerError::NumberTypeDefaultInferenceFailed {
                            loc: loc.clone(),
                            number: val.clone(),
                        };
                        self.reporter.report_checker_error(err);
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
                                let ty = if let Ok(_) = val_i8 {
                                    Type {
                                        tag: Sig::I8,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    }
                                } else {
                                    let err = CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: recv_ty.as_str(),
                                    };
                                    self.reporter.report_checker_error(err);
                                    Type {
                                        tag: Sig::ErrorType,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    }
                                };
                                self.pcode.update_expr_type(expr_i, ty.clone());
                                return ty;
                            }
                            Sig::I16 => {
                                let val_i16 = val.parse::<i16>();
                                let ty = if let Ok(_) = val_i16 {
                                    Type {
                                        tag: Sig::I16,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    }
                                } else {
                                    let err = CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: recv_ty.as_str(),
                                    };
                                    self.reporter.report_checker_error(err);
                                    Type {
                                        tag: Sig::ErrorType,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    }
                                };
                                self.pcode.update_expr_type(expr_i, ty.clone());
                                return ty;
                            }
                            Sig::I32 => {
                                let val_i32 = val.parse::<i32>();
                                let ty = if let Ok(_) = val_i32 {
                                    Type {
                                        tag: Sig::I32,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    }
                                } else {
                                    let err = CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: recv_ty.as_str(),
                                    };
                                    self.reporter.report_checker_error(err);
                                    Type {
                                        tag: Sig::ErrorType,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    }
                                };
                                self.pcode.update_expr_type(expr_i, ty.clone());
                                return ty;
                            }
                            Sig::I64 => {
                                let val_i64 = val.parse::<i64>();
                                let ty = if let Ok(_) = val_i64 {
                                    Type {
                                        tag: Sig::I64,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    }
                                } else {
                                    // report error
                                    let err = CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: recv_ty.as_str(),
                                    };
                                    self.reporter.report_checker_error(err);
                                    Type {
                                        tag: Sig::ErrorType,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    }
                                };
                                self.pcode.update_expr_type(expr_i, ty.clone());
                                return ty;
                            }
                            Sig::Int => {
                                let val_int = val.parse::<i64>();
                                let ty = if let Ok(_) = val_int {
                                    Type {
                                        tag: Sig::Int,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    }
                                } else {
                                    let err = CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: recv_ty.as_str(),
                                    };
                                    self.reporter.report_checker_error(err);
                                    Type {
                                        tag: Sig::ErrorType,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    }
                                };
                                self.pcode.update_expr_type(expr_i, ty.clone());
                                return ty;
                            }
                            Sig::U8 => {
                                let val_u8 = val.parse::<u8>();
                                let ty = if let Ok(_) = val_u8 {
                                    Type {
                                        tag: Sig::U8,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    }
                                } else {
                                    let err = CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: recv_ty.as_str(),
                                    };
                                    self.reporter.report_checker_error(err);
                                    Type {
                                        tag: Sig::ErrorType,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    }
                                };
                                self.pcode.update_expr_type(expr_i, ty.clone());
                                return ty;
                            }
                            Sig::U16 => {
                                let val_u16 = val.parse::<u16>();
                                let ty = if let Ok(_) = val_u16 {
                                    Type {
                                        tag: Sig::U16,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    }
                                } else {
                                    let err = CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: recv_ty.as_str(),
                                    };
                                    self.reporter.report_checker_error(err);
                                    Type {
                                        tag: Sig::ErrorType,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    }
                                };
                                self.pcode.update_expr_type(expr_i, ty.clone());
                                return ty;
                            }
                            Sig::U32 => {
                                let val_u32 = val.parse::<u32>();
                                let ty = if let Ok(_) = val_u32 {
                                    Type {
                                        tag: Sig::U32,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    }
                                } else {
                                    let err = CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: recv_ty.as_str(),
                                    };
                                    self.reporter.report_checker_error(err);
                                    Type {
                                        tag: Sig::ErrorType,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    }
                                };
                                self.pcode.update_expr_type(expr_i, ty.clone());
                                return ty;
                            }
                            Sig::U64 => {
                                let val_u64 = val.parse::<u64>();
                                let ty = if let Ok(_) = val_u64 {
                                    Type {
                                        tag: Sig::U64,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    }
                                } else {
                                    let err = CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: recv_ty.as_str(),
                                    };
                                    self.reporter.report_checker_error(err);
                                    Type {
                                        tag: Sig::ErrorType,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    }
                                };
                                self.pcode.update_expr_type(expr_i, ty.clone());
                                return ty;
                            }
                            Sig::UInt => {
                                let val_uint = val.parse::<u64>();
                                let ty = if let Ok(_) = val_uint {
                                    Type {
                                        tag: Sig::UInt,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    }
                                } else {
                                    let err = CheckerError::NumberTypeInferenceFailed {
                                        loc: loc.clone(),
                                        number: val.clone(),
                                        given_type: recv_ty.as_str(),
                                    };
                                    self.reporter.report_checker_error(err);
                                    Type {
                                        tag: Sig::ErrorType,
                                        name: None,
                                        sub_types: vec![],
                                        aux_type: None,
                                        loc: loc.clone(),
                                    }
                                };
                                self.pcode.update_expr_type(expr_i, ty.clone());
                                return ty;
                            }
                            _ => {
                                unreachable!(
                                    "Checker::check_expr: unreachable code for numerical types"
                                );
                            }
                        }
                    } else {
                        // if the type given to the number is not a numerical type,
                        // we will return an error type
                        let err = CheckerError::NumberTypeInferenceFailed {
                            loc: loc.clone(),
                            number: val.clone(),
                            given_type: recv_ty.as_str(),
                        };
                        self.reporter.report_checker_error(err);
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
            Expr::Void { loc } => {
                // we already know the type of the expression
                return Type {
                    tag: Sig::Void,
                    name: None,
                    sub_types: vec![],
                    aux_type: None,
                    loc: loc.clone(),
                };
            }
            Expr::Ident { name, loc, ty } => {
                // check if the identifier is in the symbol table
                if self.sym_table.check_name(&name) {
                    // if it does, we will return the type of the identifier
                    let ty = self.sym_table.get(&name).unwrap().clone();
                    ty
                } else {
                    // if we are in the first pass, we will return an infer type
                    if matches!(self.pass, Pass::First) {
                        return Type {
                            tag: Sig::Infer,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        };
                    } else {
                        // we are in the second pass where we should know about this identifier
                        // if the identifier is not in the symbol table, we will return an error type
                        // and report an error
                        let err = CheckerError::ReferenceToUndefinedName {
                            loc: loc.clone(),
                            var_name: name.clone(),
                        };
                        self.reporter.report_checker_error(err);
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
            Expr::Add { lhs, rhs, loc, ty } => {
                // we will check the types of the left and right hand side of the expression
                let lhs_ty = self.check_expr(&lhs, recv_ty);
                let rhs_ty = self.check_expr(&rhs, recv_ty);
                // if the types of the left and right hand side of the expression are not numerical types,
                // we will return an error type
                match (lhs_ty.tag, rhs_ty.tag) {
                    (l_tag, r_tag) if l_tag.is_numerical_type() && l_tag == r_tag => {
                        // if the types of the left and right hand side of the expression are numerical types,
                        // we will return the type of the left hand side of the expression
                        Type {
                            tag: l_tag,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                    (Sig::Str, Sig::Char) => {
                        // if the left hand side of the expression is a string and the right hand side of the expression
                        // is a char, we will return the type of the left hand side of the expression
                        Type {
                            tag: Sig::Str,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                    (Sig::Str, Sig::Str) => {
                        // if the left hand side of the expression is a string and the right hand side of the expression
                        // is a string, we will return the type of the left hand side of the expression
                        Type {
                            tag: Sig::Str,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                    (Sig::Char, Sig::Char) => {
                        // if the left hand side of the expression is a char and the right hand side of the expression
                        // is a char, we will return the type of the left hand side of the expression
                        Type {
                            tag: Sig::Char,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                    (l_type, r_type) if l_type.is_infer_type() || r_type.is_infer_type() => {
                        // if the types of the left and right hand side of the expression are infer types,
                        // we will return an infer type
                        Type {
                            tag: Sig::Infer,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                    _ => {
                        // if the types of the left and right hand side of the expression are not numerical types,
                        // we will return an error type
                        let err = CheckerError::InvalidUseOfBinaryOperator {
                            loc: loc.clone(),
                            op: "+".to_string(),
                            left: lhs_ty.as_str(),
                            right: rhs_ty.as_str(),
                        };
                        self.reporter.report_checker_error(err);
                        Type {
                            tag: Sig::ErrorType,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                }
            }
            _ => todo!("Checker::check_expr: implement more cases"),
        }
    }

    pub fn check(&mut self) {
        self.collect_info();
    }
}
