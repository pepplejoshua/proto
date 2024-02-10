#![allow(dead_code)]
#![allow(unused_variables)]

use crate::{
    parser::pcode::{Expr, ExprLoc, Ins, InsLoc, PCode},
    source::{
        errors::CheckerError,
        source::{SourceFile, SourceReporter},
    },
    symbol_info::symbol_info::{SymbolInfo, SymbolTable, SymbolTableType},
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
    One,
    Two,
}

#[derive(Clone)]
pub struct Checker {
    pub pcode: PCode, // parsed code
    pub sym_table: SymbolTable,
    scope: CheckerScope,
    needs_next_pass: bool,
    reporter: SourceReporter,
    error_count: u8,
    pass: Pass,
}

impl Checker {
    pub fn new(pcode: PCode, src: SourceFile) -> Checker {
        Checker {
            pcode,
            sym_table: SymbolTable::new(SymbolTableType::Preserved),
            scope: CheckerScope::Global,
            needs_next_pass: false,
            reporter: SourceReporter::new(src),
            pass: Pass::One,
            error_count: 0,
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

    fn pass_1_check_ins(&mut self, ins_i: InsLoc) {
        let stmt = self.pcode.get_ins_c(&ins_i);
        match stmt {
            Ins::Comment { .. } => {
                // do nothing
            }
            Ins::NewConstant { name, ty, val, loc } => {
                let const_exists = self.sym_table.check_name(&name);
                let type_is_valid = self.verify_type(&ty);

                if const_exists {
                    let err = CheckerError::NameAlreadyDefined {
                        loc: loc.clone(),
                        name: name.clone(),
                    };
                    self.report_error(err);
                    return;
                }

                if !type_is_valid {
                    let mut info = SymbolInfo::new_const_info();
                    info.set_def_location(loc.clone());
                    info.fully_initialized = false;

                    self.sym_table.register(
                        name.clone(),
                        Type {
                            tag: Sig::Infer,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: ty.loc.clone(),
                        },
                        info,
                    );
                    self.needs_next_pass = true;
                    return;
                }

                let expr_ty = self.check_expr(&val, &ty);
                let expr_ty_is_valid = self.verify_type(&expr_ty) && !expr_ty.tag.is_error_type();
                if !expr_ty_is_valid || expr_ty.is_infer_type() {
                    let mut info = SymbolInfo::new_const_info();
                    info.set_def_location(loc.clone());
                    info.fully_initialized = false;

                    self.sym_table.register(name.clone(), expr_ty, info);
                    self.needs_next_pass = true;
                    return;
                }

                if !ty.typecheck(&expr_ty) {
                    let span = ty.loc.clone();
                    let expr_span = expr_ty.loc.clone();
                    let span = span.combine(expr_span);
                    let err = CheckerError::TypeMismatch {
                        loc: span,
                        expected: ty.as_str(),
                        found: expr_ty.as_str(),
                    };
                    self.report_error(err);

                    let mut info = SymbolInfo::new_const_info();
                    info.set_def_location(loc.clone());
                    info.fully_initialized = false;
                    let const_ty = Type {
                        tag: Sig::ErrorType,
                        name: None,
                        sub_types: vec![],
                        aux_type: None,
                        loc: loc.clone(),
                    };
                    self.sym_table.register(name.clone(), const_ty, info);
                    return;
                }

                let mut info = SymbolInfo::new_const_info();
                info.set_def_location(loc.clone());
                info.fully_initialized = true;
                self.sym_table.register(name.clone(), expr_ty, info);
            }
            Ins::NewVariable { name, ty, val, loc } => {
                let var_exists = self.sym_table.check_name(&name);
                let type_is_valid = self.verify_type(&ty);

                if var_exists {
                    let err = CheckerError::NameAlreadyDefined {
                        loc: loc.clone(),
                        name: name.clone(),
                    };
                    self.report_error(err);
                    return;
                }

                if !type_is_valid {
                    let mut info = SymbolInfo::new_var_info();
                    info.set_def_location(loc.clone());
                    info.fully_initialized = false;

                    self.sym_table.register(
                        name.clone(),
                        Type {
                            tag: Sig::Infer,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: ty.loc.clone(),
                        },
                        info,
                    );
                    self.needs_next_pass = true;
                    return;
                }

                // sometimes, u might want to declare a variable and initialize
                // it later, so we don't need to check the value here
                let mut _var_type = ty.clone();
                if let Some(val) = val {
                    let expr_ty = self.check_expr(&val, &ty);
                    let expr_ty_is_valid =
                        self.verify_type(&expr_ty) && !expr_ty.tag.is_error_type();
                    if !expr_ty_is_valid || expr_ty.is_infer_type() {
                        let mut info = SymbolInfo::new_var_info();
                        info.set_def_location(loc.clone());
                        info.fully_initialized = false;

                        self.sym_table.register(name.clone(), expr_ty, info);
                        self.needs_next_pass = true;
                        return;
                    }

                    if !ty.typecheck(&expr_ty) {
                        let span = ty.loc.clone();
                        let expr_span = expr_ty.loc.clone();
                        let span = span.combine(expr_span);
                        let err = CheckerError::TypeMismatch {
                            loc: span,
                            expected: ty.as_str(),
                            found: expr_ty.as_str(),
                        };
                        self.report_error(err);

                        let mut info = SymbolInfo::new_var_info();
                        info.set_def_location(loc.clone());
                        info.fully_initialized = false;
                        let var_ty = Type {
                            tag: Sig::ErrorType,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        };
                        self.sym_table.register(name.clone(), var_ty, info);
                        return;
                    } else {
                        _var_type = expr_ty;
                    }
                } else {
                    let mut info = SymbolInfo::new_var_info();
                    info.set_def_location(loc.clone());
                    info.fully_initialized = false;

                    self.sym_table.register(name.clone(), ty.clone(), info);
                    return;
                }

                let mut info = SymbolInfo::new_var_info();
                info.set_def_location(loc.clone());
                info.fully_initialized = true;
                self.sym_table.register(name.clone(), _var_type, info);
            }
            _ => todo!(
                "Checker::pass_1_check_ins: implement checking for {:?} :> {:?}",
                ins_i,
                stmt
            ),
        }
    }

    fn pass_1(&mut self) {
        let top_level = self.pcode.top_level.clone();

        for (loc, _) in top_level.iter().enumerate() {
            self.pass_1_check_ins((0, loc));
        }
    }

    fn pass_2_check_ins(&mut self, ins_i: InsLoc) {
        let stmt = self.pcode.get_ins_c(&ins_i);
        match stmt {
            Ins::Comment { .. } => {
                // do nothing
            }
            Ins::NewConstant { name, ty, val, loc } => {
                let in_local_table = self.sym_table.is_locals_table();

                // in a local table, we should not have any constants already defined / declared
                // because we are in a new scope. if there is a constant already defined, it is
                // an error which we already reported in pass_1
                let const_exists = self.sym_table.name_is_const(&name);
                if in_local_table && const_exists {
                    return;
                }

                if in_local_table {
                    // we can treat it normally and process it
                    // because we are in a new scope and we don't have any
                    // constants already defined with same name

                    let type_is_valid = self.verify_type(&ty);
                    if !type_is_valid {
                        // now we have to throw an error since given all the information
                        // we have, we still cannot resolve this type
                        let err = CheckerError::InvalidType {
                            loc: ty.loc.clone(),
                            type_name: ty.as_str(),
                        };
                        self.report_error(err);
                        return;
                    }

                    let expr_ty = self.check_expr(&val, &ty);
                    let expr_ty_is_valid =
                        self.verify_type(&expr_ty) && !expr_ty.tag.is_error_type();
                    if !expr_ty_is_valid {
                        // now we have to throw an error since given all the information
                        // we have, we still cannot resolve this type
                        let err = CheckerError::InvalidType {
                            loc: expr_ty.loc.clone(),
                            type_name: expr_ty.as_str(),
                        };
                        self.report_error(err);
                        return;
                    }

                    if !ty.typecheck(&expr_ty) {
                        let span = ty.loc.clone();
                        let expr_span = expr_ty.loc.clone();
                        let span = span.combine(expr_span);
                        let err = CheckerError::TypeMismatch {
                            loc: span,
                            expected: ty.as_str(),
                            found: expr_ty.as_str(),
                        };
                        self.report_error(err);
                        return;
                    }

                    let mut info = SymbolInfo::new_const_info();
                    info.set_def_location(loc.clone());
                    self.sym_table.register(name.clone(), expr_ty, info);
                    return;
                } else {
                    // now we are in a Preserved or SelfContained table
                    // so we should have seen this constant before. We will
                    // get what info we have on it and then decide whether it needs
                    // further processing or not
                    let const_info = self.sym_table.get_info(&name);
                    match const_info {
                        Some(info) => {
                            if info.fully_initialized {
                                // we are done with this constant
                                return;
                            }

                            // we still have to process it if its type is
                            // "Infer" and not "Error"
                            let sym_ty = self.sym_table.get_type(&name).unwrap();
                            match sym_ty.tag {
                                Sig::ErrorType => {
                                    // we are done with this constant
                                    return;
                                }
                                Sig::Infer => {
                                    // now we can focus on the expression type and the type assigned to the
                                    // constant, if any. If the type is not assigned, we will assign the type
                                    // of the expression to the constant, else we will typecheck with the
                                    // assigned value
                                    let type_is_valid = self.verify_type(&ty);

                                    if !type_is_valid {
                                        // now we have to throw an error since given all the information
                                        // we have, we still cannot resolve this type
                                        let err = CheckerError::InvalidType {
                                            loc: ty.loc.clone(),
                                            type_name: ty.as_str(),
                                        };
                                        self.report_error(err);

                                        // set the type of the constant to error
                                        // so that we don't process it again
                                        let err_ty = Type {
                                            tag: Sig::ErrorType,
                                            loc: ty.loc.clone(),
                                            name: None,
                                            sub_types: vec![],
                                            aux_type: None,
                                        };
                                        self.sym_table.update_type(&name, err_ty, true);
                                        return;
                                    }

                                    let expr_ty = self.check_expr(&val, &ty);
                                    let expr_ty_is_valid = self.verify_type(&expr_ty);
                                    let expr_ty_is_error = expr_ty.tag.is_error_type();
                                    if !expr_ty_is_valid {
                                        if !expr_ty_is_error {
                                            // now we have to throw an error since given all the information
                                            // we have, we still cannot resolve this type
                                            let err = CheckerError::InvalidType {
                                                loc: expr_ty.loc.clone(),
                                                type_name: expr_ty.as_str(),
                                            };
                                            self.report_error(err);
                                        }
                                        // set the type of the constant to error
                                        let err_ty = Type {
                                            tag: Sig::ErrorType,
                                            loc: ty.loc.clone(),
                                            name: None,
                                            sub_types: vec![],
                                            aux_type: None,
                                        };
                                        self.sym_table.update_type(&name, err_ty, true);
                                        return;
                                    }

                                    if !ty.typecheck(&expr_ty) {
                                        let span = ty.loc.clone();
                                        let expr_span = expr_ty.loc.clone();
                                        let span = span.combine(expr_span);
                                        let err = CheckerError::TypeMismatch {
                                            loc: span,
                                            expected: ty.as_str(),
                                            found: expr_ty.as_str(),
                                        };
                                        self.report_error(err);

                                        // set the type of the constant to error
                                        let err_ty = Type {
                                            tag: Sig::ErrorType,
                                            loc: ty.loc.clone(),
                                            name: None,
                                            sub_types: vec![],
                                            aux_type: None,
                                        };
                                        self.sym_table.update_type(&name, err_ty, true);
                                        return;
                                    }

                                    self.sym_table.update_type(&name, expr_ty, true);
                                }
                                _ => {
                                    unreachable!("Checker::pass_2_check_ins: constant type is not Infer or Error");
                                }
                            }
                        }
                        None => {
                            unreachable!("Checker::pass_2_check_ins: constant not found in table");
                        }
                    }
                }
            }
            Ins::NewVariable { name, ty, val, loc } => {
                let in_local_table = self.sym_table.is_locals_table();

                // in a local table, we should not have any variables already defined / declared
                // with the same name. If we do, it is an error which we already reported in pass_1
                // so we can safely ignore it here
                let var_exists = self.sym_table.name_is_var(&name);
                if in_local_table && var_exists {
                    return;
                }

                if in_local_table {
                    // we can treat it normally and process it
                    // because we are in a new scope and we don't have any
                    // variables already defined with same name
                    let type_is_valid = self.verify_type(&ty);
                    if !type_is_valid {
                        // now we have to throw an error since given all the information
                        // we have, we still cannot resolve this type
                        let err = CheckerError::InvalidType {
                            loc: ty.loc.clone(),
                            type_name: ty.as_str(),
                        };
                        self.report_error(err);
                        return;
                    }

                    // sometimes a variable will be declared without an init value
                    if let Some(val) = val {
                        let expr_ty = self.check_expr(&val, &ty);
                        let expr_ty_is_valid = self.verify_type(&expr_ty);
                        let expr_ty_is_error = expr_ty.tag.is_error_type();
                        if !expr_ty_is_valid {
                            if !expr_ty_is_error {
                                // now we have to throw an error since given all the information
                                // we have, we still cannot resolve this type
                                let err = CheckerError::InvalidType {
                                    loc: expr_ty.loc.clone(),
                                    type_name: expr_ty.as_str(),
                                };
                                self.report_error(err);
                            }
                            // we have to assign an error type to the variable
                            let err_ty = Type {
                                tag: Sig::ErrorType,
                                loc: ty.loc.clone(),
                                name: None,
                                sub_types: vec![],
                                aux_type: None,
                            };
                            let mut info = SymbolInfo::new_var_info();
                            info.set_def_location(loc.clone());
                            self.sym_table.register(name.clone(), err_ty, info);
                            return;
                        }

                        if !ty.typecheck(&expr_ty) {
                            let span = ty.loc.clone();
                            let expr_span = expr_ty.loc.clone();
                            let span = span.combine(expr_span);
                            let err = CheckerError::TypeMismatch {
                                loc: span,
                                expected: ty.as_str(),
                                found: expr_ty.as_str(),
                            };
                            self.report_error(err);

                            // we have to assign an error type to the variable
                            let err_ty = Type {
                                tag: Sig::ErrorType,
                                loc: ty.loc.clone(),
                                name: None,
                                sub_types: vec![],
                                aux_type: None,
                            };
                            let mut info = SymbolInfo::new_var_info();
                            info.set_def_location(loc.clone());
                            self.sym_table.register(name.clone(), err_ty, info);
                            return;
                        }

                        let mut info = SymbolInfo::new_var_info();
                        info.set_def_location(loc.clone());
                        self.sym_table.register(name.clone(), ty.clone(), info);
                        return;
                    } else {
                        // we have to assign the type to the variable although it is not
                        // initialized yet
                        let mut info = SymbolInfo::new_var_info();
                        info.fully_initialized = false;
                        info.set_def_location(loc.clone());
                        self.sym_table.register(name.clone(), ty.clone(), info);
                        return;
                    }
                } else {
                    // since we are in a Preserved or SelfContained table so we should
                    // have seen this variable before. We will get what info we have on it and
                    // then decide whether it needs further processing or not
                    let var_info = self.sym_table.get_info(&name);
                    match var_info {
                        Some(info) => {
                            if info.fully_initialized {
                                // we have already processed this variable
                                return;
                            }

                            let sym_ty = self.sym_table.get_type(&name).unwrap();
                            match sym_ty.tag {
                                Sig::ErrorType => {
                                    // we have already processed this variable
                                    return;
                                }
                                Sig::Infer => {
                                    // now we can focus on the expression type (if any) and
                                    // the type assigned to the variable, if any. If we have
                                    // no expression, we can just assign the type to the variable
                                    // (without marking it as initialized) and if we have an expression,
                                    // we can check it against the type if any and assign its type to
                                    // the variable
                                    let type_is_valid = self.verify_type(&ty);

                                    if !type_is_valid {
                                        // now we have to throw an error since given all the information
                                        // we have, we still cannot resolve this type
                                        let err = CheckerError::InvalidType {
                                            loc: ty.loc.clone(),
                                            type_name: ty.as_str(),
                                        };
                                        self.report_error(err);

                                        // we have to update the variable'stype to an error type
                                        // and mark it as fully initialized
                                        let err_ty = Type {
                                            tag: Sig::ErrorType,
                                            loc: ty.loc.clone(),
                                            name: None,
                                            sub_types: vec![],
                                            aux_type: None,
                                        };
                                        self.sym_table.update_type(&name, err_ty, true);
                                        return;
                                    }

                                    // we have to handle the init expression if there is any
                                    if let Some(val) = val {
                                        let expr_ty = self.check_expr(&val, &ty);
                                        let expr_ty_is_valid = self.verify_type(&expr_ty);
                                        let expr_ty_is_error = expr_ty.tag.is_error_type();

                                        if !expr_ty_is_valid {
                                            if !expr_ty_is_error {
                                                // now we have to throw an error since given all the information
                                                // we have, we still cannot resolve this type
                                                let err = CheckerError::InvalidType {
                                                    loc: expr_ty.loc.clone(),
                                                    type_name: expr_ty.as_str(),
                                                };
                                                self.report_error(err);
                                            }
                                            // we have to assign an error type to the variable
                                            let err_ty = Type {
                                                tag: Sig::ErrorType,
                                                loc: ty.loc.clone(),
                                                name: None,
                                                sub_types: vec![],
                                                aux_type: None,
                                            };
                                            self.sym_table.update_type(&name, err_ty, true);
                                            return;
                                        }

                                        if !ty.typecheck(&expr_ty) {
                                            let span = ty.loc.clone();
                                            let expr_span = expr_ty.loc.clone();
                                            let span = span.combine(expr_span);
                                            let err = CheckerError::TypeMismatch {
                                                loc: span,
                                                expected: ty.as_str(),
                                                found: expr_ty.as_str(),
                                            };
                                            self.report_error(err);

                                            // we have to assign an error type to the variable
                                            let err_ty = Type {
                                                tag: Sig::ErrorType,
                                                loc: ty.loc.clone(),
                                                name: None,
                                                sub_types: vec![],
                                                aux_type: None,
                                            };
                                            self.sym_table.update_type(&name, err_ty, true);
                                            return;
                                        }

                                        // since the expression type is valid and it matches the type
                                        // assigned to the variable, we can assign the type to the variable
                                        // and mark it as fully initialized
                                        self.sym_table.update_type(&name, expr_ty, true);
                                        return;
                                    } else {
                                        // we can assign the type to the variable without marking it as
                                        // fully initialized
                                        self.sym_table.update_type(&name, ty, false);
                                        return;
                                    }
                                }
                                _ => {
                                    // we can confirm that even though the variable is not fully initialized,
                                    // it has a valid type assigned to it (and it really has no expression to init it)
                                    if val.is_none() {
                                        return;
                                    } else {
                                        unreachable!("Checker::pass_2_check_ins: variable type is not Infer or Error and it has an expression to init it");
                                    }
                                }
                            }
                        }
                        None => {
                            unreachable!("Checker::pass_2_check_ins: variable not found in table");
                        }
                    }
                }
            }
            _ => todo!(
                "Checker::pass_2_check_ins: implement checking for {:?} :> {:?}",
                ins_i,
                stmt
            ),
        }
    }

    fn pass_2(&mut self) {
        let top_level = self.pcode.top_level.clone();
        for (loc, _) in top_level.iter().enumerate() {
            self.pass_2_check_ins((0, loc));
        }
    }

    fn report_error(&mut self, err: CheckerError) {
        self.reporter.report_checker_error(err);
        self.error_count += 1;

        if self.error_count >= 10 {
            let err = CheckerError::TooManyErrors;
            self.reporter.report_checker_error(err);
            std::process::exit(1);
        }
    }

    fn check_expr(&mut self, expr_i: &ExprLoc, recv_ty: &Type) -> Type {
        let expr = self.pcode.get_expr_c(expr_i);

        match expr {
            Expr::Number { val, loc, .. } => {
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
                        self.report_error(err);

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
                                    self.report_error(err);
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
                                    self.report_error(err);
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
                                    self.report_error(err);
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
                                    self.report_error(err);
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
                                    self.report_error(err);
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
                                    self.report_error(err);
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
                                    self.report_error(err);
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
                                    self.report_error(err);
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
                                    self.report_error(err);
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
                                    self.report_error(err);
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
                        self.report_error(err);
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
            Expr::Ident { name, loc, .. } => {
                // check if the identifier is in the symbol table
                if self.sym_table.check_name(&name) {
                    // if it does, we will return the type of the identifier
                    let ty = self.sym_table.get_type(&name).unwrap().clone();
                    // TODO(@pepplejoshua):
                    // figure out a way to allow the user decide if this is import for them
                    // in their current file and toggle it on. maybe with a directive
                    // self.sym_table.update_uses(&name, loc.clone());
                    self.pcode.update_expr_type(expr_i, ty.clone());
                    ty
                } else {
                    // if we are in the first pass, we will return an infer type
                    if matches!(self.pass, Pass::One) {
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
                        self.report_error(err);
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
                let ty = match (lhs_ty.tag, rhs_ty.tag) {
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
                        self.report_error(err);
                        Type {
                            tag: Sig::ErrorType,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                };
                self.pcode.update_expr_type(expr_i, ty.clone());
                ty
            }
            Expr::Sub { lhs, rhs, loc, .. } => {
                // we will check the types of the left and right hand side of the expression
                let lhs_ty = self.check_expr(&lhs, recv_ty);
                let rhs_ty = self.check_expr(&rhs, recv_ty);
                // if the types of the left and right hand side of the expression are not numerical types,
                // we will return an error type
                let ty = match (lhs_ty.tag, rhs_ty.tag) {
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
                            op: "-".to_string(),
                            left: lhs_ty.as_str(),
                            right: rhs_ty.as_str(),
                        };
                        self.report_error(err);
                        Type {
                            tag: Sig::ErrorType,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                };
                self.pcode.update_expr_type(expr_i, ty.clone());
                ty
            }
            Expr::Mul { lhs, rhs, loc, .. } => {
                // we will check the types of the left and right hand side of the expression
                let lhs_ty = self.check_expr(&lhs, recv_ty);
                let rhs_ty = self.check_expr(&rhs, recv_ty);

                // if the types of the left and right hand side of the expression are not numerical types,
                // we will return an error type
                let ty = match (lhs_ty.tag, rhs_ty.tag) {
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
                            op: "*".to_string(),
                            left: lhs_ty.as_str(),
                            right: rhs_ty.as_str(),
                        };
                        self.report_error(err);
                        Type {
                            tag: Sig::ErrorType,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                };
                self.pcode.update_expr_type(expr_i, ty.clone());
                ty
            }
            Expr::Div { lhs, rhs, loc, .. } => {
                // we will check the types of the left and right hand side of the expression
                let lhs_ty = self.check_expr(&lhs, recv_ty);
                let rhs_ty = self.check_expr(&rhs, recv_ty);

                // if the types of the left and right hand side of the expression are not numerical types,
                // we will return an error type
                let ty = match (lhs_ty.tag, rhs_ty.tag) {
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
                            op: "/".to_string(),
                            left: lhs_ty.as_str(),
                            right: rhs_ty.as_str(),
                        };
                        self.report_error(err);
                        Type {
                            tag: Sig::ErrorType,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                };
                self.pcode.update_expr_type(expr_i, ty.clone());
                ty
            }
            Expr::Mod { lhs, rhs, loc, .. } => {
                // we will check the types of the left and right hand side of the expression
                let lhs_ty = self.check_expr(&lhs, recv_ty);
                let rhs_ty = self.check_expr(&rhs, recv_ty);

                // if the types of the left and right hand side of the expression are not numerical types,
                // we will return an error type
                let ty = match (lhs_ty.tag, rhs_ty.tag) {
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
                            op: "%".to_string(),
                            left: lhs_ty.as_str(),
                            right: rhs_ty.as_str(),
                        };
                        self.report_error(err);
                        Type {
                            tag: Sig::ErrorType,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                };
                self.pcode.update_expr_type(expr_i, ty.clone());
                ty
            }
            Expr::And { lhs, rhs, loc, .. } => {
                // we will check the types of the left and right hand side of the expression
                let lhs_ty = self.check_expr(&lhs, recv_ty);
                let rhs_ty = self.check_expr(&rhs, recv_ty);

                // if the types of the left and right hand side of the expression are not boolean types,
                // we will return an error type
                let ty = match (lhs_ty.tag, rhs_ty.tag) {
                    (Sig::Bool, Sig::Bool) => {
                        // if the types of the left and right hand side of the expression are boolean types,
                        // we will return the type of the left hand side of the expression
                        Type {
                            tag: Sig::Bool,
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
                        // if the types of the left and right hand side of the expression are not boolean types,
                        // we will return an error type
                        let err = CheckerError::InvalidUseOfBinaryOperator {
                            loc: loc.clone(),
                            op: "&&".to_string(),
                            left: lhs_ty.as_str(),
                            right: rhs_ty.as_str(),
                        };
                        self.report_error(err);
                        Type {
                            tag: Sig::ErrorType,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                };
                self.pcode.update_expr_type(expr_i, ty.clone());
                ty
            }
            Expr::Or { lhs, rhs, loc, .. } => {
                // we will check the types of the left and right hand side of the expression
                let lhs_ty = self.check_expr(&lhs, recv_ty);
                let rhs_ty = self.check_expr(&rhs, recv_ty);

                // if the types of the left and right hand side of the expression are not boolean types,
                // we will return an error type
                let ty = match (lhs_ty.tag, rhs_ty.tag) {
                    (Sig::Bool, Sig::Bool) => {
                        // if the types of the left and right hand side of the expression are boolean types,
                        // we will return the type of the left hand side of the expression
                        Type {
                            tag: Sig::Bool,
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
                        // if the types of the left and right hand side of the expression are not boolean types,
                        // we will return an error type
                        let err = CheckerError::InvalidUseOfBinaryOperator {
                            loc: loc.clone(),
                            op: "||".to_string(),
                            left: lhs_ty.as_str(),
                            right: rhs_ty.as_str(),
                        };
                        self.report_error(err);
                        Type {
                            tag: Sig::ErrorType,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                };
                self.pcode.update_expr_type(expr_i, ty.clone());
                ty
            }
            Expr::Not { expr, loc, .. } => {
                // we will check the type of the expression
                let expr_ty = self.check_expr(&expr, recv_ty);

                // if the type of the expression is not a boolean type,
                // we will return an error type
                let ty = match expr_ty.tag {
                    Sig::Bool => {
                        // if the type of the expression is a boolean type,
                        // we will return the type of the expression
                        Type {
                            tag: Sig::Bool,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                    Sig::Infer => {
                        // if the type of the expression is an infer type,
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
                        // if the type of the expression is not a boolean type,
                        // we will return an error type
                        let err = CheckerError::InvalidUseOfUnaryOperator {
                            loc: loc.clone(),
                            op: "!".to_string(),
                            operand: expr_ty.as_str(),
                            tip: None,
                        };
                        self.report_error(err);
                        Type {
                            tag: Sig::ErrorType,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                };
                self.pcode.update_expr_type(expr_i, ty.clone());
                ty
            }
            Expr::Negate { expr, loc, .. } => {
                // we will check the type of the expression
                let expr_ty = self.check_expr(&expr, recv_ty);

                // if the type of the expression is not a numerical type,
                // we will return an error type
                let ty = match expr_ty.tag {
                    l_tag if l_tag.is_signed_type() => {
                        // if the type of the expression is a signed type,
                        // we will return the type of the expression
                        Type {
                            tag: l_tag,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                    l_tag if l_tag.is_unsigned_type() => {
                        // if the type of the expression is an unsigned type,
                        // we cannot negate it, since we cannot guarantee it won't overflow
                        // so we can hint the user that this operation is not allowed
                        let tip =
                            "Consider converting the value to a signed type before negating it"
                                .to_string();
                        let err = CheckerError::InvalidUseOfUnaryOperator {
                            loc: loc.clone(),
                            op: "-".to_string(),
                            operand: expr_ty.as_str(),
                            tip: Some(tip),
                        };
                        self.report_error(err);
                        Type {
                            tag: Sig::ErrorType,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                    Sig::Infer => {
                        // if the type of the expression is an infer type,
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
                        // if the type of the expression is not a numerical type,
                        // we will return an error type
                        let err = CheckerError::InvalidUseOfUnaryOperator {
                            loc: loc.clone(),
                            op: "-".to_string(),
                            operand: expr_ty.as_str(),
                            tip: None,
                        };
                        self.report_error(err);
                        Type {
                            tag: Sig::ErrorType,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                };
                self.pcode.update_expr_type(expr_i, ty.clone());
                ty
            }
            Expr::Eq { lhs, rhs, loc, .. } => {
                // we will check the types of the left and right hand side of the expression
                let lhs_ty = self.check_expr(&lhs, recv_ty);
                let rhs_ty = self.check_expr(&rhs, recv_ty);

                // if the types of the left and right hand side of the expression are not the same,
                // we will return an error type
                let ty = match (lhs_ty.tag, rhs_ty.tag) {
                    (l_tag, r_tag) if l_tag.is_simple_type() && l_tag == r_tag => {
                        // if the types of the left and right hand side of the expression are the same,
                        // we will return a boolean type
                        Type {
                            tag: Sig::Bool,
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
                        // if the types of the left and right hand side of the expression are not the same,
                        // we will return an error type
                        let err = CheckerError::InvalidUseOfBinaryOperator {
                            loc: loc.clone(),
                            op: "==".to_string(),
                            left: lhs_ty.as_str(),
                            right: rhs_ty.as_str(),
                        };
                        self.report_error(err);
                        Type {
                            tag: Sig::ErrorType,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                };
                self.pcode.update_expr_type(expr_i, ty.clone());
                ty
            }
            Expr::Neq { lhs, rhs, loc, .. } => {
                // we will check the types of the left and right hand side of the expression
                let lhs_ty = self.check_expr(&lhs, recv_ty);
                let rhs_ty = self.check_expr(&rhs, recv_ty);

                // if the types of the left and right hand side of the expression are not the same,
                // we will return an error type
                let ty = match (lhs_ty.tag, rhs_ty.tag) {
                    (l_tag, r_tag) if l_tag.is_simple_type() && l_tag == r_tag => {
                        // if the types of the left and right hand side of the expression are the same,
                        // we will return a boolean type
                        Type {
                            tag: Sig::Bool,
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
                        // if the types of the left and right hand side of the expression are not the same,
                        // we will return an error type
                        let err = CheckerError::InvalidUseOfBinaryOperator {
                            loc: loc.clone(),
                            op: "!=".to_string(),
                            left: lhs_ty.as_str(),
                            right: rhs_ty.as_str(),
                        };
                        self.report_error(err);
                        Type {
                            tag: Sig::ErrorType,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                };
                self.pcode.update_expr_type(expr_i, ty.clone());
                ty
            }
            Expr::Gt { lhs, rhs, loc, .. } => {
                // we will check the types of the left and right hand side of the expression
                let lhs_ty = self.check_expr(&lhs, recv_ty);
                let rhs_ty = self.check_expr(&rhs, recv_ty);

                // if the types of the left and right hand side of the expression are not numerical types,
                // we will return an error type
                let ty = match (lhs_ty.tag, rhs_ty.tag) {
                    (l_tag, r_tag) if l_tag.is_numerical_type() && l_tag == r_tag => {
                        // if the types of the left and right hand side of the expression are numerical types,
                        // we will return a boolean type
                        Type {
                            tag: Sig::Bool,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                    (Sig::Char, Sig::Char) => {
                        // if the left hand side of the expression is a char and the right hand side of the expression
                        // is a char, we will return a boolean type
                        Type {
                            tag: Sig::Bool,
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
                            op: ">".to_string(),
                            left: lhs_ty.as_str(),
                            right: rhs_ty.as_str(),
                        };
                        self.report_error(err);
                        Type {
                            tag: Sig::ErrorType,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                };
                self.pcode.update_expr_type(expr_i, ty.clone());
                ty
            }
            Expr::Lt { lhs, rhs, loc, .. } => {
                // we will check the types of the left and right hand side of the expression
                let lhs_ty = self.check_expr(&lhs, recv_ty);
                let rhs_ty = self.check_expr(&rhs, recv_ty);

                // if the types of the left and right hand side of the expression are not numerical types,
                // we will return an error type
                let ty = match (lhs_ty.tag, rhs_ty.tag) {
                    (l_tag, r_tag) if l_tag.is_numerical_type() && l_tag == r_tag => {
                        // if the types of the left and right hand side of the expression are numerical types,
                        // we will return a boolean type
                        Type {
                            tag: Sig::Bool,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                    (Sig::Char, Sig::Char) => {
                        // if the left hand side of the expression is a char and the right hand side of the expression
                        // is a char, we will return a boolean type
                        Type {
                            tag: Sig::Bool,
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
                            op: "<".to_string(),
                            left: lhs_ty.as_str(),
                            right: rhs_ty.as_str(),
                        };
                        self.report_error(err);
                        Type {
                            tag: Sig::ErrorType,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                };
                self.pcode.update_expr_type(expr_i, ty.clone());
                ty
            }
            Expr::GtEq { lhs, rhs, loc, .. } => {
                // we will check the types of the left and right hand side of the expression
                let lhs_ty = self.check_expr(&lhs, recv_ty);
                let rhs_ty = self.check_expr(&rhs, recv_ty);

                // if the types of the left and right hand side of the expression are not numerical types,
                // we will return an error type
                let ty = match (lhs_ty.tag, rhs_ty.tag) {
                    (l_tag, r_tag) if l_tag.is_numerical_type() && l_tag == r_tag => {
                        // if the types of the left and right hand side of the expression are numerical types,
                        // we will return a boolean type
                        Type {
                            tag: Sig::Bool,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                    (Sig::Char, Sig::Char) => {
                        // if the left hand side of the expression is a char and the right hand side of the expression
                        // is a char, we will return a boolean type
                        Type {
                            tag: Sig::Bool,
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
                            op: ">=".to_string(),
                            left: lhs_ty.as_str(),
                            right: rhs_ty.as_str(),
                        };
                        self.report_error(err);
                        Type {
                            tag: Sig::ErrorType,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                };
                self.pcode.update_expr_type(expr_i, ty.clone());
                ty
            }
            Expr::LtEq { lhs, rhs, loc, .. } => {
                // we will check the types of the left and right hand side of the expression
                let lhs_ty = self.check_expr(&lhs, recv_ty);
                let rhs_ty = self.check_expr(&rhs, recv_ty);

                // if the types of the left and right hand side of the expression are not numerical types,
                // we will return an error type
                let ty = match (lhs_ty.tag, rhs_ty.tag) {
                    (l_tag, r_tag) if l_tag.is_numerical_type() && l_tag == r_tag => {
                        // if the types of the left and right hand side of the expression are numerical types,
                        // we will return a boolean type
                        Type {
                            tag: Sig::Bool,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                    (Sig::Char, Sig::Char) => {
                        // if the left hand side of the expression is a char and the right hand side of the expression
                        // is a char, we will return a boolean type
                        Type {
                            tag: Sig::Bool,
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
                            op: "<=".to_string(),
                            left: lhs_ty.as_str(),
                            right: rhs_ty.as_str(),
                        };
                        self.report_error(err);
                        Type {
                            tag: Sig::ErrorType,
                            name: None,
                            sub_types: vec![],
                            aux_type: None,
                            loc: loc.clone(),
                        }
                    }
                };
                self.pcode.update_expr_type(expr_i, ty.clone());
                ty
            }
            _ => todo!("Checker::check_expr: implement more cases"),
        }
    }

    pub fn check(&mut self) -> SymbolTable {
        self.pass_1();
        if self.needs_next_pass {
            self.pass_2();
        }
        self.sym_table.clone()
    }
}
