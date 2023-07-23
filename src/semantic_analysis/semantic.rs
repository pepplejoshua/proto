use crate::pir::ir::{KeyValueBindings, PIRExpr, PIRIns, PIRModule, PIRModulePass};

use super::sym_table::{SemanticAnalysisError, SymbolTable, Type, TypeKind};

#[allow(dead_code)]
pub struct SemanticAnalyzr<'a> {
    module: &'a PIRModule,
    symbol_table: SymbolTable,
    pub errors: Vec<SemanticAnalysisError>,
}

#[allow(unused_variables)]
impl<'a> PIRModulePass<'a, (), Type, (), (), SemanticAnalysisError> for SemanticAnalyzr<'a> {
    fn process_ins(&mut self, ins: &usize) -> Result<(), SemanticAnalysisError> {
        let module = self.module;
        let ins_node = &module.ins_pool.get(&ins);

        match ins_node {
            PIRIns::SingleLineComment { comment: _, src: _ } => Ok(()),
            PIRIns::VariableDecl(name, expected_type, optional_init, src) => {
                // check that the variable name is not already defined in
                // immediate scope
                if self.symbol_table.shallow_sym_exists(&name.as_str()) {
                    return Err(SemanticAnalysisError::RedefinitionOfSymbol(
                        name.get_source_ref(),
                    ));
                }

                let mut has_type = false;
                // check that the type of the variable exists
                if let Some(expected_type) = expected_type {
                    if !self.symbol_table.type_exists(&expected_type.as_str()) {
                        return Err(SemanticAnalysisError::TypeNotDefined(
                            expected_type.as_str(),
                            expected_type.get_source_ref().unwrap(),
                        ));
                    }
                    has_type = true;
                }

                // typecheck init expression, if any
                if let Some(init) = optional_init {
                    let expr_type = self.process_expr(&init)?;
                    if has_type {
                        // check that the type of the init expression matches
                        // the expected type
                        let expected_type = expected_type.clone().unwrap();
                        let expected_type =
                            self.symbol_table.get_type(&expected_type.as_str()).unwrap();
                        if expr_type != *expected_type {
                            return Err(SemanticAnalysisError::TypeMismatch(
                                expected_type.identifier.clone(),
                                expr_type.identifier,
                                src.clone(),
                            ));
                        }
                    } else {
                        // infer the type of the variable
                        // and register it in the symbol table
                    }
                } else {
                    // if there is no init or type, set the type to be
                    // "Uninitialized". This will be checked later
                    self.symbol_table.register_uninitialized_sym(&name.as_str())
                }

                Ok(())
            }
            PIRIns::NamedStructDecl {
                name,
                fields,
                src: _,
            } => {
                let struct_name_expr = module.expr_pool.get(&name);
                let struct_name = match struct_name_expr {
                    PIRExpr::Id(name, _) => name,
                    _ => unreachable!(),
                };

                // if the name of the Struct is already defined, error
                if self.symbol_table.sym_exists(&struct_name.as_str()) {
                    return Err(SemanticAnalysisError::RedefinitionOfSymbol(
                        struct_name.get_source_ref(),
                    ));
                }

                self.process_pairs(fields)?;

                let struct_type = Type {
                    identifier: struct_name.as_str(),
                    definition_loc: Some(*ins),
                    depth: self.symbol_table.depth,
                    kind: TypeKind::UserDefined,
                };
                // register the struct
                self.symbol_table.register_type(struct_type);

                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn process_expr(&mut self, expr: &usize) -> Result<Type, SemanticAnalysisError> {
        todo!()
    }

    fn process_pairs(&mut self, kv: &KeyValueBindings) -> Result<(), SemanticAnalysisError> {
        // go through fields, check the type of the field
        // and verify it exists in the symbol table
        for (name_with_type, optional_init) in kv.pairs.iter() {
            // validate that the type of the field exists
            let field = self.module.expr_pool.get(&name_with_type);
            if let PIRExpr::Id(name, field_type) = field {
                // check if the type exists
                if let Some(field_type) = field_type {
                    if !self.symbol_table.type_exists(&field_type.as_str()) {
                        return Err(SemanticAnalysisError::TypeNotDefined(
                            field_type.as_str(),
                            name.get_source_ref(),
                        ));
                    }
                }
            }
        }
        Ok(())
    }

    fn process(&mut self) -> Result<(), SemanticAnalysisError> {
        let module = self.module;
        let top_level = &module.top_level;

        for ins_ref in top_level {
            let res = self.process_ins(ins_ref);
            if let Err(err) = res {
                self.errors.push(err);
            }
        }

        Ok(())
    }

    fn new(module: &'a PIRModule) -> Self {
        Self {
            module,
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),
        }
    }

    fn get_module(&mut self) -> &'a PIRModule {
        self.module
    }
}
