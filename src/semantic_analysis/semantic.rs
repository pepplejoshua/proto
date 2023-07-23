use crate::pir::ir::{KeyValueBindings, PIRIns, PIRModule, PIRModulePass};

use super::sym_table::{SemanticAnalysisError, SymbolTable};

#[allow(dead_code)]
pub struct SemanticAnalyzr<'a> {
    module: &'a PIRModule,
    symbol_table_with_types: SymbolTable,
    pub errors: Vec<SemanticAnalysisError>,
}

#[allow(unused_variables)]
impl<'a> PIRModulePass<'a, (), (), (), (), SemanticAnalysisError> for SemanticAnalyzr<'a> {
    fn process_ins(&mut self, ins: &usize) -> Result<(), SemanticAnalysisError> {
        let module = self.module;
        let ins_node = &module.ins_pool.get(&ins);

        match ins_node {
            PIRIns::SingleLineComment { comment: _, src: _ } => Ok(()),
            PIRIns::NamedStructDecl {
                name: _,
                fields: _,
                src: _,
            } => Ok(()),
            _ => Ok(()),
        }
    }

    fn process_expr(&mut self, expr: &usize) -> Result<(), SemanticAnalysisError> {
        Ok(())
    }

    fn process_pairs(&mut self, kv: &KeyValueBindings) -> Result<(), SemanticAnalysisError> {
        todo!()
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
            symbol_table_with_types: SymbolTable::new(),
            errors: Vec::new(),
        }
    }

    fn get_module(&mut self) -> &'a PIRModule {
        self.module
    }
}
