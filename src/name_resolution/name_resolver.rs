use std::rc::Rc;

use crate::pir::ir::{
    ExprRef, InsRef, KeyValueBindings, PIRExpr, PIRIns, PIRModule, PIRModulePass,
};

use super::resolution_table::{ResolutionTable, SymbolKind};

pub struct Resolver<'a> {
    module: &'a PIRModule,
    table: Rc<ResolutionTable>,
}

impl<'a> Resolver<'a> {
    fn add_symbol(&mut self, identifier: String, kind: SymbolKind) -> Result<(), String> {
        Rc::get_mut(&mut self.table)
            .expect("Could not borrow table as mutable")
            .add_symbol(identifier, kind)
    }
}

impl<'a> PIRModulePass<'a, (), (), (), Rc<ResolutionTable>, String> for Resolver<'a> {
    fn new(module: &'a PIRModule) -> Self {
        Self {
            module,
            table: Rc::new(ResolutionTable::new()),
        }
    }

    fn process_ins(&mut self, ins: &InsRef) -> Result<(), String> {
        let ins_node = self.module.ins_pool.get(&ins);

        match ins_node {
            PIRIns::VariableDecl(var_name, _, init_expr, _) => {
                let var_name = var_name.as_str();
                if let Some(init_expr) = init_expr {
                    self.process_expr(&init_expr)?;
                }

                self.add_symbol(var_name, SymbolKind::Binding)?;
            }
            PIRIns::AssignmentIns(lhs, rhs) => {
                self.process_expr(&lhs)?;
                self.process_expr(&rhs)?;
            }
            PIRIns::ExpressionIns(expr, _) => {
                self.process_expr(&expr)?;
            }
            n => todo!("Inst: {n:?}"),
        }

        Ok(())
    }

    fn process_expr(&mut self, expr: &ExprRef) -> Result<(), String> {
        let expr_node = self.module.expr_pool.get(&expr);

        match expr_node {
            PIRExpr::Id(token, _) => {}
            n => todo!("Expr: {n:?}"),
        }

        Ok(())
    }

    fn process_pairs(&mut self, kv: &KeyValueBindings) -> Result<(), String> {
        todo!()
    }

    fn process(&mut self) -> Result<Rc<ResolutionTable>, String> {
        Ok(Rc::clone(&self.table))
    }

    fn get_module(&mut self) -> &'a PIRModule {
        self.module
    }
}
