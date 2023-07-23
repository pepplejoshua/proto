use std::collections::HashMap;

use crate::{
    frontend::{ast::Expr, directives::DIRECTIVES_EXPRS, source::SourceRef, token::Token},
    pir::ir::{KeyValueBindings, PIRExpr, PIRModule, PIRModulePass},
};

#[allow(dead_code)]
pub struct SimpleExecutor<'a> {
    module: &'a PIRModule,
    new_module: PIRModule,
    expr_changes: HashMap<usize, Expr>,
}

#[allow(unused_variables)]
impl SimpleExecutor<'_> {
    fn update_expr(&mut self, directive: usize, expr: Option<usize>, src: SourceRef) {
        let directive_name = self.module.expr_pool.get(&directive);
        if let PIRExpr::Id(name, type_info) = directive_name {
            let directive_name = name.as_str();
            let directive = DIRECTIVES_EXPRS.get(directive_name.as_str());
            match directive {
                Some(_has_extra_expr) => match directive_name.as_str() {
                    "filename" => {
                        let src = name.get_source_ref();
                        // turn src.file into a str object
                        // self.module.expr_pool.to_pir(Expr::StringLiteral(
                        //     Token::StringLiteral(src, src.file.clone()),
                        //     None,
                        // ));
                    }
                    _ => {}
                },
                None => unreachable!("Unknown directive: {}", name.as_str()),
            }
        }
    }
}

// will need to do a full copy of the instructions and expr, starting from top level
// and as the nodes are being processed, when we see a DirectiveExpr, if it is one
// applicable to this pass, we will track the update to be made in the expr_changes.
// The same idea is applicable for InsExprs and KeyValueBindings.
#[allow(unused_variables)]
impl<'a> PIRModulePass<'a, (), (), (), (), ()> for SimpleExecutor<'a> {
    fn process_ins(&mut self, ins: &usize) -> Result<(), ()> {
        Ok(())
    }

    fn process_expr(&mut self, expr: &usize) -> Result<(), ()> {
        Ok(())
    }

    fn process_pairs(&mut self, kv: &KeyValueBindings) -> Result<(), ()> {
        todo!()
    }

    fn process(&mut self) -> Result<(), ()> {
        let module = self.module;
        let expr_pool = &module.expr_pool;

        for expr_node in expr_pool.pool.iter() {
            match expr_node {
                PIRExpr::DirectiveExpr {
                    directive,
                    expr: expr_loc,
                    resolved_type,
                    src,
                } => {
                    self.update_expr(*directive, *expr_loc, src.clone());
                }
                _ => continue,
            }
        }

        Ok(())
    }

    fn new(module: &'a PIRModule) -> Self {
        Self {
            module,
            new_module: PIRModule::empty(),
            expr_changes: HashMap::new(),
        }
    }

    fn get_module(&mut self) -> &'a PIRModule {
        self.module
    }
}
