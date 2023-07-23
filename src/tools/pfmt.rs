use std::fs;

use crate::pir::ir::{KeyValueBindings, PIRModule, PIRModulePass};

use super::pcodeview::PCodeView;

pub struct Pfmt<'a> {
    module: &'a PIRModule,
}

impl<'a> PIRModulePass<'a, (), (), (), String, String> for Pfmt<'a> {
    fn new(module: &'a PIRModule) -> Self {
        Self { module }
    }

    fn process_ins(&mut self, _: &usize) -> Result<(), String> {
        Ok(())
    }

    fn process_expr(&mut self, _: &usize) -> Result<(), String> {
        Ok(())
    }

    fn process_pairs(&mut self, _: &KeyValueBindings) -> Result<(), String> {
        Ok(())
    }

    fn process(&mut self) -> Result<String, String> {
        let module = self.module;
        let mut code_view = PCodeView::new(&module);
        let contents = code_view.process().unwrap();

        // write out to module path
        let write_res = fs::write(&module.path, contents);
        if let Err(e) = write_res {
            Err(format!(
                "Failed to write to file at path `{}`. \n{}",
                module.path, e
            ))
        } else {
            Ok("pfmt complete".to_string())
        }
    }

    fn get_module(&mut self) -> &'a PIRModule {
        self.module
    }
}
