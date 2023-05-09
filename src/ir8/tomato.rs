use super::codeviewer::CodeViewer;
#[allow(unused_imports)]
use super::lowir::{ExprPool, InsPool, InsRef, KeyValueBindings, LowIRModule};
#[allow(unused_imports)]
use super::visitir::apply_to_module;
use std::fs;
use std::io::Error;

#[allow(dead_code)]
pub struct Tomato {
    pub filepath: String,
    viewer: CodeViewer,
}

#[allow(dead_code)]
impl Tomato {
    pub fn new(filename: String, ins_pool: InsPool, expr_pool: ExprPool) -> Tomato {
        Tomato {
            filepath: filename,
            viewer: CodeViewer::new(ins_pool, expr_pool),
        }
    }

    pub fn format(&mut self, ir_module: &LowIRModule) -> Result<(), Error> {
        let res = apply_to_module(&mut self.viewer, &ir_module);
        let res = self.viewer.unwrap(res);

        let contents = res.join("\n\n");
        self.update_file_contents(contents)
    }

    fn update_file_contents(&self, contents: String) -> Result<(), Error> {
        // open file
        fs::write(&self.filepath, contents)
    }
}
