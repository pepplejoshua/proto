#![allow(unused)]
mod context;

pub use context::CompileContext;

#[derive(Debug)]
pub enum CompileMode {
    Script {
        path: std::path::PathBuf,
    },
    Project {
        root_dir: std::path::PathBuf,
        run: bool,
    },
}
