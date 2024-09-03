use std::env;

use crate::compilation::pipeline::PipelineConfig;
use compilation::pipeline::Workspace;

mod compilation;
mod lexer;
mod parser;
mod pastel;
mod seman;
mod source;

const USAGE: &str = "
Usage: proto command [options]?

commands:
    - c | compile <file> [configurations]?
        - [configurations]:
            (*) backends:
                - cpp: use C++ backend.
            (*) stages:
                - lex: stop after lexing.
                - parse: stop after parsing.
                - sema (default): stop after semantic analysis.
                - gen: stop after code generation, which is the final step.
            (*) flags:
                - dbg: show debug info. Default: false.
                - help: show this help message. Default: false.
    - h | help: show this help message.
";

fn show_help() {
    println!("{}", USAGE);
}

fn main() {
    let args = env::args().collect::<Vec<String>>();
    let config = PipelineConfig::new_from_args(args);

    if config.show_help {
        show_help();
    }

    if let None = config.cmd {
        return;
    }

    let mut workspace = Workspace::new(config);
    workspace.compile_workspace();
}
