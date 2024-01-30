use std::env;

use crate::compilation::pipeline::PipelineConfig;
use compilation::pipeline::Workspace;

mod compilation;
mod forge;
mod frontend;
mod pastel;
mod symbol_info;
mod pir;

const USAGE: &str = "
Usage: proto command [options]?

commands:
    - c | compile <file> [configurations]?
        - [configurations]:
            (*) backends:
                - pir (default): use PIR backend.
                - cpp: use C++ backend.
            (*) stages:
                - lex: stop after lexing.
                - parse (default): stop after parsing.
            (*) flags:
                - fmt: format the file. Default: false.
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
