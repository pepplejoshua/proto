use std::{env, fs, path::PathBuf};

use analysis_a::dependency_res::DependencyResolvr;
use frontend::{
    lexer::Lexer,
    parser::Parser,
    source::{SourceFile, SourceReporter},
    token::Token,
};
use pir::ir::{PIRModule, PIRModulePass};

// use crate::tools::pfmt::Pfmt;

mod analysis_a;
mod frontend;
mod pastel;
mod pir;
mod tools;

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
                - parse: stop after parsing.
                - fmt (default): stop after formatting.
            (*) flags:
                - nofmt: do not format the file. Default: false.
                - dbg: show debug info. Default: false.
                - help: show this help message. Default: false.
    - h | help: show this help message.
";

#[allow(dead_code)]
enum Backend {
    PIR, // will go to PVM
    CPP, // will go to C++
}

#[allow(dead_code)]
enum Stage {
    Lexer,
    Parser,
    PfmtFile,
    DependencyResolvr,
}

#[allow(dead_code)]
enum Command {
    Compile,
}

#[allow(dead_code)]
struct ProtoConfig {
    cmd: Option<Command>,
    backend: Backend,
    target_file: String,
    max_stage: Stage,
    show_help: bool,
    dbg_info: bool,
}

fn create_config(args: Vec<String>) -> ProtoConfig {
    let mut args = args.iter().skip(1);

    // make sure there is at least 1 more arg
    if args.len() < 1 {
        return ProtoConfig {
            cmd: None,
            backend: Backend::PIR,
            target_file: "".to_string(),
            max_stage: Stage::PfmtFile,
            show_help: true,
            dbg_info: false,
        };
    }

    // expect one of:
    // c which will accept a file name and configurations
    let command = args.next().unwrap();
    match command.as_str() {
        "c" | "compile" => {
            // make sure there is at least 1 more arg
            // which is the file name
            if args.len() < 1 {
                return ProtoConfig {
                    cmd: None,
                    backend: Backend::PIR,
                    target_file: "".to_string(),
                    max_stage: Stage::PfmtFile,
                    show_help: true,
                    dbg_info: false,
                };
            }
            let target_file = args.next().unwrap();
            let mut backend = Backend::PIR;
            let mut max_stage = Stage::PfmtFile;
            let mut show_help = false;
            let mut dbg_info = false;
            for arg in args {
                match arg.as_str() {
                    "pir" => backend = Backend::PIR,
                    "cpp" => backend = Backend::CPP,
                    "lex" => max_stage = Stage::Lexer,
                    "parse" => max_stage = Stage::Parser,
                    "fmt" => max_stage = Stage::PfmtFile,
                    "dep" => max_stage = Stage::DependencyResolvr,
                    "dbg" => dbg_info = true,
                    "help" => show_help = true,
                    _ => {}
                }
            }
            ProtoConfig {
                cmd: Some(Command::Compile),
                backend,
                target_file: target_file.to_string(),
                max_stage,
                show_help,
                dbg_info,
            }
        }
        "h" | "help" => ProtoConfig {
            backend: Backend::PIR,
            target_file: "".to_string(),
            max_stage: Stage::PfmtFile,
            show_help: true,
            dbg_info: false,
            cmd: None,
        },
        _ => ProtoConfig {
            backend: Backend::PIR,
            target_file: "".to_string(),
            max_stage: Stage::PfmtFile,
            show_help: true,
            dbg_info: false,
            cmd: None,
        },
    }
}

fn show_help() {
    println!("{}", USAGE);
}

fn main() {
    let args = env::args().collect::<Vec<String>>();
    let config = create_config(args);

    if config.show_help {
        show_help();
    }

    if let None = config.cmd {
        return;
    }

    let fpath = config.target_file;
    let cwd = env::current_dir().unwrap();
    let path = format!("{}/{}", cwd.display(), fpath);
    let path = fs::canonicalize(PathBuf::from(path)).unwrap();
    let path = path.to_str().unwrap().to_string();

    let src = SourceFile::new(path.clone());
    let reporter = SourceReporter::new(src.clone());
    let mut lexer = Lexer::new(src);

    if let Stage::Lexer = config.max_stage {
        loop {
            let maybe_tok = lexer.next_token();
            match maybe_tok {
                Ok(tok) => {
                    if let Token::Eof(_) = tok {
                        break;
                    }
                }
                Err(le) => reporter.report_lexer_error(&le),
            }
        }
    }

    let mut parser = Parser::new(lexer);
    parser.parse();

    if !parser.lexer_errors.is_empty() {
        for le in parser.lexer_errors {
            reporter.report_lexer_error(&le);
        }
    }

    if config.dbg_info {
        reporter.show_info("No errors during lexing.".to_string());
    }

    if !parser.parser_errors.is_empty() {
        for pe in parser.parser_errors {
            reporter.report_parser_error(pe);
        }
    }

    if config.dbg_info {
        reporter.show_info("No errors during parsing.".to_string());
    }

    let module = parser.compilation_module;
    let mut ir_mod = PIRModule::new(module, path);

    if let Stage::PfmtFile = config.max_stage {
        // let mut pfmt = Pfmt::new(&mut ir_mod);
        // let res = pfmt.process();
        // match res {
        //     Ok(msg) if config.dbg_info => reporter.show_info(msg),
        //     Err(e) => reporter.show_error(e),
        //     _ => {}
        // }
    }

    if let Stage::DependencyResolvr = config.max_stage {
        let mut dep_resolvr = DependencyResolvr::new(&mut ir_mod);
        let res = dep_resolvr.process();
        match res {
            Ok(indices) => {
                let _ = dep_resolvr.resolve(indices);
            }
            Err(_) => todo!(),
        }
    }
}
