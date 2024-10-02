use std::{collections::HashMap, env, fs, path::PathBuf, rc::Rc};

use crate::{
    lexer::{lexer::Lexer, token::TokenType},
    parser::parser::Parser,
    seman::seman::CheckerState,
    source::source::{SourceFile, SourceReporter},
};

#[allow(dead_code)]
pub enum Stage {
    Lexer,
    Parser,
    Sema,
    CodeGen,
}

#[allow(dead_code)]
pub enum Command {
    Compile,
}

#[allow(dead_code)]
pub enum Backend {
    WASM, // will go to WASM
    CPP,  // will go to C++
}

#[allow(dead_code)]
pub struct PipelineConfig {
    pub cmd: Option<Command>,
    pub backend: Backend,
    pub target_file: String,
    pub max_stage: Stage,
    pub show_help: bool,
    pub dbg_info: bool,
    pub use_pfmt: bool,
}

impl PipelineConfig {
    pub fn new_from_args(args: Vec<String>) -> Self {
        let mut args = args.iter().skip(1);

        // make sure there is at least 1 more arg
        if args.len() < 1 {
            return PipelineConfig {
                cmd: None,
                backend: Backend::CPP,
                target_file: "".to_string(),
                max_stage: Stage::Sema,
                show_help: true,
                dbg_info: false,
                use_pfmt: true,
            };
        }

        // expect one of:
        // c which will accept a file name and configurations
        // h which will show help
        // TODO: add more commands
        let command = args.next().unwrap();
        match command.as_str() {
            "c" | "compile" => {
                // make sure there is at least 1 more arg
                // which is the file name
                if args.len() < 1 {
                    return PipelineConfig {
                        cmd: None,
                        backend: Backend::CPP,
                        target_file: "".to_string(),
                        max_stage: Stage::Sema,
                        show_help: true,
                        dbg_info: false,
                        use_pfmt: false,
                    };
                }
                let target_file = args.next().unwrap();
                let mut backend = Backend::CPP;
                let mut max_stage = Stage::Parser;
                let mut show_help = false;
                let mut dbg_info = false;
                let mut use_pfmt = false;
                for arg in args {
                    match arg.as_str() {
                        "wasm" => backend = Backend::WASM,
                        "cpp" => backend = Backend::CPP,
                        "lex" => max_stage = Stage::Lexer,
                        "parse" => max_stage = Stage::Parser,
                        "sema" => max_stage = Stage::Sema,
                        "gen" => max_stage = Stage::CodeGen,
                        "fmt" => use_pfmt = true,
                        "dbg" => dbg_info = true,
                        "help" => show_help = true,
                        _ => {}
                    }
                }
                PipelineConfig {
                    cmd: Some(Command::Compile),
                    backend,
                    target_file: target_file.to_string(),
                    max_stage,
                    show_help,
                    dbg_info,
                    use_pfmt,
                }
            }
            "h" | "help" => PipelineConfig {
                backend: Backend::CPP,
                target_file: "".to_string(),
                max_stage: Stage::Parser,
                show_help: true,
                dbg_info: false,
                cmd: None,
                use_pfmt: false,
            },
            _ => PipelineConfig {
                backend: Backend::CPP,
                target_file: "".to_string(),
                max_stage: Stage::Parser,
                show_help: true,
                dbg_info: false,
                cmd: None,
                use_pfmt: false,
            },
        }
    }
}

pub struct Workspace {
    entry_file: String,
    files: HashMap<String, SourceFile>,
    config: PipelineConfig,
}

impl Workspace {
    pub fn new(config: PipelineConfig) -> Self {
        let entry_file = config.target_file.clone();
        let cwd = env::current_dir().unwrap();
        let abs_entry_file = format!("{}/{}", cwd.display(), entry_file);
        let abs_entry_file = fs::canonicalize(PathBuf::from(abs_entry_file)).unwrap();
        let abs_entry_file = abs_entry_file.to_str().unwrap().to_string();

        Workspace {
            entry_file: abs_entry_file,
            files: HashMap::new(),
            config,
        }
    }

    pub fn compile_workspace(&mut self) {
        self.process_file(self.entry_file.clone(), &mut vec![]);
    }

    pub fn truncate_path(&self, path: String) -> String {
        let mut path = path;
        let cwd = env::current_dir().unwrap();
        let cwd = cwd.to_str().unwrap();
        path = path.replace(cwd, "");
        path
    }

    fn process_file(&mut self, file_path: String, path_stack: &mut Vec<String>) {
        // check if file is already processed and stored in files
        if self.files.get(&file_path).is_some() {
            return;
        }

        // check for circular dependency
        for path in path_stack.clone() {
            if path == file_path {
                println!("circular dependency detected: {}", file_path);
                return;
            }
        }

        // if not, process the file
        let src = SourceFile::new(file_path.clone());
        let reporter = SourceReporter::new(src.clone());
        let msg = format!("processing {}", self.truncate_path(file_path.clone()));
        SourceReporter::show_info(msg);
        let mut lexer = Lexer::new(src.clone());

        if let Stage::Lexer = self.config.max_stage {
            loop {
                let maybe_tok = lexer.next_token();
                match maybe_tok {
                    Ok(tok) => {
                        if let TokenType::Eof = tok.ty {
                            break;
                        }
                        println!("{:#?}", tok)
                    }
                    Err(le) => reporter.report_lexer_error(&le),
                }
            }
            if self.config.dbg_info {
                SourceReporter::show_info("lexing complete.".to_string());
            }
            return;
        }

        let mut parser = Parser::new(lexer);
        let ins = parser.parse_file();

        if !parser.lex_errs.is_empty() {
            for le in parser.lex_errs {
                reporter.report_lexer_error(&le);
            }
            return;
        }
        if self.config.dbg_info {
            SourceReporter::show_info("lexing complete.".to_string());
        }

        let mut early_return = false;
        if !parser.parse_errs.is_empty() {
            for pe in parser.parse_errs {
                reporter.report_parser_error(pe);
            }
            early_return = true
        }
        if early_return {
            return;
        }

        if self.config.dbg_info {
            for i in ins.iter() {
                println!("{}\n", i.as_str(&src));
            }
            SourceReporter::show_info("parsing complete.".to_string());
        }
        if let Stage::Parser = self.config.max_stage {
            return;
        }

        let mut checker = CheckerState::new(Rc::new(src));
        let res = checker.check_main_file(ins);

        match res {
            Ok(_) => {}
            Err(errs) => {
                for se in errs {
                    reporter.report_seman_error(se);
                }
                return;
            }
        }

        // let file_mod = parser.file_mod;
        // let src_file = parser.lexer.src;
        // let (state, ty_file_mod) = check_top_level(&file_mod, src_file.clone());
        // if !state.errs.is_empty() {
        //     for ce in state.errs.iter() {
        //         reporter.report_checker_error(ce.clone());
        //     }
        //     return;
        // }

        // if self.config.dbg_info {
        //     for ins in ty_file_mod.top_level.iter() {
        //         println!("{}", ins.as_str());
        //     }
        //     SourceReporter::show_info("checking complete.".to_string());
        // }

        // if let Stage::Sema = self.config.max_stage {
        //     return;
        // }

        // let code_gen_res = cpp_gen_top_level(&ty_file_mod);

        // match code_gen_res {
        //     Ok(exe_path) => {
        //         if self.config.dbg_info {
        //             SourceReporter::show_info("codegen complete.".to_string());
        //             SourceReporter::show_info(format!("{exe_path}"));
        //         }
        //     }
        //     Err(err) => SourceReporter::show_error(err),
        // }
    }
}
