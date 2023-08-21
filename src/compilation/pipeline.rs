use std::{collections::HashMap, env, fs, path::PathBuf};

use crate::frontend::{
    lexer::Lexer,
    parser::Parser,
    source::{SourceFile, SourceReporter},
    token::Token,
};

#[allow(dead_code)]
pub enum Stage {
    Lexer,
    Parser,
    DependencyResolution,
    SemanticAnalysis,
}

#[allow(dead_code)]
pub enum Command {
    Compile,
}

#[allow(dead_code)]
pub enum Backend {
    PIR, // will go to PVM
    CPP, // will go to C++
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
                backend: Backend::PIR,
                target_file: "".to_string(),
                max_stage: Stage::DependencyResolution,
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
                        backend: Backend::PIR,
                        target_file: "".to_string(),
                        max_stage: Stage::DependencyResolution,
                        show_help: true,
                        dbg_info: false,
                        use_pfmt: false,
                    };
                }
                let target_file = args.next().unwrap();
                let mut backend = Backend::PIR;
                let mut max_stage = Stage::DependencyResolution;
                let mut show_help = false;
                let mut dbg_info = false;
                let mut use_pfmt = false;
                for arg in args {
                    match arg.as_str() {
                        "pir" => backend = Backend::PIR,
                        "cpp" => backend = Backend::CPP,
                        "lex" => max_stage = Stage::Lexer,
                        "parse" => max_stage = Stage::Parser,
                        "dep" => max_stage = Stage::DependencyResolution,
                        "sem" => max_stage = Stage::SemanticAnalysis,
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
                backend: Backend::PIR,
                target_file: "".to_string(),
                max_stage: Stage::DependencyResolution,
                show_help: true,
                dbg_info: false,
                cmd: None,
                use_pfmt: false,
            },
            _ => PipelineConfig {
                backend: Backend::PIR,
                target_file: "".to_string(),
                max_stage: Stage::DependencyResolution,
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

        let w = Workspace {
            entry_file: abs_entry_file,
            files: HashMap::new(),
            config,
        };

        // TODO: add back when std is ready and we can handle directives
        // w.process_file(
        //     "/Users/iwarilama/Desktop/Code/rust/proto/src/std/primitives.pr".to_string(),
        //     &mut vec![],
        // );

        return w;
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
        if let Some(_) = self.files.get(&file_path) {
            return;
        }

        // check for circular dependency
        for path in path_stack.clone() {
            if path == file_path {
                let msg = format!("circular dependency detected: {}", file_path);
                println!("{}", msg);
                return;
            }
        }

        // if not, process the file
        let src = SourceFile::new(file_path.clone());
        let reporter = SourceReporter::new(src.clone());
        let msg = format!("PROCESSING {}", self.truncate_path(file_path.clone()));
        reporter.show_info(msg);
        let mut lexer = Lexer::new(src.clone());

        if let Stage::Lexer = self.config.max_stage {
            loop {
                let maybe_tok = lexer.next_token();
                match maybe_tok {
                    Ok(tok) => {
                        if let Token::Eof(_) = tok {
                            break;
                        }
                        println!("{:?}", tok)
                    }
                    Err(le) => reporter.report_lexer_error(&le),
                }
            }
            if self.config.dbg_info {
                reporter.show_info("lexing complete.".to_string());
            }
            return;
        }

        let mut parser = Parser::new(lexer);
        parser.parse();

        if !parser.lexer_errors.is_empty() {
            for le in parser.lexer_errors {
                reporter.report_lexer_error(&le);
            }
            return;
        }
        if self.config.dbg_info {
            reporter.show_info("lexing complete.".to_string());
        }

        if !parser.parser_errors.is_empty() {
            for pe in parser.parser_errors {
                reporter.report_parser_error(pe);
            }
            return;
        }
        if self.config.dbg_info {
            let _module = parser.compilation_module;

            for ins in _module.instructions {
                println!("{}", ins.as_str());
            }

            reporter.show_info("parsing complete.".to_string());
        }

        if let Stage::Parser = self.config.max_stage {
            return;
        }
    }
}
