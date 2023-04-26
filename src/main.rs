use std::{env, fs, path::PathBuf};

use docopt::Docopt;
use frontend::{
    lexer::Lexer,
    parser::Parser,
    source::{SourceFile, SourceReporter},
};
use serde::Deserialize;

use crate::frontend::token::Token;

mod frontend;
mod pastel;

const USAGE: &str = "
Usage: proto (-h | -l | -p) -f <FILE>

Options:
    -h, --help  Show this message.
    -l          Run lexer and show its output.
    -p          Run parser and show its output.
    -f <FILE>   File to be processed.
";

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct Args {
    flag_h: bool,
    flag_l: bool,
    flag_p: bool,
    flag_f: String,
}

enum Stage {
    Lexer,
    Parser,
}

fn main() {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.deserialize())
        .unwrap_or_else(|e| e.exit());

    let stage = if args.flag_l {
        Stage::Lexer
    } else {
        Stage::Parser
    };

    let fpath = args.flag_f;
    let cwd = env::current_dir().unwrap();
    let path = format!("{}/{}", cwd.display(), fpath);
    let path = fs::canonicalize(PathBuf::from(path)).unwrap();
    let path = path.to_str().unwrap().to_string();

    let src = SourceFile::new(path);
    let reporter = SourceReporter::new(src.clone());
    let mut lexer = Lexer::new(src);

    if let Stage::Lexer = stage {
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
    } else {
        let mut parser = Parser::new(lexer);
        parser.parse();

        let _has_errors = !parser.lexer_errors.is_empty() || !parser.parser_errors.is_empty();
        for le in parser.lexer_errors {
            reporter.report_lexer_error(&le);
        }

        for pe in parser.parser_errors {
            reporter.report_parser_error(pe);
        }
    }
}
