use docopt::Docopt;
use frontend::{lexer::Lexer, parser::Parser, source::SourceFile};
use serde::Deserialize;

mod frontend;
mod pastel;

enum Stage {
    Lexer,
    Parser,
}

const USAGE: &str = "
Usage: proto (-h | -l | -p) -f <FILE>

Options:
    -h, --help  Show this message.
    -l          Run lexer and show its output.
    -p          Run parser and show its output.
    -f <FILE>   File to be processed.
";

#[derive(Debug, Deserialize)]
struct Args {
    flag_h: bool,
    flag_l: bool,
    flag_p: bool,
    flag_f: String,
}

fn main() {
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.deserialize())
        .unwrap_or_else(|e| e.exit());

    println!("{args:#?}");
    // let text = r#"// mut a = 1 + 2;
    // // let b = 3 * 5;
    // // let c = a + b * a / b - a;
    // a = 3;
    // a = c * b / a + b - 1;
    // "#
    // .to_string();
    // let src = SourceFile {
    //     path: "parser_inputs/binary_exprs.pr".into(),
    //     text,
    //     flat_index: 0,
    //     col: 0,
    //     line: 0,
    //     lines: vec![],
    // };

    // let mut p = Parser::new(Lexer::new(src));
    // let module = p.parse();

    // for ins in module.instructions {
    //     println!("{}", ins.as_str());
    // }

    // println!("{:#?}", p.lexer_errors);
    // println!("{:#?}", p.parser_errors);
}
