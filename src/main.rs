use frontend::{lexer::Lexer, parser::Parser, source::SourceFile};

mod frontend;

fn main() {
    let text = r#"// mut a = 1 + 2;
    // let b = 3 * 5;
    // let c = a + b * a / b - a;
    a = 3;
    a = c * b / a + b - 1;
    "#
    .to_string();
    let src = SourceFile {
        path: "parser_inputs/binary_exprs.pr".into(),
        text,
        flat_index: 0,
        col: 0,
        line: 0,
    };

    let mut p = Parser::new(Lexer::new(src));
    let module = p.parse();

    for ins in module.instructions {
        println!("{}", ins.as_str());
    }

    println!("{:#?}", p.lexer_errors);
    println!("{:#?}", p.parser_errors);
}
