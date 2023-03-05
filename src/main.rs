use crate::frontend::{lexer::Lexer, source::SourceFile, token::Token};

mod frontend;

// use frontend::colors::ColorOption;

fn main() {
    // let col = ColorOption::bold_and(
    //     Some("l:white".to_string()),
    //     Some("l:magenta".to_string()));

    // println!(
    //     "{}",
    //     col.format_text("Bold white text on magenta background!")
    // );

    // let underline = ColorOption::underline_and(
    //     Some("d:black".to_string()),
    //     Some("d:white".to_string()));

    // println!(
    //     "\n{}",
    //     underline.format_text("Underlined black text on white background!")
    // );

    insta::glob!("frontend/lexer_inputs/*.json", |path| {
        println!("testing lexer on file: {}", path.display());
        // build the SourceFile from the json file
        let file_contents = std::fs::read_to_string(path).unwrap();
        let src: SourceFile = serde_json::from_str(&file_contents).unwrap();

        // build the lexer
        let mut lexer = Lexer::new(src);

        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            if token.is_err() {
                panic!("error lexing: {token:?}");
            }
            let token = token.unwrap();
            if let Token::Eof(_) = token {
                break;
            }
            tokens.push(token);
        }
        tokens.iter().for_each(|token| println!("token: {token:?}"));
    });
}
