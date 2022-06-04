use std::fs;

// use parser::parse;
use common::LexContext;
use lexer::lex_next;

use crate::lexer::TokenKind;

// mod ast;
mod common;
mod lexer;
// mod parser;

fn main() {
    let source = match fs::read_to_string("examples/proto_tokens.pr") {
        Ok(it) => it,
        _ => return,
    };
    // if let Err(err) = parse(source) {
    //     println!("Error: {}", err.msg);
    // }

    let mut ctx = LexContext::new(source);

    loop {
        let cur = lex_next(&mut ctx);

        match cur {
            Ok(token) => {
                println!("Token -> {:#?}", token);

                if token.kind == TokenKind::End {
                    break;
                }
            }
            Err(e) => {
                println!("Error -> {:#?}", e);
                break;
            }
        }
    }
}
