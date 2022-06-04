use std::fs;

use parser::parse;

mod lexer;
mod common;
mod parser;
mod ast;

fn main() {
    if let Ok(source) = fs::read_to_string("example.proto") {
        if let Err(err) = parse(source) {
            println!("Error: {}", err.msg);
        }
    }
}
