use super::lexer::Lexer;

#[allow(dead_code)]
pub struct Parser {
    lexer: Lexer,
}

#[allow(dead_code)]
impl Parser {
    pub fn new(l: Lexer) -> Parser {
        Parser { lexer: l }
    }

    pub fn parse() {}
}
