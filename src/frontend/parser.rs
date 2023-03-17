use super::{
    ast::{Instruction, Module},
    errors::LexerError,
    lexer::Lexer,
    token::Token,
};

#[allow(dead_code)]
pub struct Parser {
    lexer: Lexer,
    tokens: Vec<Token>,
    lexer_errors: Vec<LexerError>,
    token_index: usize,
}

#[allow(dead_code)]
impl Parser {
    pub fn new(l: Lexer) -> Parser {
        Parser {
            lexer: l,
            tokens: vec![],
            lexer_errors: vec![],
            token_index: 0,
        }
    }

    fn collect_tokens(&mut self) {
        let mut cur_token = self.lexer.next_token();
        let mut errors: Vec<LexerError> = vec![];

        loop {
            if let Err(lex_err) = cur_token {
                errors.push(lex_err.clone());
                cur_token = self.lexer.next_token();
                continue;
            }

            let token: Token = cur_token.unwrap();
            match token {
                Token::Eof(_) => break,
                _ => {
                    self.tokens.push(token);
                    cur_token = self.lexer.next_token();
                }
            }
        }
    }

    // this function will return one Instruction at
    // a time.
    pub fn next_instruction(&mut self) -> Instruction {
        todo!()
    }

    pub fn parse(&mut self) -> Module {
        let _main_module = Module::new();

        _main_module
    }
}
