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
                Token::Eof(_) => {
                    self.tokens.push(token);
                    break;
                }
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

    fn cur_token(&self) -> &Token {
        &self.tokens[self.token_index]
    }

    fn advance_index(&mut self) {
        self.token_index += 1;
    }

    pub fn parse_const_decl(&mut self) -> Instruction {
        self.advance_index();
    }

    pub fn parse_var_decl(&mut self) -> Instruction {
        todo!()
    }

    pub fn parse(&mut self) -> Module {
        let mut main_module = Module::new();

        while self.token_index < self.tokens.len() {
            let cur: &Token = self.cur_token();
            let ins = match &cur {
                Token::Let(_) => self.parse_const_decl(),
                Token::Mut(_) => self.parse_var_decl(),
                _ => todo!(),
            };
            main_module.add_instruction(ins);
        }
        main_module
    }
}
