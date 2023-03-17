use super::{
    ast::{Expr, Instruction, Module},
    errors::{LexerError, ParserError},
    lexer::Lexer,
    token::Token,
    types::Type,
};

#[allow(dead_code)]
pub struct Parser {
    lexer: Lexer,
    tokens: Vec<Token>,
    lexer_errors: Vec<LexerError>,
    parser_errors: Vec<ParserError>,
    token_index: usize,
}

#[allow(dead_code)]
impl Parser {
    pub fn new(l: Lexer) -> Parser {
        Parser {
            lexer: l,
            tokens: vec![],
            lexer_errors: vec![],
            parser_errors: vec![],
            token_index: 0,
        }
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

    // used to recover from parsing errors
    fn skip_to_next_instruction_start(&mut self) {}

    fn parse_const_decl(&mut self) -> Result<Instruction, ParserError> {
        self.advance_index(); // skip past "let"

        let mut cur = self.cur_token();
        let mut label = None;
        if let Token::Identifier(_, _) = cur {
            label = Some(cur);
            self.advance_index();
            cur = self.cur_token();
        } else {
            return Err(ParserError::Expected(
                "an identifier".into(),
                *self.cur_token().get_source_ref(),
            ));
        }

        let mut const_type = None;
        // try to parse an optional type and/or an optional assignment
        if cur.is_type_token() {
            const_type = Some(cur.to_type());
            self.advance_index();
        }

        cur = self.cur_token();
        if let Token::Assign(_) = cur {
            self.advance_index();
            cur = self.cur_token();
        } else {
            return Err(ParserError::ConstantDeclarationNeedsInitValue(
                *self.cur_token().get_source_ref(),
            ));
        }

        let mut init_value = self.parse_expr();
    }

    fn parse_var_decl(&mut self) -> Result<Instruction, ParserError> {
        todo!()
    }

    fn parse_expr(&mut self) -> Result<Expr, ParserError> {
        todo!()
    }

    fn parse_primary(&mut self) -> Result<Expr, ParserError> {
        let mut cur = self.cur_token();

        match cur {
            Token::I8Literal(_, _) => {
                let res = Ok(Expr::Number(*cur, Some(Type::I8)));
                self.advance_index();
                res
            }
            Token::I16Literal(_, _) => {
                let res = Ok(Expr::Number(*cur, Some(Type::I16)));
                self.advance_index();
                res
            }
            Token::I32Literal(_, _) => {
                let res = Ok(Expr::Number(*cur, Some(Type::I32)));
                self.advance_index();
                res
            }
            Token::I64Literal(_, _) => {
                let res = Ok(Expr::Number(*cur, Some(Type::I64)));
                self.advance_index();
                res
            }
            Token::IsizeLiteral(_, _) => {
                let res = Ok(Expr::Number(*cur, Some(Type::ISize)));
                self.advance_index();
                res
            }
            Token::U8Literal(_, _) => {
                let res = Ok(Expr::Number(*cur, Some(Type::U8)));
                self.advance_index();
                res
            }
            Token::U16Literal(_, _) => {
                let res = Ok(Expr::Number(*cur, Some(Type::U16)));
                self.advance_index();
                res
            }
            Token::U32Literal(_, _) => {
                let res = Ok(Expr::Number(*cur, Some(Type::U32)));
                self.advance_index();
                res
            }
            Token::U64Literal(_, _) => {
                let res = Ok(Expr::Number(*cur, Some(Type::U64)));
                self.advance_index();
                res
            }
            Token::UsizeLiteral(_, _) => {
                let res = Ok(Expr::Number(*cur, Some(Type::USize)));
                self.advance_index();
                res
            }
            Token::LParen(_) => {
                // parse group
                self.advance_index();
                let expr = self.parse_expr()?;
                let maybe_rparen = self.cur_token();
                if let Token::RParen(_) = maybe_rparen {
                    self.advance_index();
                } else {
                    return Err(ParserError::Expected(
                        "a ')' to terminate the grouped expression".into(),
                        *maybe_rparen.get_source_ref(),
                    ));
                }
                Ok(expr)
            }
        }
    }
}
