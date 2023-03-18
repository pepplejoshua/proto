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
    lexer_errors: Vec<LexerError>,
    parser_errors: Vec<ParserError>,
    lexed_token: Option<Token>,
}

#[allow(dead_code)]
impl Parser {
    pub fn new(l: Lexer) -> Parser {
        let mut p = Parser {
            lexer: l,
            lexer_errors: vec![],
            parser_errors: vec![],
            lexed_token: None,
        };
        p.advance_index();
        p
    }

    pub fn parse(&mut self) -> Module {
        let mut main_module = Module::new();

        while !self.no_more_tokens() {
            let ins = self.next_instruction();

            match ins {
                Ok(instruc) => main_module.add_instruction(instruc),
                Err(err) => {
                    self.parser_errors.push(err);
                    self.skip_to_next_instruction_start();
                }
            }
        }
        main_module
    }

    // this function will return one Instruction at
    // a time.
    pub fn next_instruction(&mut self) -> Result<Instruction, ParserError> {
        let cur: Token = self.cur_token();
        match &cur {
            Token::Let(_) => self.parse_const_decl(),
            Token::Mut(_) => self.parse_var_decl(),
            _ => self.parse_assignment_or_expr_instruc(),
        }
    }

    fn cur_token(&self) -> Token {
        let tok = self.lexed_token.clone();
        tok.unwrap()
    }

    fn advance_index(&mut self) {
        loop {
            let next_token = self.lexer.next_token();
            match next_token {
                Ok(tok) => {
                    self.lexed_token = Some(tok);
                    break;
                }
                Err(l_err) => {
                    self.lexer_errors.push(l_err);
                }
            }
        }
    }

    fn no_more_tokens(&self) -> bool {
        // so we don't go past Eof
        matches!(self.cur_token(), Token::Eof(_))
    }

    // used to recover from parsing errors
    fn skip_to_next_instruction_start(&mut self) {
        // read till we see one of:
        // - ;
        // - }
        loop {
            let cur = self.cur_token();
            match cur {
                Token::Semicolon(_) | Token::RBrace(_) => {
                    self.advance_index();
                    break;
                }
                Token::Eof(_) => break,
                _ => self.advance_index(),
            }
        }
    }

    fn parse_const_decl(&mut self) -> Result<Instruction, ParserError> {
        let start = self.cur_token();
        self.advance_index(); // skip past "let"

        let mut cur = self.cur_token();
        let label = Some(cur.clone());
        let const_name;
        if let Token::Identifier(name, _) = cur {
            const_name = name;
            self.advance_index();
            cur = self.cur_token();
        } else {
            return Err(ParserError::Expected(
                "an identifier to name constant".into(),
                self.cur_token().get_source_ref(),
                Some("Provide a name for the constant. E.g: let my_constant = value;".into()),
            ));
        }

        let mut const_type = None;
        // try to parse an optional type and/or an optional assignment
        // this will be replaced with a call to parse_type(try: true)
        // which will try to parse a type
        if cur.is_type_token() {
            const_type = Some(cur.to_type());
            self.advance_index();
            cur = self.cur_token();
        }

        if let Token::Assign(_) = cur {
            self.advance_index();
        } else {
            return Err(ParserError::ConstantDeclarationNeedsInitValue(
                self.cur_token()
                    .get_source_ref()
                    .combine(start.get_source_ref()),
            ));
        }

        let init_value = self.parse_expr()?;
        cur = self.cur_token();
        match cur {
            Token::Semicolon(_) => {
                let end_ref = cur.get_source_ref();
                self.advance_index();
                let name = label.unwrap();
                Ok(Instruction::ConstantDecl(
                    name,
                    const_type,
                    init_value,
                    start.get_source_ref().combine(end_ref),
                ))
            }
            _ => {
                let tip = match const_type {
                    Some(type_t) => {
                        format!(
                            "Terminate constant declaration: let {const_name} {} = {};",
                            type_t.as_str(),
                            init_value.as_str()
                        )
                    }
                    None => format!(
                        "Terminate constant declaration: let {const_name} = {};",
                        init_value.as_str()
                    ),
                };
                Err(ParserError::Expected(
                    "';' to terminate constant declaration".into(),
                    cur.get_source_ref(),
                    Some(tip),
                ))
            }
        }
    }

    fn parse_var_decl(&mut self) -> Result<Instruction, ParserError> {
        let start = self.cur_token();
        self.advance_index(); // skip past "let"

        let mut cur = self.cur_token();
        let label = Some(cur.clone());
        let var_name;
        if let Token::Identifier(name, _) = cur {
            var_name = name;
            self.advance_index();
            cur = self.cur_token();
        } else {
            return Err(ParserError::Expected(
                "an identifier to name variable".into(),
                self.cur_token().get_source_ref(),
                Some("Provide a name for the variable. E.g: let my_var = value;".into()),
            ));
        }

        let mut var_type = None;
        // try to parse an optional type and/or an optional assignment
        // this will be replaced with a call to parse_type(try: true)
        // which will try to parse a type
        if cur.is_type_token() {
            var_type = Some(cur.to_type());
            self.advance_index();
            cur = self.cur_token();
        }

        let mut init_value = None;
        if let Token::Assign(_) = cur {
            self.advance_index();
            init_value = Some(self.parse_expr()?);
        }

        cur = self.cur_token();
        match cur {
            Token::Semicolon(_) => {
                let end_ref = cur.get_source_ref();
                self.advance_index();
                let name = label.unwrap();
                Ok(Instruction::VariableDecl(
                    name,
                    var_type,
                    init_value,
                    start.get_source_ref().combine(end_ref),
                ))
            }
            _ => {
                let tip = match (var_type, init_value) {
                    (None, None) => {
                        format!("Terminate variable declaration: let {var_name};")
                    }
                    (None, Some(expr)) => {
                        format!(
                            "Terminate variable declaration: let {var_name} = {};",
                            expr.as_str()
                        )
                    }
                    (Some(v_type), None) => {
                        format!(
                            "Terminate variable declaration: let {var_name} {};",
                            v_type.as_str()
                        )
                    }
                    (Some(v_type), Some(expr)) => {
                        format!(
                            "Terminate variable declaration: let {var_name} {} = {};",
                            v_type.as_str(),
                            expr.as_str(),
                        )
                    }
                };
                Err(ParserError::Expected(
                    "';' to terminate variable declaration".into(),
                    cur.get_source_ref(),
                    Some(tip),
                ))
            }
        }
    }

    fn parse_assignment_or_expr_instruc(&mut self) -> Result<Instruction, ParserError> {
        let target = self.parse_expr()?;

        let cur = self.cur_token();

        match cur {
            Token::Assign(_) => {
                self.advance_index();
                let value = self.parse_expr()?;
                Ok(Instruction::AssignmentIns(target, value))
            }
            Token::Semicolon(_) => {
                self.advance_index();
                Ok(Instruction::ExpressionIns(target, cur))
            }
            _ => Err(ParserError::Expected(
                "a terminated expression or an assignment instruction".into(),
                cur.get_source_ref(),
                None,
            )),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParserError> {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<Expr, ParserError> {
        let mut lhs = self.parse_and()?;

        loop {
            let cur = self.cur_token();
            match cur {
                Token::Or(_) => {
                    self.advance_index();
                    let rhs = self.parse_and()?;
                    lhs = Expr::Comparison(cur, Box::new(lhs), Box::new(rhs), None);
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_and(&mut self) -> Result<Expr, ParserError> {
        let mut lhs = self.parse_equality()?;

        loop {
            let cur = self.cur_token();
            match cur {
                Token::And(_) => {
                    self.advance_index();
                    let rhs = self.parse_equality()?;
                    lhs = Expr::Comparison(cur, Box::new(lhs), Box::new(rhs), None);
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_equality(&mut self) -> Result<Expr, ParserError> {
        let mut lhs = self.parse_comparison()?;

        loop {
            let cur = self.cur_token();
            match cur {
                Token::Equal(_) | Token::NotEqual(_) => {
                    self.advance_index();
                    let rhs = self.parse_comparison()?;
                    lhs = Expr::Comparison(cur, Box::new(lhs), Box::new(rhs), None);
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParserError> {
        let mut lhs = self.parse_term()?;

        loop {
            let cur = self.cur_token();
            match cur {
                Token::Greater(_)
                | Token::GreaterEqual(_)
                | Token::Less(_)
                | Token::LessEqual(_) => {
                    self.advance_index();
                    let rhs = self.parse_term()?;
                    lhs = Expr::Comparison(cur, Box::new(lhs), Box::new(rhs), None);
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_term(&mut self) -> Result<Expr, ParserError> {
        let mut lhs = self.parse_factor()?;

        loop {
            let cur = self.cur_token();
            match cur {
                Token::Plus(_) | Token::Minus(_) => {
                    self.advance_index();
                    let rhs = self.parse_factor()?;
                    lhs = Expr::Binary(cur, Box::new(lhs), Box::new(rhs), None);
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_factor(&mut self) -> Result<Expr, ParserError> {
        let mut lhs = self.parse_unary()?;

        loop {
            let cur = self.cur_token();
            match cur {
                Token::Star(_) | Token::Modulo(_) | Token::Slash(_) => {
                    self.advance_index();
                    let rhs = self.parse_unary()?;
                    lhs = Expr::Binary(cur, Box::new(lhs), Box::new(rhs), None);
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_unary(&mut self) -> Result<Expr, ParserError> {
        let cur = self.cur_token();
        match cur {
            Token::Not(_) => {
                self.advance_index();
                let operand = self.parse_unary()?;
                Ok(Expr::Unary(cur, Box::new(operand), None))
            }
            _ => self.parse_index_like_exprs(),
        }
    }

    fn parse_index_like_exprs(&mut self) -> Result<Expr, ParserError> {
        let mut lhs = self.parse_primary()?;

        loop {
            let mut cur = self.cur_token();

            match cur {
                Token::LParen(_) => {
                    self.advance_index();

                    let mut args: Vec<Expr> = vec![];
                    'args: while !self.no_more_tokens() {
                        if args.len() > 255 {
                            return Err(ParserError::TooManyFnArgs(
                                lhs.source_ref().combine(cur.get_source_ref()),
                            ));
                        }
                        let arg = self.parse_expr()?;
                        args.push(arg);

                        cur = self.cur_token();

                        match cur {
                            Token::RParen(_) => {
                                lhs = Expr::FnCall {
                                    func: Box::new(lhs),
                                    args,
                                    rparen: cur,
                                    fn_type: None,
                                };
                                self.advance_index();
                                break 'args;
                            }
                            Token::Comma(_) => {
                                self.advance_index();
                                continue 'args;
                            }
                            _ => {
                                return Err(ParserError::Expected(
                                    "',' to separate arguments or ')' to terminate function call"
                                        .into(),
                                    cur.get_source_ref(),
                                    None,
                                ))
                            }
                        }
                    }
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, ParserError> {
        let cur = self.cur_token();

        match cur {
            Token::Identifier(_, _) => {
                let res = Ok(Expr::Id(cur, None));
                self.advance_index();
                res
            }
            Token::I8Literal(_, _) => {
                let res = Ok(Expr::Number(cur, Some(Type::I8)));
                self.advance_index();
                res
            }
            Token::I16Literal(_, _) => {
                let res = Ok(Expr::Number(cur, Some(Type::I16)));
                self.advance_index();
                res
            }
            Token::I32Literal(_, _) => {
                let res = Ok(Expr::Number(cur, Some(Type::I32)));
                self.advance_index();
                res
            }
            Token::I64Literal(_, _) => {
                let res = Ok(Expr::Number(cur, Some(Type::I64)));
                self.advance_index();
                res
            }
            Token::IsizeLiteral(_, _) => {
                let res = Ok(Expr::Number(cur, Some(Type::ISize)));
                self.advance_index();
                res
            }
            Token::U8Literal(_, _) => {
                let res = Ok(Expr::Number(cur, Some(Type::U8)));
                self.advance_index();
                res
            }
            Token::U16Literal(_, _) => {
                let res = Ok(Expr::Number(cur, Some(Type::U16)));
                self.advance_index();
                res
            }
            Token::U32Literal(_, _) => {
                let res = Ok(Expr::Number(cur, Some(Type::U32)));
                self.advance_index();
                res
            }
            Token::U64Literal(_, _) => {
                let res = Ok(Expr::Number(cur, Some(Type::U64)));
                self.advance_index();
                res
            }
            Token::UsizeLiteral(_, _) => {
                let res = Ok(Expr::Number(cur, Some(Type::USize)));
                self.advance_index();
                res
            }
            Token::True(_) | Token::False(_) => {
                self.advance_index();
                Ok(Expr::Boolean(cur, Some(Type::Bool)))
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
                        maybe_rparen.get_source_ref(),
                        None,
                    ));
                }
                Ok(Expr::Grouped(
                    Box::new(expr),
                    None,
                    cur.get_source_ref().combine(maybe_rparen.get_source_ref()),
                ))
            }
            _ => Err(ParserError::CannotParseAnExpression(cur.get_source_ref())),
        }
    }
}

#[allow(dead_code)]
#[derive(serde::Deserialize, serde::Serialize, Debug)]
struct ParserTestResult {
    module: Vec<String>,
    lexer_error: Vec<LexerError>,
    parser_error: Vec<ParserError>,
}

#[test]
fn test_parser() {
    use crate::frontend::source::SourceFile;
    insta::glob!("parser_inputs/*.pr", |path| {
        // build the SourceFile from the proto file
        let path = path.to_str().unwrap().to_string();
        let src: SourceFile = SourceFile::new(path);

        // build the lexer
        let lexer = Lexer::new(src);
        let mut parser = Parser::new(lexer);

        let module = parser.parse();
        let l_errs = parser.lexer_errors;
        let p_errs = parser.parser_errors;

        let mut res = ParserTestResult {
            module: vec![],
            lexer_error: l_errs,
            parser_error: p_errs,
        };

        for ins in module.instructions {
            res.module.push(ins.as_str());
        }

        insta::assert_yaml_snapshot!(res, @r###"
        ---
        module:
          - let a i8 = 32;
          - mut b = 0;
          - let c = false;
          - mut d;
          - let e usize = 0;
        lexer_error: []
        parser_error: []
        "###)
    });
}
