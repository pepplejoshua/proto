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
            let cur: Token = self.cur_token();
            let ins = match &cur {
                Token::Let(_) => self.parse_const_decl(),
                Token::Mut(_) => self.parse_var_decl(),
                _ => todo!(),
            };

            main_module.add_instruction(ins.unwrap());
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
    pub fn next_instruction(&mut self) -> Result<Instruction, ParserError> {
        todo!()
    }

    fn cur_token(&self) -> Token {
        self.tokens[self.token_index].clone()
    }

    fn advance_index(&mut self) {
        if !self.no_more_tokens() {
            self.token_index += 1;
        }
    }

    fn no_more_tokens(&self) -> bool {
        // so we don't go past Eof
        self.token_index >= self.tokens.len() - 1
    }

    // used to recover from parsing errors
    fn skip_to_next_instruction_start(&mut self) {}

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
                    "';' to terminate constant declaration".into(),
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
                Ok(expr)
            }
            _ => Err(ParserError::CannotParseAnExpression(cur.get_source_ref())),
        }
    }
}
