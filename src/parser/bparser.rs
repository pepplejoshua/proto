use crate::frontend::bcode::IndexTag;

use super::{
    bcode::{Code, CodeBundle, CodeTag, Index},
    errors::{LexError, ParseError},
    lexer::Lexer,
    token::Token,
    types::{TypeSignature, TypeSignatureTag},
};

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
enum ParseScope {
    TopLevel,
    Fn,
    Loop,
    CodeBlock,
    Struct,
}

#[allow(dead_code)]
pub struct Parser {
    pub lexer: Lexer,
    pub lexer_errors: Vec<LexError>,
    pub parser_errors: Vec<ParseError>,
    lexed_token: Option<Token>,
    parse_scope: ParseScope,
    pub code: CodeBundle,
    current_fn_ret_ty_index: Option<Index>,
    allow_struct_init: bool,
}

#[allow(dead_code)]
impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut p = Self {
            lexer,
            lexer_errors: vec![],
            parser_errors: vec![],
            lexed_token: None,
            parse_scope: ParseScope::TopLevel,
            code: CodeBundle::new(),
            allow_struct_init: true,
            current_fn_ret_ty_index: None,
        };
        p.advance_index();
        p
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

    fn cur_token(&self) -> Token {
        let tok = self.lexed_token.clone();
        tok.unwrap()
    }

    fn no_more_tokens(&self) -> bool {
        // so we don't go past Eof
        matches!(self.cur_token(), Token::Eof(_))
    }

    fn report_error(&mut self, err: ParseError) {
        self.parser_errors.push(err);
        // if we have reached 10 errors, stop parsing
        // exit quietly without Rust stack trace
        if self.parser_errors.len() >= 10 {
            // show all errors and exit
            use super::source::SourceReporter;
            let reporter = SourceReporter::new(self.lexer.src.clone());
            for err in self.lexer_errors.iter() {
                reporter.report_lexer_error(err);
            }

            for err in self.parser_errors.iter() {
                reporter.report_parser_error(err.clone());
            }

            let too_many_errors = ParseError::TooManyErrors(self.cur_token().get_source_ref());
            reporter.report_parser_error(too_many_errors);
            // process exit with error code to show
            // user error
            std::process::exit(1);
        }
    }

    fn recover_from_err(&mut self) {
        while !self.no_more_tokens() {
            let cur = self.cur_token();
            if cur.is_terminator() {
                self.advance_index();
                break;
            } else if cur.begins_instruction() {
                break;
            } else {
                self.advance_index();
            }
        }
    }

    pub fn parse(&mut self) {
        while !self.no_more_tokens() {
            let index_m = self.next_instruction(true);

            match index_m {
                Ok(_) => { /* do nothing, since we have already added the code */ }
                Err(e) => {
                    if matches!(
                        e,
                        ParseError::CannotParseAnExpressionOrType(_)
                            | ParseError::Expected(_, _, _)
                            | ParseError::NoBreakOutsideLoop(_)
                            | ParseError::NoContinueOutsideLoop(_),
                    ) {
                        self.recover_from_err();
                    }
                    self.report_error(e);
                }
            }
        }
    }

    fn next_instruction(&mut self, enter_scope_for_new_blocks: bool) -> Result<Index, ParseError> {
        let mut cur = self.cur_token();
        match cur {
            Token::SingleLineComment(src, comment) => {
                self.advance_index();
                let comment_i = self.code.add_string(comment);
                let ins = Code {
                    tag: CodeTag::SrcComment,
                    data: vec![comment_i],
                    src,
                };
                Ok(self.code.add_ins(ins))
            }
            Token::Fn(_) => self.parse_fn(),
            Token::Return(_) => self.parse_return(),
            Token::Let(_) => self.parse_const_var(),
            Token::Mut(_) => self.parse_mut_var(),
            Token::LCurly(_) => self.parse_code_block(enter_scope_for_new_blocks),
            Token::Pub(_) => {
                let pub_ref = cur.get_source_ref();
                self.advance_index();
                cur = self.cur_token();
                let pub_is_allowed = matches!(self.parse_scope, ParseScope::TopLevel);
                let target_i = match cur {
                    Token::Let(_) => self.parse_const_var(),
                    Token::Mut(_) => self.parse_mut_var(),
                    Token::Fn(_) => self.parse_fn(),
                    _ => {
                        let ins_i = self.next_instruction(enter_scope_for_new_blocks)?;
                        let ins = self.code.get_ins(ins_i);
                        Err(ParseError::MisuseOfPubKeyword(
                            ins.src.combine(pub_ref.clone()),
                        ))
                    }
                };

                if !pub_is_allowed {
                    let ins = self.code.get_ins(target_i?);
                    return Err(ParseError::MisuseOfPubKeyword(
                        ins.src.combine(pub_ref.clone()),
                    ));
                }

                if let Ok(target_i) = target_i {
                    let ins = self.code.get_ins(target_i);
                    let pub_ins = Code {
                        tag: CodeTag::MakePublic,
                        data: vec![target_i],
                        src: ins.src.combine(pub_ref),
                    };
                    Ok(self.code.add_ins(pub_ins))
                } else {
                    target_i
                }
            }
            _ => self.parse_expr_instr(),
        }
        // todo!()
    }

    fn parse_expr_instr(&mut self) -> Result<Index, ParseError> {
        let mut src = self.cur_token().get_source_ref();
        let target_i = self.parse_expr()?;

        let mut cur = self.cur_token();
        src = src.combine(cur.get_source_ref());

        match cur {
            Token::Assign(_) => {
                self.advance_index();
                let value_i = self.parse_expr()?;
                cur = self.cur_token();
                src = src.combine(cur.get_source_ref());
                if let Token::Semicolon(_) = cur {
                    self.advance_index();
                } else {
                    let tip = "Terminate assignment instruction.".to_string();
                    return Err(ParseError::Expected(
                        "a ';' to terminate the assignment instruction.".into(),
                        src,
                        Some(tip),
                    ));
                }
                let indices = vec![target_i, value_i];
                let ins = Code {
                    tag: CodeTag::Update,
                    data: indices,
                    src,
                };
                Ok(self.code.add_ins(ins))
            }
            Token::Semicolon(_) => {
                self.advance_index();
                Ok(target_i)
            }
            _ => Err(ParseError::Expected(
                "a semi-colon terminated expression or an assignment instruction.".into(),
                src,
                None,
            )),
        }
    }

    // all callers of this method should set the ParseScope before
    // calling this method
    fn parse_code_block(&mut self, enter_scope: bool) -> Result<Index, ParseError> {
        let start = self.cur_token();
        self.advance_index(); // skip past `{`

        if enter_scope {
            let en_scope_c = Code {
                tag: CodeTag::EnterScope,
                data: vec![],
                src: start.get_source_ref(),
            };
            self.code.add_ins(en_scope_c);
        }

        let mut cur = self.cur_token();
        while !self.no_more_tokens() && !matches!(cur, Token::RCurly(_)) {
            let ins = self.next_instruction(true);
            if ins.is_err() {
                self.report_error(ins.err().unwrap());
                self.recover_from_err();
                cur = self.cur_token();
                continue;
            }
            cur = self.cur_token();
        }

        if !matches!(cur, Token::RCurly(_)) {
            let src = start.get_source_ref().combine(cur.get_source_ref());
            let tip = "Terminate code block with '}'. E.g: '{ // content }'.".to_string();
            return Err(ParseError::UnterminatedCodeBlock(src, Some(tip)));
        }

        self.advance_index(); // skip past `}`

        if enter_scope {
            let ex_scope_c = Code {
                tag: CodeTag::ExitScope,
                data: vec![],
                src: cur.get_source_ref(),
            };
            let exit = self.code.add_ins(ex_scope_c);
            Ok(exit)
        } else {
            Ok(Index {
                tag: IndexTag::Code,
                index: 0,
            })
        }
    }

    fn parse_fn(&mut self) -> Result<Index, ParseError> {
        let temp_scope = self.parse_scope;
        self.parse_scope = ParseScope::Fn;
        let start = self.cur_token();
        self.advance_index(); // skip past `fn`

        let mut cur = self.cur_token();
        let mut indices = vec![];
        // handle function name
        if let Token::Identifier(name, _) = cur {
            let name_i = self.code.add_string(name);
            indices.push(name_i);
            self.advance_index();
            cur = self.cur_token();
        } else {
            return Err(ParseError::Expected(
                "an identifier to name function.".into(),
                self.cur_token().get_source_ref(),
                Some("Provide a name for the function. E.g: 'fn my_function() {}'".into()),
            ));
        }

        // reserve function index
        let fn_i = self.code.reserve_ins();

        // handle function parameters
        if !matches!(cur, Token::LParen(_)) {
            return Err(ParseError::Expected(
                "an open parenthesis to start function parameters.".into(),
                self.cur_token().get_source_ref(),
                Some("Provide a name for the function. E.g: 'fn my_function() {}'".into()),
            ));
        }
        self.advance_index(); // skip past `(`
        let mut params_c = 0;
        let mut too_many_params = false;
        cur = self.cur_token();
        let mut fn_tys = vec![];
        let params_start = cur.get_source_ref();
        while !self.no_more_tokens() {
            if !too_many_params && params_c >= 20 {
                too_many_params = true;
                self.report_error(ParseError::TooManyFnParams(
                    self.cur_token().get_source_ref(),
                ));
            }

            if !matches!(cur, Token::RParen(_)) {
                // handle parameter syntax and get the parameter type index
                // back and use it in the function instruction
                let param_ty_i = self.parse_param();
                if let Err(err) = param_ty_i {
                    self.report_error(err);
                    self.recover_from_err();
                    cur = self.cur_token();
                    continue;
                }

                let param_ty_i = param_ty_i?;
                fn_tys.push(param_ty_i);
                params_c += 1;
                cur = self.cur_token();

                if matches!(cur, Token::Comma(_)) {
                    self.advance_index();
                    cur = self.cur_token();
                }
            } else {
                break;
            }
        }

        if !matches!(cur, Token::RParen(_)) {
            return Err(ParseError::Expected(
                "a closing parenthesis to end function parameters.".into(),
                self.cur_token().get_source_ref(),
                Some("Provide a name for the function. E.g: 'fn my_function() {}'".into()),
            ));
        }

        self.advance_index(); // skip past `)`
        let return_ty_i = self.parse_type(false)?;

        // get return type instruction so we can use its span
        let return_ty_src = self.code.types.get(return_ty_i.index).unwrap().src.clone();

        let temp_ret_ty_index = self.current_fn_ret_ty_index;
        self.current_fn_ret_ty_index = Some(return_ty_i);

        // put return_ty_i at the first index of fn_tys
        fn_tys.insert(0, return_ty_i);

        // construct a function type for this function
        let fn_ty_src = params_start.combine(return_ty_src);
        let fn_ty = TypeSignature {
            tag: TypeSignatureTag::FunctionTS,
            src: fn_ty_src,
            indices: fn_tys,
        };

        // add the function type to the type table
        let fn_ty_i = self.code.add_type(fn_ty);

        // add the function type index to the function instruction
        indices.push(fn_ty_i);

        // we do not need to track the index of the next instruction
        // since we will be using the span of instructions right after the
        // function instruction till the end of the function body
        let body_i = self.next_instruction(false)?;
        let body_src = self.code.get_ins(body_i).src.clone();
        let body_end_i = Index {
            tag: IndexTag::Code,
            index: self.code.ins.len() - 1,
        };

        self.current_fn_ret_ty_index = temp_ret_ty_index;
        indices.push(body_end_i);
        let fn_ins = Code {
            tag: CodeTag::NewFunction,
            data: indices,
            src: start.get_source_ref().combine(body_src.clone()),
        };
        self.code.update_ins(fn_i, fn_ins);
        self.parse_scope = temp_scope;
        // add end of function instruction
        let end_fn_ins = Code {
            tag: CodeTag::EndFunction,
            data: vec![],
            src: start.get_source_ref().combine(body_src),
        };
        self.code.add_ins(end_fn_ins);
        Ok(fn_i)
    }

    fn parse_return(&mut self) -> Result<Index, ParseError> {
        let start = self.cur_token();
        let mut src = start.get_source_ref();
        self.advance_index(); // skip past `return`

        // if there is a value, use a combination of
        // ExpectTypeIs @cur_return_type Code:ReturnValue
        // and Return Code:ReturnValue

        // if there is no value, use a combination of
        // ExpectTypeEq @cur_return_type void
        let mut cur = self.cur_token();
        let mut indices = vec![];

        if !matches!(cur, Token::Semicolon(_)) {
            // has a return value
            let expr_i = self.parse_expr()?;
            cur = self.cur_token();

            // expect a semicolon
            if !matches!(cur, Token::Semicolon(_)) {
                return Err(ParseError::Expected(
                    "a semicolon to end return statement.".into(),
                    self.cur_token().get_source_ref(),
                    Some(
                        "Provide a semicolon to end the return statement. E.g: 'return 42;'".into(),
                    ),
                ));
            } else {
                src = src.combine(cur.get_source_ref());
            }

            // generate instruction to do type checking
            let cur_return_ty_i = self.current_fn_ret_ty_index.unwrap();
            let expect_ty_ins = Code {
                tag: CodeTag::ExpectTypeIs,
                data: vec![cur_return_ty_i, expr_i],
                src: src.clone(),
            };
            let expect_ty_i = self.code.add_ins(expect_ty_ins);
            indices.push(expect_ty_i);
            self.advance_index();
        } else {
            // has no return value
            src = src.combine(cur.get_source_ref());
            let cur_return_ty_i = self.current_fn_ret_ty_index.unwrap();
            let void_ty = TypeSignature {
                tag: TypeSignatureTag::VoidTS,
                indices: vec![],
                src: src.clone(),
            };
            let void_ty_i = self.code.add_type(void_ty);
            let expect_ty_ins = Code {
                tag: CodeTag::ExpectTypesMatch,
                data: vec![cur_return_ty_i, void_ty_i],
                src: src.clone(),
            };
            // will perform a type check to ensure that the return type
            // of the function is void
            self.code.add_ins(expect_ty_ins);
            self.advance_index();
        }

        let return_ins = Code {
            tag: CodeTag::Return,
            data: indices,
            src,
        };
        Ok(self.code.add_ins(return_ins))
    }

    fn parse_param(&mut self) -> Result<Index, ParseError> {
        // parameters are of the structure: mut? name type
        let mut cur = self.cur_token();
        let mut indices = vec![];
        let mut is_mut = false;
        let mut span = cur.get_source_ref();
        if matches!(cur, Token::Mut(_)) {
            is_mut = true;
            self.advance_index();
            cur = self.cur_token();
            span = span.combine(cur.get_source_ref());
        }

        if let Token::Identifier(name, _) = cur {
            let name_i = self.code.add_string(name);
            indices.push(name_i);
            self.advance_index();
        } else {
            return Err(ParseError::Expected(
                "an identifier to name function parameter.".into(),
                self.cur_token().get_source_ref(),
                Some("Provide a name for the function parameter. E.g: 'fn my_function(my_param i32) void {}'".into()),
            ));
        }

        let type_i = self.parse_type(false)?;
        let ty_src = self.code.types.get(type_i.index).unwrap().src.clone();
        span = span.combine(ty_src);
        indices.push(type_i);

        let param_ins = Code {
            tag: if is_mut {
                CodeTag::VarParam
            } else {
                CodeTag::Param
            },
            data: indices,
            src: span,
        };
        self.code.add_ins(param_ins);
        Ok(type_i)
    }

    fn parse_const_var(&mut self) -> Result<Index, ParseError> {
        let start = self.cur_token();
        self.advance_index(); // skip past `let`

        let mut cur = self.cur_token();
        let mut indices = vec![];
        if let Token::Identifier(name, _) = cur {
            let name_i = self.code.add_string(name);
            indices.push(name_i);
            self.advance_index();
            cur = self.cur_token();
        } else {
            return Err(ParseError::Expected(
                "an identifier to name constant.".into(),
                self.cur_token().get_source_ref(),
                Some("Provide a name for the constant. E.g: 'let my_constant = value;'".into()),
            ));
        }

        if !matches!(cur, Token::Assign(_)) {
            // we should parse a type then
            let type_i = self.parse_type(false)?;
            indices.push(type_i);
        }

        cur = self.cur_token();
        if matches!(cur, Token::Assign(_)) {
            self.advance_index(); // skip past `=`
        } else {
            return Err(ParseError::ConstantDeclarationNeedsTypeOrInitValue(
                self.cur_token()
                    .get_source_ref()
                    .combine(start.get_source_ref()),
            ));
        }

        let value_i = self.parse_expr()?;
        indices.push(value_i);

        cur = self.cur_token();
        if !matches!(cur, Token::Semicolon(_)) {
            return Err(ParseError::Expected(
                "a semicolon to end constant declaration.".into(),
                cur.get_source_ref(),
                Some("Provide a semicolon to end the constant declaration. E.g: 'let my_constant = value;'".into()),
            ));
        }

        let mut span = start.get_source_ref();
        span = span.combine(cur.get_source_ref());
        self.advance_index(); // skip past `;`

        let ins = Code {
            tag: CodeTag::NewConstant,
            data: indices,
            src: span,
        };
        Ok(self.code.add_ins(ins))
    }

    fn parse_mut_var(&mut self) -> Result<Index, ParseError> {
        let start = self.cur_token();
        self.advance_index(); // skip past `mut`

        let mut cur = self.cur_token();
        let mut indices = vec![];
        if let Token::Identifier(name, _) = cur {
            let name_i = self.code.add_string(name);
            indices.push(name_i);
            self.advance_index();
            cur = self.cur_token();
        } else {
            return Err(ParseError::Expected(
                "an identifier to name variable.".into(),
                self.cur_token().get_source_ref(),
                Some("Provide a name for the variable. E.g: 'mut my_variable = value;'".into()),
            ));
        }

        let mut is_typed = false;
        if !matches!(cur, Token::Assign(_)) {
            // we should parse a type then
            let type_i = self.parse_type(false)?;
            indices.push(type_i);
            is_typed = true;
        }

        cur = self.cur_token();
        if matches!(cur, Token::Assign(_)) {
            self.advance_index(); // skip past `=`
            let value_i = self.parse_expr()?;
            indices.push(value_i);
            cur = self.cur_token();
        }

        if !matches!(cur, Token::Semicolon(_)) {
            let tip =
                        format!("This variable can be declared by writing: 'mut var_name type? (= expr)?;'. ? means optional.");
            return Err(ParseError::MalformedDeclaration(tip, cur.get_source_ref()));
        }

        let mut span = start.get_source_ref();
        span = span.combine(cur.get_source_ref());
        self.advance_index(); // skip past `;`

        let ins = Code {
            tag: if is_typed {
                CodeTag::NTVariable
            } else {
                CodeTag::NUVariable
            },
            data: indices,
            src: span,
        };
        Ok(self.code.add_ins(ins))
    }

    fn parse_expr(&mut self) -> Result<Index, ParseError> {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<Index, ParseError> {
        let mut left = self.parse_and()?;
        while matches!(self.cur_token(), Token::Or(_)) {
            let op = self.cur_token();
            self.advance_index();
            let right = self.parse_and()?;
            let ins = Code {
                tag: CodeTag::Or,
                data: vec![left, right],
                src: op.get_source_ref(),
            };
            left = self.code.add_ins(ins);
        }
        Ok(left)
    }

    fn parse_and(&mut self) -> Result<Index, ParseError> {
        let mut left = self.parse_equality()?;
        while matches!(self.cur_token(), Token::And(_)) {
            let op = self.cur_token();
            self.advance_index();
            let right = self.parse_equality()?;
            let ins = Code {
                tag: CodeTag::And,
                data: vec![left, right],
                src: op.get_source_ref(),
            };
            left = self.code.add_ins(ins);
        }
        Ok(left)
    }

    fn parse_equality(&mut self) -> Result<Index, ParseError> {
        let mut left = self.parse_comparison()?;
        while matches!(self.cur_token(), Token::Equal(_) | Token::NotEqual(_)) {
            let op = self.cur_token();
            self.advance_index();
            let right = self.parse_comparison()?;
            let ins = Code {
                tag: match op {
                    Token::Equal(_) => CodeTag::Eq,
                    Token::NotEqual(_) => CodeTag::Neq,
                    _ => unreachable!(),
                },
                data: vec![left, right],
                src: op.get_source_ref(),
            };
            left = self.code.add_ins(ins);
        }
        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<Index, ParseError> {
        let mut left = self.parse_term()?;
        while matches!(
            self.cur_token(),
            Token::Less(_) | Token::LessEqual(_) | Token::Greater(_) | Token::GreaterEqual(_)
        ) {
            let op = self.cur_token();
            self.advance_index();
            let right = self.parse_term()?;
            let ins = Code {
                tag: match op {
                    Token::Less(_) => CodeTag::Lt,
                    Token::LessEqual(_) => CodeTag::LtEq,
                    Token::Greater(_) => CodeTag::Gt,
                    Token::GreaterEqual(_) => CodeTag::GtEq,
                    _ => unreachable!(),
                },
                data: vec![left, right],
                src: op.get_source_ref(),
            };
            left = self.code.add_ins(ins);
        }
        Ok(left)
    }

    fn parse_term(&mut self) -> Result<Index, ParseError> {
        let mut left = self.parse_factor()?;
        while matches!(self.cur_token(), Token::Plus(_) | Token::Minus(_)) {
            let op = self.cur_token();
            self.advance_index();
            let right = self.parse_factor()?;
            let ins = Code {
                tag: match op {
                    Token::Plus(_) => CodeTag::Add,
                    Token::Minus(_) => CodeTag::Sub,
                    _ => unreachable!(),
                },
                data: vec![left, right],
                src: op.get_source_ref(),
            };
            left = self.code.add_ins(ins);
        }
        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<Index, ParseError> {
        let mut left = self.parse_unary()?;
        while matches!(
            self.cur_token(),
            Token::Star(_) | Token::Slash(_) | Token::Modulo(_)
        ) {
            let op = self.cur_token();
            self.advance_index();
            let right = self.parse_unary()?;
            let ins = Code {
                tag: match op {
                    Token::Star(_) => CodeTag::Mult,
                    Token::Slash(_) => CodeTag::Div,
                    Token::Modulo(_) => CodeTag::Modulo,
                    _ => unreachable!(),
                },
                data: vec![left, right],
                src: op.get_source_ref(),
            };
            left = self.code.add_ins(ins);
        }
        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Index, ParseError> {
        let cur = self.cur_token();
        match cur {
            Token::Minus(_) => {
                self.advance_index();
                let right = self.parse_unary()?;
                let ins = Code {
                    tag: CodeTag::Negate,
                    data: vec![right],
                    src: cur.get_source_ref(),
                };
                Ok(self.code.add_ins(ins))
            }
            Token::Not(_) => {
                self.advance_index();
                let right = self.parse_unary()?;
                let ins = Code {
                    tag: CodeTag::Not,
                    data: vec![right],
                    src: cur.get_source_ref(),
                };
                Ok(self.code.add_ins(ins))
            }
            _ => self.parse_index_expr(),
        }
    }

    fn parse_index_expr(&mut self) -> Result<Index, ParseError> {
        let mut left = self.parse_primary()?;
        while matches!(
            self.cur_token(),
            Token::LParen(_) | Token::Dot(_) | Token::LBracket(_)
        ) {
            let op = self.cur_token();
            self.advance_index();
            match op {
                Token::LParen(_) => {
                    let mut args = vec![left];
                    let mut too_many_args = false;
                    while !self.no_more_tokens() {
                        let mut cur = self.cur_token();
                        if !too_many_args && args.len() >= 20 {
                            too_many_args = true;
                            self.report_error(ParseError::TooManyFnArgs(
                                op.get_source_ref().combine(cur.get_source_ref()),
                            ));
                        }

                        if !matches!(cur, Token::RParen(_)) {
                            let arg = self.parse_expr()?;
                            if !too_many_args {
                                args.push(arg);
                            }
                            cur = self.cur_token();
                        }

                        match cur {
                            Token::RParen(src) => {
                                let ins = Code {
                                    tag: CodeTag::Call,
                                    data: args,
                                    src: op.get_source_ref().combine(src),
                                };
                                left = self.code.add_ins(ins);
                                self.advance_index();
                                // early return to prevent call()() from being parsed
                                return Ok(left);
                            }
                            Token::Comma(_) => {
                                self.advance_index();
                            }
                            _ => return Err(ParseError::Expected(
                                "a ',' to separate arguments or a ')' to terminate function call."
                                    .into(),
                                cur.get_source_ref(),
                                None,
                            )),
                        }
                    }
                }
                Token::LBracket(_) => {
                    let index = self.parse_expr()?;
                    let mut src = op
                        .get_source_ref()
                        .combine(self.cur_token().get_source_ref());

                    if !matches!(self.cur_token(), Token::RBracket(_)) {
                        return Err(ParseError::Expected(
                            "a ']' to terminate array indexing.".into(),
                            self.cur_token().get_source_ref(),
                            None,
                        ));
                    } else {
                        src = src.combine(self.cur_token().get_source_ref());
                        self.advance_index();
                    }

                    let ins = Code {
                        tag: CodeTag::AccessIndex,
                        data: vec![left, index],
                        src,
                    };
                    left = self.code.add_ins(ins);
                }
                Token::Dot(_) => {
                    let mut src = op.get_source_ref();
                    let field = self.parse_primary()?;
                    src = src.combine(self.cur_token().get_source_ref());

                    let ins = Code {
                        tag: CodeTag::AccessMember,
                        data: vec![left, field],
                        src,
                    };
                    left = self.code.add_ins(ins);
                }
                _ => unreachable!(),
            }
        }
        Ok(left)
    }

    pub fn parse_primary(&mut self) -> Result<Index, ParseError> {
        let cur = self.cur_token();

        match cur {
            Token::SingleLineStringLiteral(src, content) => {
                self.advance_index();
                let str_i = self.code.add_string(content);
                let ins = Code {
                    tag: CodeTag::LIStr,
                    data: vec![str_i],
                    src,
                };
                Ok(self.code.add_ins(ins))
            }
            Token::CharLiteral(src, char) => {
                self.advance_index();
                let char_i = self.code.add_string(char.to_string());
                let ins = Code {
                    tag: CodeTag::LIChar,
                    data: vec![char_i],
                    src,
                };
                Ok(self.code.add_ins(ins))
            }
            Token::Identifier(id, src) => {
                self.advance_index();
                let id_i = self.code.add_string(id);
                let ins = Code {
                    tag: CodeTag::NameRef,
                    data: vec![id_i],
                    src,
                };
                Ok(self.code.add_ins(ins))
            }
            Token::NumberLiteral(num, src) => {
                self.advance_index();
                let num_i = self.code.add_string(num);
                let ins = Code {
                    tag: CodeTag::LINum,
                    data: vec![num_i],
                    src,
                };
                Ok(self.code.add_ins(ins))
            }
            Token::True(src) => {
                self.advance_index();
                let ins = Code {
                    tag: CodeTag::LoadTrue,
                    data: vec![],
                    src,
                };
                Ok(self.code.add_ins(ins))
            }
            Token::False(src) => {
                self.advance_index();
                let ins = Code {
                    tag: CodeTag::LoadFalse,
                    data: vec![],
                    src,
                };
                Ok(self.code.add_ins(ins))
            }
            Token::LParen(_) => {
                self.advance_index();
                let expr = self.parse_expr()?;
                if !matches!(self.cur_token(), Token::RParen(_)) {
                    return Err(ParseError::Expected(
                        "a ')' to terminate expression.".into(),
                        self.cur_token().get_source_ref(),
                        None,
                    ));
                } else {
                    self.advance_index();
                }
                Ok(expr)
            }
            _ => {
                // since types are values in proto, we can try to parse a
                // type here as the last resort. This means the syntax of a type
                // cannot be ambiguous with any other syntax.
                let maybe_type_i = self.parse_type(true);
                if let Ok(type_i) = maybe_type_i {
                    let ty = self.code.get_ins(type_i);
                    if !matches!(ty.tag, CodeTag::TypeRef) {
                        return Err(ParseError::Expected(
                            "a type.".into(),
                            self.cur_token().get_source_ref(),
                            None,
                        ));
                    }
                    let actual_type_i = ty.data[0];
                    let actual_ty = self.code.types.get(actual_type_i.index).unwrap();
                    match actual_ty.tag {
                        TypeSignatureTag::StaticArrayTS => {
                            // since we have parsed a static array type in an R value position
                            // i.e. as an expression, we need to check if it is immediately followed
                            // by a '{' to indicate that we are initializing an array literal with that
                            // type
                            let cur = self.cur_token();
                            if matches!(cur, Token::LCurly(_)) {
                                let mut src = actual_ty.src.clone();
                                src = src.combine(cur.get_source_ref());
                                self.advance_index();
                                let mut elements = vec![type_i];
                                let mut too_many_elements = false;
                                while !self.no_more_tokens() {
                                    let mut cur = self.cur_token();
                                    if !too_many_elements && elements.len() >= 20 {
                                        too_many_elements = true;
                                        self.report_error(ParseError::TooManyArrayElements(
                                            src.combine(cur.get_source_ref()),
                                        ));
                                    }

                                    if !matches!(cur, Token::RCurly(_)) {
                                        let element = self.parse_expr()?;
                                        if !too_many_elements {
                                            elements.push(element);
                                        }
                                        cur = self.cur_token();
                                    }

                                    match &cur {
                                        Token::RCurly(src) => {
                                            let ins = Code {
                                                tag: CodeTag::MakeStaticArray,
                                                data: elements,
                                                src: src.combine(cur.get_source_ref()),
                                            };
                                            self.advance_index();
                                            return Ok(self.code.add_ins(ins));
                                        }
                                        Token::Comma(_) => {
                                            self.advance_index();
                                        }
                                        _ => {
                                            return Err(ParseError::Expected(
                                                "a ',' to separate array elements or a '}' to terminate array literal."
                                                    .into(),
                                                cur.get_source_ref(),
                                                None,
                                            ))
                                        }
                                    }
                                }

                                return Err(ParseError::Expected(
                                    "a '}' to terminate array literal.".into(),
                                    self.cur_token().get_source_ref(),
                                    None,
                                ));
                            } else {
                                Ok(type_i)
                            }
                        }
                        _ => Ok(type_i),
                    }
                } else {
                    let err = maybe_type_i.unwrap_err();
                    Err(ParseError::CannotParseAnExpressionOrType(
                        err.get_error_src(),
                    ))
                }
            }
        }
    }

    pub fn parse_type(&mut self, generate_value: bool) -> Result<Index, ParseError> {
        let cur = self.cur_token();
        self.advance_index();
        match cur {
            Token::Identifier(name, src) => {
                let name_i = self.code.add_string(name);
                let n_type = TypeSignature {
                    tag: TypeSignatureTag::TypeNameRefTS,
                    src: src.clone(),
                    indices: vec![name_i],
                };
                let n_type_i = self.code.add_type(n_type);
                if !generate_value {
                    return Ok(n_type_i);
                }
                let ins = Code {
                    tag: CodeTag::TypeRef,
                    data: vec![n_type_i],
                    src,
                };
                Ok(self.code.add_ins(ins))
            }
            Token::LBracket(src) => {
                // parse array type, iteratively
                // [SizeN]SomeType
                let size_i = self.parse_expr()?;
                let cur = self.cur_token();
                let mut span = src.combine(cur.get_source_ref());
                if !matches!(cur, Token::RBracket(_)) {
                    return Err(ParseError::Expected(
                        "a ']' to terminate array type.".into(),
                        cur.get_source_ref(),
                        None,
                    ));
                } else {
                    span = span.combine(cur.get_source_ref());
                    self.advance_index();
                }
                let inner_type_i = self.parse_type(false)?;
                let inner_type = self.code.types.get(inner_type_i.index).unwrap();
                span = span.combine(inner_type.src.clone());

                let n_type = TypeSignature {
                    tag: TypeSignatureTag::StaticArrayTS,
                    src: span.clone(),
                    indices: vec![size_i, inner_type_i],
                };
                let n_type_i = self.code.add_type(n_type);
                if !generate_value {
                    return Ok(n_type_i);
                }
                let ins = Code {
                    tag: CodeTag::TypeRef,
                    data: vec![n_type_i],
                    src: span,
                };
                Ok(self.code.add_ins(ins))
            }
            Token::I8(src) => {
                let n_type = TypeSignature {
                    tag: TypeSignatureTag::I8TS,
                    src: src.clone(),
                    indices: vec![],
                };
                let n_type_i = self.code.add_type(n_type);
                if !generate_value {
                    return Ok(n_type_i);
                }
                let ins = Code {
                    tag: CodeTag::TypeRef,
                    data: vec![n_type_i],
                    src,
                };
                Ok(self.code.add_ins(ins))
            }
            Token::I16(src) => {
                let n_type = TypeSignature {
                    tag: TypeSignatureTag::I16TS,
                    src: src.clone(),
                    indices: vec![],
                };
                let n_type_i = self.code.add_type(n_type);
                if !generate_value {
                    return Ok(n_type_i);
                }
                let ins = Code {
                    tag: CodeTag::TypeRef,
                    data: vec![n_type_i],
                    src,
                };
                Ok(self.code.add_ins(ins))
            }
            Token::I32(src) => {
                let n_type = TypeSignature {
                    tag: TypeSignatureTag::I32TS,
                    src: src.clone(),
                    indices: vec![],
                };
                let n_type_i = self.code.add_type(n_type);
                if !generate_value {
                    return Ok(n_type_i);
                }
                let ins = Code {
                    tag: CodeTag::TypeRef,
                    data: vec![n_type_i],
                    src,
                };
                Ok(self.code.add_ins(ins))
            }
            Token::I64(src) => {
                let n_type = TypeSignature {
                    tag: TypeSignatureTag::I64TS,
                    src: src.clone(),
                    indices: vec![],
                };
                let n_type_i = self.code.add_type(n_type);
                if !generate_value {
                    return Ok(n_type_i);
                }
                let ins = Code {
                    tag: CodeTag::TypeRef,
                    data: vec![n_type_i],
                    src,
                };
                Ok(self.code.add_ins(ins))
            }
            Token::Int(src) => {
                let n_type = TypeSignature {
                    tag: TypeSignatureTag::IntTS,
                    src: src.clone(),
                    indices: vec![],
                };
                let n_type_i = self.code.add_type(n_type);
                if !generate_value {
                    return Ok(n_type_i);
                }
                let ins = Code {
                    tag: CodeTag::TypeRef,
                    data: vec![n_type_i],
                    src,
                };
                Ok(self.code.add_ins(ins))
            }
            Token::U8(src) => {
                let n_type = TypeSignature {
                    tag: TypeSignatureTag::U8TS,
                    src: src.clone(),
                    indices: vec![],
                };
                let n_type_i = self.code.add_type(n_type);
                if !generate_value {
                    return Ok(n_type_i);
                }
                let ins = Code {
                    tag: CodeTag::TypeRef,
                    data: vec![n_type_i],
                    src,
                };
                Ok(self.code.add_ins(ins))
            }
            Token::U16(src) => {
                let n_type = TypeSignature {
                    tag: TypeSignatureTag::U16TS,
                    src: src.clone(),
                    indices: vec![],
                };
                let n_type_i = self.code.add_type(n_type);
                if !generate_value {
                    return Ok(n_type_i);
                }
                let ins = Code {
                    tag: CodeTag::TypeRef,
                    data: vec![n_type_i],
                    src,
                };
                Ok(self.code.add_ins(ins))
            }
            Token::U32(src) => {
                let n_type = TypeSignature {
                    tag: TypeSignatureTag::U32TS,
                    src: src.clone(),
                    indices: vec![],
                };
                let n_type_i = self.code.add_type(n_type);
                if !generate_value {
                    return Ok(n_type_i);
                }
                let ins = Code {
                    tag: CodeTag::TypeRef,
                    data: vec![n_type_i],
                    src,
                };
                Ok(self.code.add_ins(ins))
            }
            Token::U64(src) => {
                let n_type = TypeSignature {
                    tag: TypeSignatureTag::U64TS,
                    src: src.clone(),
                    indices: vec![],
                };
                let n_type_i = self.code.add_type(n_type);
                if !generate_value {
                    return Ok(n_type_i);
                }
                let ins = Code {
                    tag: CodeTag::TypeRef,
                    data: vec![n_type_i],
                    src,
                };
                Ok(self.code.add_ins(ins))
            }
            Token::UInt(src) => {
                let n_type = TypeSignature {
                    tag: TypeSignatureTag::UIntTS,
                    src: src.clone(),
                    indices: vec![],
                };
                let n_type_i = self.code.add_type(n_type);
                if !generate_value {
                    return Ok(n_type_i);
                }
                let ins = Code {
                    tag: CodeTag::TypeRef,
                    data: vec![n_type_i],
                    src,
                };
                Ok(self.code.add_ins(ins))
            }
            Token::Bool(src) => {
                let n_type = TypeSignature {
                    tag: TypeSignatureTag::BoolTS,
                    src: src.clone(),
                    indices: vec![],
                };
                let n_type_i = self.code.add_type(n_type);
                if !generate_value {
                    return Ok(n_type_i);
                }
                let ins = Code {
                    tag: CodeTag::TypeRef,
                    src,
                    data: vec![n_type_i],
                };
                Ok(self.code.add_ins(ins))
            }
            Token::Char(src) => {
                let n_type = TypeSignature {
                    tag: TypeSignatureTag::CharTS,
                    src: src.clone(),
                    indices: vec![],
                };
                let n_type_i = self.code.add_type(n_type);
                if !generate_value {
                    return Ok(n_type_i);
                }
                let ins = Code {
                    src,
                    tag: CodeTag::TypeRef,
                    data: vec![n_type_i],
                };
                Ok(self.code.add_ins(ins))
            }
            Token::Str(src) => {
                let n_type = TypeSignature {
                    tag: TypeSignatureTag::StrTS,
                    src: src.clone(),
                    indices: vec![],
                };
                let n_type_i = self.code.add_type(n_type);
                if !generate_value {
                    return Ok(n_type_i);
                }
                let ins = Code {
                    src,
                    tag: CodeTag::TypeRef,
                    data: vec![n_type_i],
                };
                Ok(self.code.add_ins(ins))
            }
            Token::Void(src) => {
                let n_type = TypeSignature {
                    tag: TypeSignatureTag::VoidTS,
                    src: src.clone(),
                    indices: vec![],
                };
                let n_type_i = self.code.add_type(n_type);
                if !generate_value {
                    return Ok(n_type_i);
                }
                let ins = Code {
                    src,
                    tag: CodeTag::TypeRef,
                    data: vec![n_type_i],
                };
                Ok(self.code.add_ins(ins))
            }
            Token::Type(src) => {
                let n_type = TypeSignature {
                    tag: TypeSignatureTag::TypeTS,
                    src: src.clone(),
                    indices: vec![],
                };
                let n_type_i = self.code.add_type(n_type);
                if !generate_value {
                    return Ok(n_type_i);
                }
                let ins = Code {
                    src,
                    tag: CodeTag::TypeRef,
                    data: vec![n_type_i],
                };
                Ok(self.code.add_ins(ins))
            }
            _ => {
                return Err(ParseError::Expected(
                    "a type signature.".into(),
                    cur.get_source_ref(),
                    None,
                ));
            }
        }
    }
}

#[allow(dead_code)]
#[test]
#[derive(serde::Deserialize, serde::Serialize, Debug)]
struct ParserTestResult {
    code: String,
    lexer_error: Vec<LexError>,
    parser_error: Vec<ParseError>,
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

        parser.parse();
        let module = parser.code;
        let l_errs = parser.lexer_errors;
        let p_errs = parser.parser_errors;

        let res = ParserTestResult {
            code: module.as_str(),
            lexer_error: l_errs,
            parser_error: p_errs,
        };

        insta::assert_yaml_snapshot!(res)
    });
}
