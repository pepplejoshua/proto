use super::{
    ast::{CompilationModule, Expr, Instruction, KeyValueBindings, TypeReference},
    directives::{DIRECTIVES_EXPRS, DIRECTIVES_INS},
    errors::{LexError, ParseError},
    lexer::Lexer,
    source::SourceRef,
    token::Token,
};

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
enum ParseScope {
    TopLevel,
    FnBody,
    Loop,
    CodeBlock,
    Struct,
}

#[allow(dead_code)]
pub struct Parser {
    lexer: Lexer,
    pub lexer_errors: Vec<LexError>,
    pub parser_errors: Vec<ParseError>,
    lexed_token: Option<Token>,
    parse_scope: ParseScope,
    pub compilation_module: CompilationModule,
    allow_directive_expr_use: bool,
}

#[allow(dead_code)]
impl Parser {
    pub fn new(l: Lexer) -> Parser {
        let mut p = Parser {
            lexer: l,
            lexer_errors: vec![],
            parser_errors: vec![],
            lexed_token: None,
            parse_scope: ParseScope::TopLevel,
            compilation_module: CompilationModule::new(),
            allow_directive_expr_use: true,
        };
        p.advance_index();
        p
    }

    pub fn parse(&mut self) {
        while !self.no_more_tokens() {
            let ins = self.next_instruction();

            match ins {
                Ok(instruc) => self.compilation_module.add_instruction(instruc),
                Err(err) => {
                    self.report_error(err.clone());
                    if matches!(
                        err,
                        ParseError::CannotParseAnExpression(_)
                            | ParseError::Expected(_, _, _)
                            | ParseError::NoBreakOutsideLoop(_)
                            | ParseError::NoContinueOutsideLoop(_),
                    ) {
                        self.recover_from_err();
                    }
                }
            }
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

    // this function will return one Instruction at
    // a time.
    fn next_instruction(&mut self) -> Result<Instruction, ParseError> {
        let mut cur: Token = self.cur_token();
        match &cur {
            Token::SingleLineComment(src, comment) => {
                self.advance_index();
                Ok(Instruction::SingleLineComment {
                    comment: comment.to_string(),
                    src: src.clone(),
                })
            }
            Token::At(_) => self.parse_directive_instruction(),
            Token::If(_) => self.parse_conditional_branch_ins(),
            Token::Fn(_) => self.parse_fn_def(false, None),
            Token::Let(_) => self.parse_const_decl(false, None),
            // prevent mutable bindings at the top level
            // use a ParseContext type to distinguish between:
            // - top level declarations
            // - grouped code code (control flow or functions or blocks)
            // mutable declarations are allowed in grouped code but not
            // top level code
            Token::Mut(_) => {
                let var = self.parse_var_decl()?;
                if matches!(self.parse_scope, ParseScope::Struct) {
                    // declaring variables at the top level is not allowed
                    Err(ParseError::NoVariableAtCurrentScope(var.source_ref()))
                } else {
                    Ok(var)
                }
            }
            Token::Break(_) => {
                let break_ins = self.parse_break_ins()?;
                if !matches!(self.parse_scope, ParseScope::Loop) {
                    Err(ParseError::NoBreakOutsideLoop(break_ins.source_ref()))
                } else {
                    Ok(break_ins)
                }
            }
            Token::Continue(_) => {
                let continue_ins = self.parse_continue_ins()?;
                if !matches!(self.parse_scope, ParseScope::Loop) {
                    Err(ParseError::NoContinueOutsideLoop(continue_ins.source_ref()))
                } else {
                    Ok(continue_ins)
                }
            }
            Token::Loop(_) => {
                let inf_loop = self.parse_inf_loop()?;
                if matches!(self.parse_scope, ParseScope::TopLevel) {
                    Err(ParseError::NoLoopAtTopLevel(inf_loop.source_ref()))
                } else {
                    Ok(inf_loop)
                }
            }
            Token::While(_) => {
                let while_loop = self.parse_while_loop()?;
                if matches!(self.parse_scope, ParseScope::TopLevel) {
                    Err(ParseError::NoLoopAtTopLevel(while_loop.source_ref()))
                } else {
                    Ok(while_loop)
                }
            }
            Token::LCurly(_) => {
                let blk = self.parse_code_block()?;
                if matches!(self.parse_scope, ParseScope::TopLevel | ParseScope::Struct) {
                    // declaring code blocks at the top level is not allowed
                    Err(ParseError::NoCodeBlockAllowedInCurrentContext(
                        blk.source_ref(),
                    ))
                } else {
                    Ok(blk)
                }
            }
            Token::Return(_) => {
                let mut ret_ref = cur.get_source_ref();
                self.advance_index();
                let cur = self.cur_token();
                let value = if let Token::Semicolon(_) = cur {
                    self.advance_index();
                    ret_ref = ret_ref.combine(cur.get_source_ref());
                    // return with no value
                    None
                } else {
                    // return with a value
                    let res = self.parse_expr()?;
                    // make sure there is a semi colon following.
                    // if not, report an error
                    let cur = self.cur_token();
                    if let Token::Semicolon(_) = cur {
                        self.advance_index();
                        ret_ref = ret_ref.combine(cur.get_source_ref());
                        Some(res)
                    } else {
                        let tip = format!(
                            "Terminate return statement with ';'. E.g: return {};",
                            res.as_str()
                        );
                        return Err(ParseError::Expected(
                            "a ';' to terminate return instruction.".to_string(),
                            cur.get_source_ref(),
                            Some(tip),
                        ));
                    }
                };

                // we can only return things in functions
                if matches!(self.parse_scope, ParseScope::FnBody) {
                    Ok(Instruction::Return {
                        src: ret_ref,
                        value,
                    })
                } else {
                    Err(ParseError::ReturnInstructionOutsideFunction(ret_ref))
                }
            }
            Token::Pub(_) => {
                // collect pub and then check what type of
                // declaration is coming next
                // public declaration in a file can include:
                // - functions
                // - modules
                // - user defined types (structs and in the future, enums)
                // - constant declarations (let bindings)
                let pub_ref = cur.get_source_ref();
                self.advance_index();
                cur = self.cur_token();
                match cur {
                    Token::Fn(_) => self.parse_fn_def(true, Some(pub_ref)),
                    Token::Let(_) => self.parse_const_decl(true, Some(pub_ref)),
                    _ => {
                        let ins = self.next_instruction()?;
                        Err(ParseError::MisuseOfPubKeyword(
                            ins.source_ref().combine(pub_ref),
                        ))
                    }
                }
            }
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

    fn parse_code_block(&mut self) -> Result<Instruction, ParseError> {
        let prev_scope = self.parse_scope;
        if !matches!(self.parse_scope, ParseScope::FnBody | ParseScope::Loop) {
            self.parse_scope = ParseScope::CodeBlock;
        }
        let mut block_ref = self.cur_token().get_source_ref();
        self.advance_index();

        let mut instructions: Vec<Instruction> = vec![];
        let mut cur = self.cur_token();
        while !self.no_more_tokens() && !matches!(cur, Token::RCurly(_)) {
            let instruction = self.next_instruction();
            if instruction.is_err() {
                // record the error and continue parsing
                self.report_error(instruction.err().unwrap());
                self.recover_from_err();
                // update the current token
                cur = self.cur_token();
                continue;
            }
            instructions.push(instruction.unwrap());
            cur = self.cur_token();
        }

        if matches!(cur, Token::RCurly(_)) {
            block_ref = block_ref.combine(cur.get_source_ref());
            self.advance_index();
        } else {
            block_ref = block_ref.combine(cur.get_source_ref());
            let tip = "Terminate code block with '}'. E.g: '{ // content }'.".to_string();
            return Err(ParseError::UnterminatedCodeBlock(block_ref, Some(tip)));
        }
        self.parse_scope = prev_scope;
        Ok(Instruction::CodeBlock {
            src: block_ref,
            instructions,
        })
    }

    fn parse_directive_instruction(&mut self) -> Result<Instruction, ParseError> {
        let start = self.cur_token();
        // skip the '@' token
        self.advance_index();
        let cur = self.cur_token();
        if let Token::Identifier(name, _) = &cur {
            let binding = DIRECTIVES_INS.get(name);
            match binding {
                Some(needs_additional_ins) => {
                    let directive = self.parse_id(false)?;
                    let mut ins = None;
                    let mut span = start.get_source_ref();
                    if *needs_additional_ins {
                        let body = self.next_instruction()?;
                        span = span.combine(body.source_ref());
                        ins = Some(Box::new(body));
                    }
                    let direc = Instruction::DirectiveInstruction {
                        directive,
                        src: span,
                        block: ins,
                    };
                    Ok(direc)
                }
                None => Err(ParseError::UnknownCompilerDirective(cur.get_source_ref())),
            }
        } else {
            Err(ParseError::Expected(
                "an identifier to name the directive after '@'".to_string(),
                cur.get_source_ref(),
                Some("Provide a name for the directive. E.g: '@run { ... }'".to_string()),
            ))
        }
    }

    fn parse_fn_def(
        &mut self,
        is_public: bool,
        pub_ref: Option<SourceRef>,
    ) -> Result<Instruction, ParseError> {
        let temp_scope = self.parse_scope;
        self.parse_scope = ParseScope::FnBody;
        let start = self.cur_token();
        self.advance_index();

        let mut cur = self.cur_token();
        let label;
        if let Token::Identifier(_, _) = &cur {
            label = Some(cur.clone());
            self.advance_index();
            cur = self.cur_token();
        } else {
            return Err(ParseError::Expected(
                "an identifier to name function.".into(),
                self.cur_token().get_source_ref(),
                Some("Provide a name for the function. E.g: 'fn func_name() {}'".into()),
            ));
        }

        // skip opening brace
        if matches!(cur, Token::LParen(_)) {
            self.advance_index();
            cur = self.cur_token();
        } else {
            return Err(ParseError::Expected(
                "a '(' to denote start of function parameters, if any.".into(),
                self.cur_token().get_source_ref(),
                Some("Provide '()' around parameters (if any). E.g: 'fn func_name() {} or fn test_(a i64) {}'".into()),
            ));
        }

        // parse the parameters of the function
        let mut params: Vec<Expr> = vec![];
        let mut too_many_params = false;
        'parameters: while !self.no_more_tokens() {
            if !too_many_params && params.len() > 256 {
                self.report_error(ParseError::TooManyFnParams(
                    start.get_source_ref().combine(cur.get_source_ref()),
                ));
                too_many_params = true;
            }

            // TODO: fn some_name(mut param type = init, param_b type = init, param_c type); reuse code from parse_var_decl

            cur = self.cur_token();
            if !matches!(cur, Token::RParen(_)) {
                let param = self.parse_id(true)?;
                if !too_many_params {
                    params.push(param);
                }
                cur = self.cur_token();
            }

            match cur {
                Token::RParen(_) => {
                    self.advance_index();
                    break 'parameters;
                }
                Token::Comma(_) => {
                    self.advance_index();
                    continue 'parameters;
                }
                _ => {
                    return Err(ParseError::Expected(
                        "a ',' to separate parameters or a ')' to terminate parameter list.".into(),
                        start.get_source_ref().combine(cur.get_source_ref()),
                        None,
                    ))
                }
            }
        }

        // parse return type
        cur = self.cur_token();
        let return_type;
        if let Some(sem_type) = self.parse_type() {
            return_type = Some(sem_type);
        } else {
            // report that a type is expected
            return Err(ParseError::Expected(
                "a return type for this function.".into(),
                start.get_source_ref().combine(cur.get_source_ref()),
                Some("If it returns nothing, use the 'void' type as its return type.".into()),
            ));
        }

        // check if the next token is a semi-colon. If it is,
        // then this is a function prototype, not a function definition
        // so we just return a function prototype instruction
        // and don't parse the function body
        cur = self.cur_token();
        if matches!(cur, Token::Semicolon(_)) {
            self.advance_index();
            self.parse_scope = temp_scope;
            let ret_type = return_type.unwrap();
            let mut fn_ref = start.get_source_ref().combine(cur.get_source_ref());
            if is_public {
                fn_ref = fn_ref.combine(pub_ref.unwrap());
            }
            return Ok(Instruction::FunctionPrototype {
                name: label.unwrap(),
                params,
                return_type: ret_type,
                is_public,
                src: fn_ref,
            });
        }

        // parse function body, which is just an instruction
        // set the scope of this instruction to be Fn (to allow parsing variables)
        let fn_body = self.next_instruction()?;
        self.parse_scope = temp_scope;
        let ret_type = return_type.unwrap();
        let mut fn_ref = start.get_source_ref().combine(fn_body.source_ref());
        if is_public {
            fn_ref = fn_ref.combine(pub_ref.unwrap());
        }
        Ok(Instruction::FunctionDef {
            name: label.unwrap(),
            params,
            return_type: ret_type,
            body: Box::new(fn_body),
            is_public,
            src: fn_ref,
        })
    }

    fn parse_type(&mut self) -> Option<TypeReference> {
        let cur = self.cur_token();
        let start = cur.get_source_ref();

        match cur {
            Token::Identifier(_, _) => {
                let id = cur.as_str();
                self.advance_index();
                Some(TypeReference::IdentifierType(id, Some(start)))
            }
            _ => None,
        }
    }

    fn parse_const_decl(
        &mut self,
        is_public: bool,
        pub_ref: Option<SourceRef>,
    ) -> Result<Instruction, ParseError> {
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
            return Err(ParseError::Expected(
                "an identifier to name constant.".into(),
                self.cur_token().get_source_ref(),
                Some("Provide a name for the constant. E.g: 'let my_constant = value;'".into()),
            ));
        }

        let mut const_type = None;
        if let Some(sem_type) = self.parse_type() {
            const_type = Some(sem_type);
            cur = self.cur_token();
        }

        if let Token::Assign(_) = cur {
            self.advance_index();
        } else {
            return Err(ParseError::ConstantDeclarationNeedsInitValue(
                self.cur_token()
                    .get_source_ref()
                    .combine(start.get_source_ref()),
            ));
        }

        let init_value = self.parse_expr()?;
        let mut end_ref = start.get_source_ref().combine(init_value.source_ref());
        cur = self.cur_token();
        match cur {
            Token::Semicolon(_) => {
                end_ref = end_ref.combine(cur.get_source_ref());
                self.advance_index();
                let name = label.unwrap();
                if is_public {
                    end_ref = end_ref.combine(pub_ref.unwrap());
                }
                Ok(Instruction::ConstantDecl {
                    const_name: name,
                    const_type,
                    init_expr: init_value,
                    src_ref: end_ref,
                    is_public: false,
                })
            }
            _ => {
                let tip = match const_type {
                    Some(type_t) => {
                        format!(
                            "Terminate constant declaration: 'let {const_name} {} = ...;'",
                            type_t.as_str(),
                        )
                    }
                    None => format!("Terminate constant declaration: 'let {const_name} = ...;'",),
                };
                Err(ParseError::Expected(
                    "';' to terminate constant declaration.".into(),
                    end_ref,
                    Some(tip),
                ))
            }
        }
    }

    fn parse_conditional_branch_ins(&mut self) -> Result<Instruction, ParseError> {
        self.advance_index(); // skip past "if"
        let mut span = self.cur_token().get_source_ref();

        // get condition for if statement
        let if_cond = self.parse_expr()?;
        span = span.combine(if_cond.source_ref());

        // get body of if statement
        let if_body = self.next_instruction()?;
        span = span.combine(if_body.source_ref());

        let mut pairs = vec![(Some(if_cond), Box::new(if_body))];

        // check for else
        let mut cur = self.cur_token();
        while let Token::Else(_) = cur {
            self.advance_index(); // skip past "else"

            // check for if
            cur = self.cur_token();
            if let Token::If(_) = cur {
                self.advance_index(); // skip past "if"

                let else_if_cond = self.parse_expr()?;

                let else_if_body = self.next_instruction()?;
                span = span.combine(else_if_body.source_ref());

                pairs.push((Some(else_if_cond), Box::new(else_if_body)));
                cur = self.cur_token();
            } else {
                // else body
                let else_body = self.next_instruction()?;
                span = span.combine(else_body.source_ref());

                pairs.push((None, Box::new(else_body)));
                break;
            }
        }

        let cond_branch_ins = Instruction::ConditionalBranchIns { pairs, src: span };
        Ok(cond_branch_ins)
    }

    fn parse_break_ins(&mut self) -> Result<Instruction, ParseError> {
        let break_ref = self.cur_token().get_source_ref();
        self.advance_index();
        // check for a semicolon
        let cur = self.cur_token();
        if let Token::Semicolon(_) = cur {
            self.advance_index();
            Ok(Instruction::Break(break_ref.combine(cur.get_source_ref())))
        } else {
            Err(ParseError::Expected(
                "';' to terminate the break statement.".into(),
                cur.get_source_ref(),
                Some("Terminate the break statement with a ';'. E.g: 'break;'".into()),
            ))
        }
    }

    fn parse_continue_ins(&mut self) -> Result<Instruction, ParseError> {
        let continue_ref = self.cur_token().get_source_ref();
        self.advance_index();
        // check for a semicolon
        let cur = self.cur_token();
        if let Token::Semicolon(_) = cur {
            self.advance_index();
            Ok(Instruction::Continue(
                continue_ref.combine(cur.get_source_ref()),
            ))
        } else {
            Err(ParseError::Expected(
                "';' to terminate the continue statement.".into(),
                cur.get_source_ref(),
                Some("Terminate the continue statement with a ';'. E.g: 'continue;'".into()),
            ))
        }
    }

    fn parse_inf_loop(&mut self) -> Result<Instruction, ParseError> {
        let loop_ref = self.cur_token().get_source_ref();
        self.advance_index();

        let temp = self.parse_scope;
        self.parse_scope = ParseScope::Loop;
        let body = self.next_instruction()?;
        self.parse_scope = temp;
        Ok(Instruction::InfiniteLoop {
            src: loop_ref.combine(body.source_ref()),
            body: Box::new(body),
        })
    }

    fn parse_while_loop(&mut self) -> Result<Instruction, ParseError> {
        let loop_ref = self.cur_token().get_source_ref();
        self.advance_index();

        let condition = self.parse_expr()?;
        let temp = self.parse_scope;
        self.parse_scope = ParseScope::Loop;
        let body = self.next_instruction()?;
        self.parse_scope = temp;
        Ok(Instruction::WhileLoop {
            src: loop_ref.combine(body.source_ref()),
            condition,
            body: Box::new(body),
        })
    }

    // parses key-value bindings for structs
    fn parse_key_value_bindings(
        &mut self,
        parse_type: bool,
    ) -> Result<KeyValueBindings, ParseError> {
        let mut span = self.cur_token().get_source_ref();
        let mut bindings = Vec::new();

        self.advance_index(); // skip past "{"
        let mut cur = self.cur_token();
        while let Token::Identifier(_, _) = cur {
            let name = self.parse_id(parse_type)?;
            span = span.combine(name.source_ref());

            cur = self.cur_token();
            if let Token::Assign(_) = cur {
                self.advance_index(); // skip past "="
                let expr = self.parse_expr()?;
                span = span.combine(expr.source_ref());
                bindings.push((name, Some(expr)));
            } else {
                bindings.push((name, None));
            }

            cur = self.cur_token();
            if let Token::Comma(_) = cur {
                self.advance_index(); // skip past ","
                cur = self.cur_token();
                span = span.combine(cur.get_source_ref());
            } else if let Token::RCurly(_) = cur {
                span = span.combine(cur.get_source_ref());
                break;
            } else {
                return Err(ParseError::Expected(
                    "a ',' between fields or '}' to terminate the struct.".into(),
                    cur.get_source_ref(),
                    Some("Terminate the struct field with a ',' or terminate the struct with a '}'. E.g: 'field1 = value, field2 = value }'".into()),
                ));
            }
        }

        self.advance_index(); // skip past "}"
        Ok(KeyValueBindings {
            span,
            pairs: bindings,
        })
    }

    fn parse_var_decl(&mut self) -> Result<Instruction, ParseError> {
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
            return Err(ParseError::Expected(
                "an identifier to name variable.".into(),
                self.cur_token().get_source_ref(),
                Some("Provide a name for the variable. E.g: 'mut my_var = value;'".into()),
            ));
        }

        let mut var_type = None;
        // try to parse an optional type and/or an optional assignment
        // this will be replaced with a call to parse_type(try: true)
        // which will try to parse a type
        if let Some(sem_type) = self.parse_type() {
            var_type = Some(sem_type);
            cur = self.cur_token();
        }

        let mut init_value = None;

        match cur {
            Token::Assign(_) => {
                self.advance_index();
                init_value = Some(self.parse_expr()?);

                // look for a semi colon
                cur = self.cur_token();
                if let Token::Semicolon(_) = cur {
                    let end_ref = cur.get_source_ref();
                    self.advance_index();
                    let name = label.unwrap();
                    Ok(Instruction::VariableDecl(
                        name,
                        var_type,
                        init_value,
                        start.get_source_ref().combine(end_ref),
                    ))
                } else {
                    let tip = match (var_type, init_value) {
                        (None, None) => {
                            format!("Terminate variable declaration: 'mut {var_name};'")
                        }
                        (None, Some(expr)) => {
                            format!(
                                "Terminate variable declaration: 'mut {var_name} = {};'",
                                expr.as_str()
                            )
                        }
                        (Some(v_type), None) => {
                            format!(
                                "Terminate variable declaration: 'mut {var_name} {};'",
                                v_type.as_str()
                            )
                        }
                        (Some(v_type), Some(expr)) => {
                            format!(
                                "Terminate variable declaration: 'mut {var_name} {} = {};'",
                                v_type.as_str(),
                                expr.as_str(),
                            )
                        }
                    };
                    Err(ParseError::Expected(
                        "';' to terminate variable declaration.".into(),
                        cur.get_source_ref(),
                        Some(tip),
                    ))
                }
            }
            Token::Semicolon(_) => {
                let end_ref = cur.get_source_ref();
                self.advance_index();

                // if there is no type, report an error, since there is no
                // way to infer the type of the variable ahead of its first use/assignment
                if var_type.is_none() {
                    let tip =
                        format!("Provide a type for the variable. E.g: 'mut {var_name} type;'.");
                    return Err(ParseError::Expected(
                        "a type for the variable.".into(),
                        cur.get_source_ref(),
                        Some(tip),
                    ));
                }
                let name = label.unwrap();
                Ok(Instruction::VariableDecl(
                    name,
                    var_type,
                    init_value,
                    start.get_source_ref().combine(end_ref),
                ))
            }
            _ => {
                let tip =
                    format!("This variable can be declared by writing: 'mut {var_name} type? (= expr)?;'. ? means optional.");
                Err(ParseError::MalformedDeclaration(tip, cur.get_source_ref()))
            }
        }
    }

    fn parse_assignment_or_expr_instruc(&mut self) -> Result<Instruction, ParseError> {
        let target = self.parse_expr()?;

        let cur = self.cur_token();

        match cur {
            Token::Assign(_) => {
                self.advance_index();
                let value = self.parse_expr()?;
                if let Token::Semicolon(_) = self.cur_token() {
                    self.advance_index();
                } else {
                    let tip = format!(
                        "Terminate assignment instruction: {} = {};",
                        target.as_str(),
                        value.as_str()
                    );
                    return Err(ParseError::Expected(
                        "a ';' to terminate the assignment instruction.".into(),
                        cur.get_source_ref(),
                        Some(tip),
                    ));
                }
                let span = target.source_ref().combine(value.source_ref());
                Ok(Instruction::AssignmentIns(target, value, span))
            }
            Token::Semicolon(_) => {
                let span = target.source_ref().combine(cur.get_source_ref());
                self.advance_index();
                Ok(Instruction::ExpressionIns(target, span))
            }
            _ => Err(ParseError::Expected(
                "a terminated expression or an assignment instruction.".into(),
                cur.get_source_ref(),
                None,
            )),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_and()?;

        loop {
            let cur = self.cur_token();
            match cur {
                Token::Or(_) => {
                    self.advance_index();
                    let rhs = self.parse_and()?;
                    let span = lhs.source_ref().combine(rhs.source_ref());
                    lhs = Expr::Comparison(cur, Box::new(lhs), Box::new(rhs), span);
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_and(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_equality()?;

        loop {
            let cur = self.cur_token();
            match cur {
                Token::And(_) => {
                    self.advance_index();
                    let rhs = self.parse_equality()?;
                    let span = lhs.source_ref().combine(rhs.source_ref());
                    lhs = Expr::Comparison(cur, Box::new(lhs), Box::new(rhs), span);
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_equality(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_comparison()?;

        loop {
            let cur = self.cur_token();
            match cur {
                Token::Equal(_) | Token::NotEqual(_) => {
                    self.advance_index();
                    let rhs = self.parse_comparison()?;
                    let span = lhs.source_ref().combine(rhs.source_ref());
                    lhs = Expr::Comparison(cur, Box::new(lhs), Box::new(rhs), span);
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
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
                    let span = lhs.source_ref().combine(rhs.source_ref());
                    lhs = Expr::Comparison(cur, Box::new(lhs), Box::new(rhs), span);
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_term(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_factor()?;

        loop {
            let cur = self.cur_token();
            match cur {
                Token::Plus(_) | Token::Minus(_) => {
                    self.advance_index();
                    let rhs = self.parse_factor()?;
                    let span = lhs.source_ref().combine(rhs.source_ref());
                    lhs = Expr::Binary(cur, Box::new(lhs), Box::new(rhs), span);
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_unary()?;

        loop {
            let cur = self.cur_token();
            match cur {
                Token::Star(_) | Token::Modulo(_) | Token::Slash(_) => {
                    self.advance_index();
                    let rhs = self.parse_unary()?;
                    let span = lhs.source_ref().combine(rhs.source_ref());
                    lhs = Expr::Binary(cur, Box::new(lhs), Box::new(rhs), span);
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        let cur = self.cur_token();
        match cur {
            Token::Not(_) => {
                self.advance_index();
                let operand = self.parse_unary()?;
                let span = cur.get_source_ref().combine(operand.source_ref());
                Ok(Expr::Unary(cur, Box::new(operand), span))
            }
            _ => self.parse_index_like_exprs(),
        }
    }

    fn parse_index_like_exprs(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_primary()?;

        loop {
            let mut cur = self.cur_token();

            match cur {
                Token::Dot(_) => {
                    // ScopeInto expr
                    self.advance_index();

                    let rhs = self.parse_id(false)?;
                    let span = lhs.source_ref().combine(rhs.source_ref());
                    lhs = Expr::ScopeInto {
                        module: Box::new(lhs),
                        target: Box::new(rhs),
                        src: span,
                    };
                }
                Token::LParen(_) => {
                    self.advance_index();

                    let mut args: Vec<Expr> = vec![];
                    let mut too_many_args = false;
                    'args: while !self.no_more_tokens() {
                        if !too_many_args && args.len() > 256 {
                            self.report_error(ParseError::TooManyFnArgs(
                                lhs.source_ref().combine(cur.get_source_ref()),
                            ));
                            too_many_args = true;
                        }

                        cur = self.cur_token();
                        if !matches!(cur, Token::RParen(_)) {
                            let arg = self.parse_expr()?;
                            if !too_many_args {
                                args.push(arg);
                            }
                            cur = self.cur_token();
                        }

                        match cur {
                            Token::RParen(_) => {
                                lhs = Expr::FnCall {
                                    span: lhs.source_ref().combine(cur.get_source_ref()),
                                    func: Box::new(lhs),
                                    args,
                                };
                                self.advance_index();
                                return Ok(lhs);
                            }
                            Token::Comma(_) => {
                                self.advance_index();
                                continue 'args;
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
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_id(&mut self, with_type: bool) -> Result<Expr, ParseError> {
        let cur = self.cur_token();
        if let Token::Identifier(_, _) = cur {
            self.advance_index();
            if with_type {
                let id = cur;
                if let Some(sem_type) = self.parse_type() {
                    let span = id
                        .get_source_ref()
                        .combine(sem_type.get_source_ref().unwrap());
                    Ok(Expr::Id(id, Some(sem_type), span))
                } else {
                    Err(ParseError::Expected(
                        format!("a type for the preceding identifier, '{}'.", id.as_str()),
                        id.get_source_ref(),
                        Some("Please provide a type for this identifier.".into()),
                    ))
                }
            } else {
                let span = cur.get_source_ref();
                Ok(Expr::Id(cur, None, span))
            }
        } else {
            Err(ParseError::Expected(
                "to parse an identifier.".into(),
                cur.get_source_ref(),
                Some(
                    "An identifier is always required to name declarations/definitions. Please provide one.".to_string()
                ),
            ))
        }
    }

    fn parse_directive_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.cur_token();
        self.advance_index();
        let cur = self.cur_token();
        if let Token::Identifier(name, _) = &cur {
            let binding = DIRECTIVES_EXPRS.get(name);
            match binding {
                Some(needs_additional_expr) => {
                    let directive = self.parse_id(false)?;
                    let mut expr = None;
                    let mut span = self.cur_token().get_source_ref();
                    if *needs_additional_expr {
                        self.allow_directive_expr_use = false;
                        let e = self.parse_expr()?;
                        self.allow_directive_expr_use = true;
                        span = span.combine(e.source_ref());
                        expr = Some(Box::new(e));
                    }
                    let direc = Expr::DirectiveExpr {
                        directive: Box::new(directive),
                        src: start.get_source_ref().combine(span),
                        expr,
                    };
                    Ok(direc)
                }
                None => Err(ParseError::UnknownCompilerDirective(cur.get_source_ref())),
            }
        } else {
            Err(ParseError::Expected(
                "an identifier to name the directive after '@'.".into(),
                cur.get_source_ref(),
                Some("Provide a name for the directive. E.g: '@filename'".into()),
            ))
        }
    }

    fn parse_named_struct_literal(&mut self, name: Expr) -> Result<Expr, ParseError> {
        let mut span = self.cur_token().get_source_ref();
        let fields = self.parse_key_value_bindings(false)?;
        span = span.combine(fields.source_ref());
        let init_named_struct = Expr::NamedStructLiteral {
            name: Box::new(name),
            fields,
            src: span,
        };
        Ok(init_named_struct)
    }

    fn parse_anon_struct_literal(&mut self) -> Result<Expr, ParseError> {
        // unlike struct {} and NameStruct {}, this should allow an
        // optional type on the fields
        let mut span = self.cur_token().get_source_ref();
        self.advance_index(); // consume 'struct'
        let fields = self.parse_key_value_bindings(true)?;
        span = span.combine(fields.source_ref());
        let init_anon_struct = Expr::AnonStructLiteral { fields, src: span };
        Ok(init_anon_struct)
    }

    // struct {} // where {} is a block
    fn parse_struct_decl(&mut self) -> Result<Expr, ParseError> {
        let mut span = self.cur_token().get_source_ref();
        self.advance_index(); // consume 'struct'

        // parse block
        let contents = self.parse_code_block()?;
        span = span.combine(contents.source_ref());
        Ok(Expr::StructDecl {
            contents: Box::new(contents),
            src: span,
        })
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        let cur = self.cur_token();
        let span = cur.get_source_ref();

        match cur {
            Token::Struct(_) => self.parse_struct_decl(),
            Token::Dot(_) => self.parse_anon_struct_literal(),
            Token::At(_) => self.parse_directive_expr(),
            Token::SingleLineStringLiteral(_, _) => {
                let res = Ok(Expr::SingleLineStringLiteral(cur, span));
                self.advance_index();
                res
            }
            Token::MultiLineStringFragment(..) => {
                let mut span = cur.get_source_ref();
                let mut literals = vec![cur];
                self.advance_index();

                while let Token::MultiLineStringFragment(..) = self.cur_token() {
                    let frag = self.cur_token();
                    span = span.combine(frag.get_source_ref());
                    literals.push(frag);
                    self.advance_index();
                } // consume all fragments

                Ok(Expr::MultiLineStringLiteral(literals, span))
            }
            Token::CharLiteral(_, _) => {
                let span = cur.get_source_ref();
                let res = Ok(Expr::CharacterLiteral(cur, span));
                self.advance_index();
                res
            }
            Token::Identifier(_, _) => {
                let id = Expr::Id(cur, None, span);
                self.advance_index();

                if let Token::LCurly(_) = self.cur_token() {
                    // parse named struct literal
                    self.parse_named_struct_literal(id)
                } else {
                    // parse identifier
                    Ok(id)
                }
            }
            Token::Integer(num, src) => {
                let res = Ok(Expr::Integer(num, src));
                self.advance_index();
                res
            }
            Token::True(_) | Token::False(_) => {
                self.advance_index();
                Ok(Expr::Boolean(cur, span))
            }
            Token::LParen(_) => {
                // parse group

                self.advance_index();
                let expr = self.parse_expr()?;
                let maybe_rparen = self.cur_token();
                if let Token::RParen(_) = maybe_rparen {
                    self.advance_index();
                } else {
                    return Err(ParseError::Expected(
                        "a ')' to terminate the grouped expression.".into(),
                        maybe_rparen.get_source_ref(),
                        None,
                    ));
                }
                Ok(Expr::Grouped(
                    Box::new(expr),
                    cur.get_source_ref().combine(maybe_rparen.get_source_ref()),
                ))
            }
            _ => Err(ParseError::CannotParseAnExpression(cur.get_source_ref())),
        }
    }
}

#[allow(dead_code)]
#[derive(serde::Deserialize, serde::Serialize, Debug)]
struct ParserTestResult {
    module: Vec<String>,
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
        let module = parser.compilation_module;
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

        insta::assert_yaml_snapshot!(res)
    });
}
