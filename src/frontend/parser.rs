use super::{
    ast::{CompilationModule, DependencyPath, Expr, Instruction, KeyValueBindings, PathAction},
    directives::{DIRECTIVES_EXPRS, DIRECTIVES_INS},
    errors::{LexError, ParseError},
    lexer::Lexer,
    source::SourceRef,
    token::Token,
    types::Type,
};

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
enum ParseScope {
    TopLevel,
    FnBody,
    Loop,
    CodeBlock,
    Module,
}

#[allow(dead_code)]
pub struct Parser {
    lexer: Lexer,
    pub lexer_errors: Vec<LexError>,
    pub parser_errors: Vec<ParseError>,
    lexed_token: Option<Token>,
    parse_scope: ParseScope,
    compilation_module: CompilationModule,
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
            Token::Colon(_) => self.parse_struct_decl(),
            Token::At(_) => self.parse_directive_instruction(),
            Token::Use(_) => self.parse_use_dependency(),
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
                if matches!(self.parse_scope, ParseScope::TopLevel | ParseScope::Module) {
                    // declaring variables at the top level is not allowed
                    Err(ParseError::NoVariableAtTopLevel(var.source_ref()))
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
                if matches!(self.parse_scope, ParseScope::TopLevel | ParseScope::Module) {
                    // declaring code blocks at the top level is not allowed
                    Err(ParseError::NoCodeBlockAtTopLevel(blk.source_ref()))
                } else {
                    Ok(blk)
                }
            }
            Token::Return(_) => {
                let ret_ref = cur.get_source_ref();
                self.advance_index();
                let value = self.parse_expr()?;

                let cur = self.cur_token();
                let c_ref = ret_ref.combine(cur.get_source_ref());
                if !matches!(cur, Token::Semicolon(_)) {
                    return Err(ParseError::Expected(
                        "a ';' to terminate the return expression.".into(),
                        c_ref,
                        Some(format!(
                            "Terminate return instruction: 'return {};'",
                            value.as_str()
                        )),
                    ));
                } else {
                    self.advance_index();
                }

                // we can only return things in functions
                if matches!(self.parse_scope, ParseScope::FnBody) {
                    Ok(Instruction::Return { src: c_ref, value })
                } else {
                    Err(ParseError::ReturnInstructionOutsideFunction(
                        ret_ref.combine(c_ref),
                    ))
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
                    Token::Mod(_) => self.parse_module(true, Some(pub_ref)),
                    Token::Let(_) => self.parse_const_decl(true, Some(pub_ref)),
                    _ => {
                        let ins = self.next_instruction()?;
                        Err(ParseError::MisuseOfPubKeyword(
                            ins.source_ref().combine(pub_ref),
                        ))
                    }
                }
            }
            Token::Mod(_) => self.parse_module(false, None),
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

    fn parse_module(
        &mut self,
        is_public: bool,
        pub_ref: Option<SourceRef>,
    ) -> Result<Instruction, ParseError> {
        let prev_scope = self.parse_scope;
        self.parse_scope = ParseScope::Module;
        let mut mod_ref = self.cur_token().get_source_ref();
        self.advance_index();

        let mod_name = self.parse_id(false)?;
        let body = self.parse_code_block()?;
        mod_ref = mod_ref.combine(body.source_ref());
        if is_public {
            mod_ref = mod_ref.combine(pub_ref.unwrap());
        }
        self.parse_scope = prev_scope;
        Ok(Instruction::Module {
            name: mod_name,
            body: Box::new(body),
            src: mod_ref,
            is_public,
        })
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
                        let block = self.parse_code_block()?;
                        span = span.combine(block.source_ref());
                        ins = Some(Box::new(block));
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

    fn parse_use_dependency(&mut self) -> Result<Instruction, ParseError> {
        let start = self.cur_token();
        // skip the 'use' keyword
        self.advance_index();

        let mut paths_to_import = vec![];
        while !self.no_more_tokens() {
            let paths = self.parse_path()?;
            paths_to_import.extend(paths);

            // expect a comma
            let cur = self.cur_token();
            if matches!(cur, Token::Comma(_)) {
                self.advance_index();
            } else if matches!(cur, Token::Semicolon(_)) {
                break;
            } else {
                return Err(ParseError::Expected(
                    "Expected ',' to separate paths in use instruction. ".to_string(),
                    cur.get_source_ref(),
                    Some(
                        "Provide a ',' to separate paths. E.g: 'use kids::next::door, cartoon::network;'".to_string(),
                    ),
                ));
            }
        }

        // let paths_to_import = self.parse_path()?;

        // expect a semicolon
        let end = self.cur_token();
        if matches!(end, Token::Semicolon(_)) {
            self.advance_index();
            let use_ins = Instruction::UseDependency {
                paths: paths_to_import,
                src: start.get_source_ref().combine(end.get_source_ref()),
            };
            println!("use ins: {}", use_ins.as_str());
            Ok(use_ins)
        } else {
            Err(ParseError::Expected(
                "Expected ';' to terminate use instruction. ".to_string(),
                end.get_source_ref(),
                Some("Provide a ';'. E.g: 'use kids::next::door;'".to_string()),
            ))
        }
    }

    fn parse_path(&mut self) -> Result<Vec<DependencyPath>, ParseError> {
        let start = self.cur_token();
        // allow parsing:
        // - immediate paths
        // - ^ paths
        // - ! paths
        // - $ paths
        // - @ paths
        // all with or without the 'as' alias
        match start {
            Token::Identifier(_, _) | Token::Caret(_) => self.parse_simple_path_section(true),
            Token::Exclamation(_) | Token::Dollar(_) | Token::At(_) => {
                self.parse_directive_path_section()
            }
            _ => Err(ParseError::UnusualTokenInUsePath(start.get_source_ref())),
        }
    }

    fn parse_directive_path_section(&mut self) -> Result<Vec<DependencyPath>, ParseError> {
        let cur = self.cur_token();
        let mut dep_paths = vec![];
        let mut path_actions = vec![];
        // this will parse one of the following directives:
        // 1. !module_in_current_file
        // 2. $module_in_project_root
        // 3. @module_in_core_library
        match cur {
            Token::Exclamation(_) => {
                // this is a module in the current file
                // skip the '!' token
                self.advance_index();
                let id = self.parse_id(false)?;
                path_actions.push(PathAction::SearchCurrentFileFor(id));
            }
            Token::Dollar(_) => {
                // this is a module in the project root
                // skip the '$' token
                self.advance_index();
                let id = self.parse_id(false)?;
                path_actions.push(PathAction::SearchProjectRootFor(id));
            }
            Token::At(_) => {
                // this is a module in the std
                // skip the '@' token
                self.advance_index();
                let id = self.parse_id(false)?;
                path_actions.push(PathAction::SearchCoreModulesFor(id));
            }
            _ => {
                unreachable!("parse_directive_path_section called with invalid token");
            }
        }

        if let Token::Scope(_) = self.cur_token() {
            self.advance_index();
            let simple_dep_paths = self.parse_simple_path_section(false)?;

            for simple_dep_path in simple_dep_paths {
                let t_dep_path = DependencyPath {
                    actions: path_actions.clone(),
                };

                let combined_path = t_dep_path.combine(&simple_dep_path);
                dep_paths.push(combined_path);
            }
            Ok(dep_paths)
        } else {
            dep_paths.push(DependencyPath {
                actions: path_actions,
            });
            Ok(dep_paths)
        }
    }

    fn parse_simple_path_section(
        &mut self,
        allow_caret: bool,
    ) -> Result<Vec<DependencyPath>, ParseError> {
        let mut cur = self.cur_token();
        let mut dep_paths = vec![];

        let mut match_condition = if allow_caret {
            matches!(cur, Token::Identifier(_, _) | Token::Caret(_))
        } else {
            matches!(cur, Token::Identifier(_, _))
        };
        // this will parse either an identifier or a caret
        if match_condition {
            let mut path_actions = vec![];
            let mut expects_path_section = false;
            while match_condition {
                if let Token::Identifier(_, _) = cur {
                    let id = self.parse_id(false)?;
                    path_actions.push(PathAction::SearchFor(id));
                } else {
                    path_actions.push(PathAction::ToParentDir(cur.get_source_ref()));
                    self.advance_index();
                }

                if let Token::Scope(_) = self.cur_token() {
                    self.advance_index();
                    cur = self.cur_token();
                    match_condition = if allow_caret {
                        matches!(cur, Token::Identifier(_, _) | Token::Caret(_))
                    } else {
                        matches!(cur, Token::Identifier(_, _))
                    };
                    expects_path_section = true;
                } else {
                    expects_path_section = false;
                    break;
                }
            }

            cur = self.cur_token();
            // look for one of:
            // - *
            // - as
            // - [ to start a nested path
            match cur {
                Token::Star(_) => {
                    self.advance_index();
                    path_actions.push(PathAction::ImportAll(cur.get_source_ref()));
                    dep_paths.push(DependencyPath {
                        actions: path_actions,
                    });
                }
                Token::As(_) => {
                    self.advance_index();
                    let alias = self.parse_id(false)?;
                    path_actions.push(PathAction::NameLastItemAs(alias));
                    dep_paths.push(DependencyPath {
                        actions: path_actions,
                    });
                }
                Token::LBracket(_) => {
                    self.advance_index();
                    cur = self.cur_token();
                    while !matches!(cur, Token::RBracket(_)) && !self.no_more_tokens() {
                        let inner_paths = self.parse_simple_path_section(allow_caret)?;
                        for inner_path in inner_paths {
                            let t_dep_path = DependencyPath {
                                actions: path_actions.clone(),
                            };

                            let combined_path = t_dep_path.combine(&inner_path);
                            dep_paths.push(combined_path);
                        }
                        cur = self.cur_token();
                        if matches!(cur, Token::Comma(_)) {
                            self.advance_index();
                            cur = self.cur_token();
                        } else {
                            break;
                        }
                    }
                    if matches!(cur, Token::RBracket(_)) {
                        self.advance_index();
                    } else {
                        return Err(ParseError::Expected(
                            "a ']' to terminate path section.".to_string(),
                            cur.get_source_ref(),
                            Some("Provide a ']'. E.g: '[kids::next::door]'".to_string()),
                        ));
                    }
                }
                _ => {
                    if expects_path_section {
                        let msg = if allow_caret {
                            "a '^', '*', 'as' or '[' to continue use path section.".to_string()
                        } else {
                            "a '*', 'as' or '[' to continue use path section.".to_string()
                        };
                        let tip = if allow_caret {
                            let mut text =
                                "Provide a '^', '*', 'as' or '[' to continue use path section.'"
                                    .to_string();
                            text.push_str("E.g:\n");
                            text.push_str("*  use immediate_module::inner::other;\n");
                            text.push_str("*  use immediate_module::inner as alias;\n");
                            text.push_str("*  use immediate::[use_path1, use_path2, ...];\n");
                            text.push_str("*  use immediate_module;\n");
                            text.push_str("*  use import_everything::*;\n");
                            text.push_str("*  use ^::module_in_parent_dir;\n");
                            text.push_str("*  use ^::module_in_parent_dir::inner;\n");
                            text.push_str("*  use ^::^::module_in_grandparent_dir::inner;\n");
                            text
                        } else {
                            let mut text =
                                "Provide a '*', 'as' or '[' to continue use path section.'"
                                    .to_string();
                            text.push_str("E.g:\n");
                            text.push_str("*  use @array::Array as core_array;\n");
                            text.push_str("*  use @array::Array;\n");
                            text.push_str("*  use @array::[Array, Array2D];\n");
                            text.push_str("*  use @array;\n");
                            text.push_str("*  use !my_module::*;\n");
                            text.push_str("*  use $some_module_in_root::[a, b]");
                            text
                        };
                        return Err(ParseError::Expected(msg, cur.get_source_ref(), Some(tip)));
                    }
                    dep_paths.push(DependencyPath {
                        actions: path_actions,
                    });
                }
            }
            Ok(dep_paths)
        } else {
            Err(ParseError::Expected(
                "an identifier or caret (^) as path section.".to_string(),
                cur.get_source_ref(),
                None,
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
        if cur.is_type_token() {
            return_type = Some(cur.to_type());
            self.advance_index();
        } else {
            // report that a type is expected
            return Err(ParseError::Expected(
                "a return type for this function.".into(),
                start.get_source_ref().combine(cur.get_source_ref()),
                Some("If it returns nothing, use the 'void' type as its return type.".into()),
            ));
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
            return Err(ParseError::ConstantDeclarationNeedsInitValue(
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
                let mut decl_ref = start.get_source_ref().combine(end_ref);
                if is_public {
                    decl_ref = decl_ref.combine(pub_ref.unwrap());
                }
                Ok(Instruction::ConstantDecl {
                    const_name: name,
                    const_type,
                    init_expr: init_value,
                    src_ref: decl_ref,
                    is_public: false,
                })
            }
            _ => {
                let tip = match const_type {
                    Some(type_t) => {
                        format!(
                            "Terminate constant declaration: 'let {const_name} {} = {};'",
                            type_t.as_str(),
                            init_value.as_str()
                        )
                    }
                    None => format!(
                        "Terminate constant declaration: 'let {const_name} = {};'",
                        init_value.as_str()
                    ),
                };
                Err(ParseError::Expected(
                    "';' to terminate constant declaration.".into(),
                    cur.get_source_ref(),
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

    fn parse_struct_decl(&mut self) -> Result<Instruction, ParseError> {
        let mut span = self.cur_token().get_source_ref();
        self.advance_index(); // skip past ":"
        let name = self.parse_id(false)?;
        let key_value_pairs = self.parse_key_value_bindings(true)?;
        span = span.combine(key_value_pairs.source_ref());
        let named_struct = Instruction::NamedStructDecl {
            name,
            fields: key_value_pairs,
            src: span,
        };
        Ok(named_struct)
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
        if cur.is_type_token() {
            var_type = Some(cur.to_type());
            self.advance_index();
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
                Ok(Instruction::AssignmentIns(target, value))
            }
            Token::Semicolon(_) => {
                self.advance_index();
                Ok(Instruction::ExpressionIns(target, cur))
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
                    lhs = Expr::Comparison(cur, Box::new(lhs), Box::new(rhs), None);
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
                    lhs = Expr::Comparison(cur, Box::new(lhs), Box::new(rhs), None);
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
                    lhs = Expr::Comparison(cur, Box::new(lhs), Box::new(rhs), None);
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
                    lhs = Expr::Comparison(cur, Box::new(lhs), Box::new(rhs), None);
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
                    lhs = Expr::Binary(cur, Box::new(lhs), Box::new(rhs), None);
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
                    lhs = Expr::Binary(cur, Box::new(lhs), Box::new(rhs), None);
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
                Ok(Expr::Unary(cur, Box::new(operand), None))
            }
            _ => self.parse_index_like_exprs(),
        }
    }

    fn parse_index_like_exprs(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_primary()?;

        'main_loop: loop {
            let mut cur = self.cur_token();

            match cur {
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
                                    fn_type: None,
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
                Token::Scope(_) => {
                    // for now, we want to only allow:
                    // 1. `foo::bar()` (function call)
                    // 2. `foo::bar` (variable or constant)
                    // 3. `foo::bar::baz` (variable or constant)
                    // 4. `foo::bar::baz()` (function call)
                    // we cannot allow functions be referenced like variables,
                    // or constants.
                    // Make sure lhs is a Identifier or a ScopeInto
                    match lhs {
                        Expr::Id(_, _) => {
                            self.advance_index();
                            let rhs = self.parse_primary()?;
                            match rhs {
                                Expr::Id(_, _) => {
                                    lhs = Expr::ScopeInto {
                                        src: lhs.source_ref().combine(rhs.source_ref()),
                                        module: Box::new(lhs),
                                        target: Box::new(rhs),
                                        resolved_type: None,
                                    };

                                    continue 'main_loop;
                                }
                                _ => {
                                    return Err(ParseError::Expected(
                                        "an identifier after '::'.".into(),
                                        rhs.source_ref(),
                                        None,
                                    ))
                                }
                            }
                        }
                        Expr::ScopeInto { .. } => {
                            self.advance_index();
                            let rhs = self.parse_primary()?;
                            match rhs {
                                Expr::Id(_, _) => {
                                    lhs = Expr::ScopeInto {
                                        src: lhs.source_ref().combine(rhs.source_ref()),
                                        module: Box::new(lhs),
                                        target: Box::new(rhs),
                                        resolved_type: None,
                                    };
                                    continue 'main_loop;
                                }
                                _ => {
                                    return Err(ParseError::Expected(
                                        "an identifier after '::'.".into(),
                                        rhs.source_ref(),
                                        None,
                                    ))
                                }
                            }
                        }
                        _ => {
                            return Err(ParseError::Expected(
                                "an identifier or scope into operation before '::'.".into(),
                                lhs.source_ref(),
                                None,
                            ))
                        }
                    }
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_id(&mut self, with_type: bool) -> Result<Expr, ParseError> {
        let mut cur = self.cur_token();
        if let Token::Identifier(_, _) = cur {
            self.advance_index();
            if with_type {
                let id = cur;
                cur = self.cur_token();
                if cur.is_type_token() {
                    self.advance_index();
                    Ok(Expr::Id(id, Some(cur.to_type())))
                } else {
                    Err(ParseError::Expected(
                        format!(
                            "to parse a type for the preceding identifier, '{}'.",
                            id.as_str()
                        ),
                        id.get_source_ref(),
                        Some("Please provide a type for this identifier.".into()),
                    ))
                }
            } else {
                Ok(Expr::Id(cur, None))
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
                        resolved_type: None,
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

    fn parse_init_named_struct(&mut self) -> Result<Expr, ParseError> {
        let mut span = self.cur_token().get_source_ref();
        self.advance_index(); // consume ':'

        let name = self.parse_id(false)?;
        let fields = self.parse_key_value_bindings(false)?;
        span = span.combine(fields.source_ref());
        let init_named_struct = Expr::NamedStructInit {
            name: Box::new(name),
            fields,
            src: span,
            resolved_type: None,
        };
        Ok(init_named_struct)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        let cur = self.cur_token();

        match cur {
            Token::Colon(_) => self.parse_init_named_struct(),
            Token::At(_) => self.parse_directive_expr(),
            Token::StringLiteral(_, _) => {
                let res = Ok(Expr::StringLiteral(cur, Some(Type::Str)));
                self.advance_index();
                res
            }
            Token::CharLiteral(_, _) => {
                let res = Ok(Expr::CharacterLiteral(cur, Some(Type::Char)));
                self.advance_index();
                res
            }
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
                    return Err(ParseError::Expected(
                        "a ')' to terminate the grouped expression.".into(),
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
