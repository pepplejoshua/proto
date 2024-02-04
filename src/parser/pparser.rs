#![allow(dead_code)]
#![allow(unused_variables)]

use crate::{
    lexer::{lexer::Lexer, token::Token},
    parser::pcode::FnArg,
    source::{
        errors::{LexError, ParseError, ParseWarning},
        source::SourceReporter,
    },
    types::signature::{Sig, Type},
};

use super::pcode::{Expr, ExprLoc, Ins, InsLoc, PCode};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParseScope {
    Global,
    Function,
    Struct,
    Mod,
    Block,
    Loop,
    Directive,
}

pub struct Parser {
    pub lexer: Lexer,
    pub lex_errors: Vec<LexError>,
    pub parse_errors: Vec<ParseError>,
    pub parse_warnings: Vec<ParseWarning>,
    pub pcode: PCode,
    scope: ParseScope,
    last_token: Option<Token>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut p = Parser {
            lexer,
            lex_errors: Vec::new(),
            pcode: PCode::new(),
            parse_errors: Vec::new(),
            parse_warnings: Vec::new(),
            scope: ParseScope::Global,
            last_token: None,
        };

        p.advance();
        p
    }

    fn advance(&mut self) {
        loop {
            match self.lexer.next_token() {
                Ok(token) => {
                    self.last_token = Some(token);
                    break;
                }
                Err(err) => {
                    self.lex_errors.push(err);
                }
            }
        }
    }

    fn cur_token(&self) -> Token {
        self.last_token.clone().unwrap()
    }

    fn at_eof(&self) -> bool {
        matches!(self.cur_token(), Token::Eof(_))
    }

    fn report_error(&mut self, err: ParseError) {
        self.parse_errors.push(err);

        if self.parse_errors.len() > 5 {
            let reporter = SourceReporter::new(self.lexer.src.clone());
            for err in self.lex_errors.iter() {
                reporter.report_lexer_error(err);
            }

            for err in self.parse_errors.iter() {
                reporter.report_parser_error(err.clone());
            }

            for warn in self.parse_warnings.iter() {
                reporter.report_parser_warning(warn.clone());
            }

            let too_many_errors = ParseError::TooManyErrors(self.cur_token().get_source_ref());
            reporter.report_parser_error(too_many_errors);
            std::process::exit(1);
        }
    }

    fn report_warning(&mut self, warn: ParseWarning) {
        self.parse_warnings.push(warn);

        if self.parse_warnings.len() > 5 {
            let reporter = SourceReporter::new(self.lexer.src.clone());
            for err in self.lex_errors.iter() {
                reporter.report_lexer_error(err);
            }

            for err in self.parse_errors.iter() {
                reporter.report_parser_error(err.clone());
            }

            for warn in self.parse_warnings.iter() {
                reporter.report_parser_warning(warn.clone());
            }

            let too_many_warnings = ParseError::TooManyErrors(self.cur_token().get_source_ref());
            reporter.report_parser_error(too_many_warnings);
            std::process::exit(1);
        }
    }

    fn add_ins(&mut self, ins: Ins) -> InsLoc {
        match self.scope {
            ParseScope::Global => self.pcode.add_top_level(ins),
            ParseScope::Function
            | ParseScope::Struct
            | ParseScope::Mod
            | ParseScope::Block
            | ParseScope::Loop
            | ParseScope::Directive => self.pcode.add_sub_ins(ins),
        }
    }

    pub fn parse_file(&mut self) {
        while !self.at_eof() {
            self.next_instruction(true);
        }
    }

    fn next_instruction(&mut self, change_scope: bool) -> InsLoc {
        let cur = self.cur_token();
        match cur {
            Token::SingleLineComment(src, comment) => {
                self.advance();
                self.add_ins(Ins::Comment { comment, loc: src })
            }
            Token::Return(_) => {
                if !matches!(self.scope, ParseScope::Function) {
                    self.report_error(ParseError::ReturnInstructionOutsideFunction(
                        cur.get_source_ref(),
                    ));
                    self.add_ins(Ins::ErrorNode {
                        expectation: "a return statement".to_string(),
                        loc: cur.get_source_ref(),
                    })
                } else {
                    self.advance();
                    // we need to check if there is a semicolon
                    let cur = self.cur_token();
                    if matches!(cur, Token::Semicolon(_)) {
                        self.advance();
                        let span = cur.get_source_ref();
                        self.advance();
                        self.add_ins(Ins::Return {
                            expr: None,
                            loc: span,
                        })
                    } else {
                        let value = self.parse_expr();
                        let value_span = self.pcode.get_source_ref_expr(value);
                        let mut span = cur.get_source_ref().combine(value_span);

                        // we need to skip past the semi colon
                        let cur_t = self.cur_token();
                        if !matches!(cur_t, Token::Semicolon(_)) {
                            self.report_error(ParseError::Expected(
                                "a semicolon to terminate the return statement.".to_string(),
                                cur_t.get_source_ref(),
                                None,
                            ));
                        } else {
                            span = span.combine(cur_t.get_source_ref());
                            self.advance();
                        }
                        self.add_ins(Ins::Return {
                            expr: Some(value),
                            loc: span,
                        })
                    }
                }
            }
            Token::LCurly(_) => {
                if matches!(
                    self.scope,
                    ParseScope::Global | ParseScope::Mod | ParseScope::Struct
                ) {
                    self.report_error(ParseError::NoCodeBlockAllowedInCurrentContext(
                        cur.get_source_ref(),
                    ));
                    self.add_ins(Ins::ErrorNode {
                        expectation: "a declaration but found a block".to_string(),
                        loc: cur.get_source_ref(),
                    })
                } else {
                    self.parse_block(change_scope)
                }
            }
            _ => self.parse_ins(),
        }
    }

    fn parse_ins(&mut self) -> InsLoc {
        let left_i = self.parse_expr();
        let left = self.pcode.get_expr_c(left_i);

        // check if there is a semicolon and return an expr statement if there is
        if matches!(self.cur_token(), Token::Semicolon(_)) {
            let left_span = self.pcode.get_source_ref_expr(left_i);
            let span = left_span.combine(self.cur_token().get_source_ref());
            self.advance();
            return self.add_ins(Ins::ExprIns {
                expr: left_i,
                loc: span,
            });
        }

        match left {
            // declaration of:
            // - variable init:
            // name : type? = value;
            // - constant init:
            // name : type? : value;
            // - function header:
            // name :: fn (args) -> ret_type; for const functions
            // - function definition:
            // name :: fn (args) -> ret_type { code } for const functions
            // name := fn (args) -> ret_type { code } for non-const functions
            // - struct:
            // name :: struct { variable init | constant init | function header | function definition | struct }
            // - module:
            // name :: mod { variable init | constant init | function header | function definition | struct }
            Expr::Ident { name, loc } => {
                let loc = loc.clone();
                let mut cur = self.cur_token();
                let mut span = loc.combine(cur.get_source_ref());

                // since all the declarations follow the same form, we can just parse the first sections
                // and then call the right function to parse the rest

                // we already have the name so we need to see if there is some type provided
                // first a :
                if !matches!(cur, Token::Colon(_)) {
                    self.report_error(ParseError::Expected(
                        "a colon to separate the identifier from its type.".to_string(),
                        cur.get_source_ref(),
                        None,
                    ));
                } else {
                    span = span.combine(cur.get_source_ref());
                    self.advance();
                }

                // then a type or another : (for constants) or a = (for variables)
                cur = self.cur_token();

                // get type
                let ident_ty = if !matches!(cur, Token::Colon(_) | Token::Assign(_)) {
                    let ty = self.parse_type();
                    let ty_span = ty.loc.clone();
                    span = loc.combine(ty_span);
                    ty
                } else {
                    Type {
                        tag: Sig::Infer,
                        name: None,
                        sub_types: vec![],
                        aux_type: None,
                        loc: loc.clone(),
                    }
                };

                // we have handled our optional type, now we need to determine whether it is a variable
                // or a constant
                cur = self.cur_token();
                match cur {
                    Token::Colon(_) => {
                        // constant
                        self.advance();
                        let value = self.parse_decl_value();
                        let value_span = self.pcode.get_source_ref_expr(value);
                        span = span.combine(value_span);
                        self.add_ins(Ins::NewConstant {
                            name: name.clone(),
                            ty: ident_ty,
                            val: value,
                            loc: span,
                        })
                    }
                    Token::Assign(_) => {
                        // variable
                        self.advance();
                        let value = self.parse_decl_value();
                        let value_span = self.pcode.get_source_ref_expr(value);
                        span = span.combine(value_span);
                        self.add_ins(Ins::NewVariable {
                            name: name.clone(),
                            ty: ident_ty,
                            val: Some(value),
                            loc: span,
                        })
                    }
                    _ => {
                        // skip past the semi colon
                        if !matches!(self.cur_token(), Token::Semicolon(_)) {
                            self.report_error(ParseError::Expected(
                                "a semicolon to terminate the declaration.".to_string(),
                                self.cur_token().get_source_ref(),
                                None,
                            ));
                        } else {
                            span = span.combine(self.cur_token().get_source_ref());
                            self.advance();
                        }

                        // this is an uninitialized variable
                        self.add_ins(Ins::NewVariable {
                            name: name.clone(),
                            ty: ident_ty,
                            val: None,
                            loc: span,
                        })
                    }
                }
            }

            // could be one of:
            // - assignment: expr = expr;
            // - expression statement: expr;
            _ => {
                let cur = self.cur_token();
                match cur {
                    Token::Assign(_) => {
                        let op = cur;
                        self.advance();
                        let right_i = self.parse_expr();
                        let right = self.pcode.get_expr(right_i);
                        let left_span = self.pcode.get_source_ref_expr(left_i);
                        let right_span = self.pcode.get_source_ref_expr(right_i);
                        let span = left_span.combine(right_span);
                        self.add_ins(Ins::AssignTo {
                            lhs: left_i,
                            rhs: right_i,
                            loc: span,
                        })
                    }
                    Token::Semicolon(_) => {
                        let left_span = self.pcode.get_source_ref_expr(left_i);
                        let span = left_span.combine(cur.get_source_ref());
                        self.advance();
                        self.add_ins(Ins::ExprIns {
                            expr: left_i,
                            loc: span,
                        })
                    }
                    _ => {
                        let left_span = self.pcode.get_source_ref_expr(left_i);
                        let span = left_span.combine(cur.get_source_ref());
                        // error
                        self.report_error(ParseError::Expected(
                            "a semicolon to terminate the expression instruction or '=' if you intend to assign to this expression.".to_string(),
                            span.clone(),
                            None,
                        ));
                        self.add_ins(Ins::ErrorNode {
                            expectation: "a semicolon to terminate the expression instruction or '=' if you intend to assign to this expression.".to_string(),
                            loc: span,
                        })
                    }
                }
            }
        }
    }

    fn parse_decl_value(&mut self) -> ExprLoc {
        let cur = self.cur_token();
        // this will handle parsing:
        // - functions
        // - structs
        // - modules
        // - expressions if it matches non of the above
        match cur {
            Token::Fn(_) => self.parse_fn(),
            Token::Struct(_) => self.parse_struct(),
            Token::Mod(_) => self.parse_mod(),
            _ => {
                let expr_i = self.parse_expr();
                // we need to skip past the semi colon
                if !matches!(self.cur_token(), Token::Semicolon(_)) {
                    self.report_error(ParseError::Expected(
                        "a semicolon to terminate the declaration.".to_string(),
                        self.cur_token().get_source_ref(),
                        None,
                    ));
                } else {
                    self.advance();
                }
                expr_i
            }
        }
    }

    fn parse_fn(&mut self) -> ExprLoc {
        // fn (arg: type, arg: type, ...) -> type INS
        // where INS is either a block or a single instruction

        let start = self.cur_token();
        let mut span = start.get_source_ref();
        self.advance();

        if !matches!(self.cur_token(), Token::LParen(_)) {
            self.report_error(ParseError::Expected(
                "a left parenthesis to begin the list of argument names.".to_string(),
                self.cur_token().get_source_ref(),
                None,
            ));
            return self.pcode.add_expr(Expr::ErrorNode {
                expectation: "a left parenthesis to begin the list of argument names.".to_string(),
                loc: start.get_source_ref(),
            });
        }

        span = span.combine(self.cur_token().get_source_ref());
        self.advance();

        let mut args = vec![];
        while !self.at_eof() {
            let mut cur = self.cur_token();
            if !matches!(cur, Token::RParen(_)) {
                // TODO(@pepplejoshua): determine if this is required
                // if args.len() == 0 && matches!(self.scope, ParseScope::Struct) {
                // since we are in a struct, we can determine if the first
                // argument is a self reference or not
                // }
                // parse identifier and the type
                let arg_name = self.parse_ident();
                let arg_name_span = self.pcode.get_expr(arg_name).get_source_ref();

                let arg_ty = if !matches!(self.cur_token(), Token::Colon(_)) {
                    self.report_error(ParseError::Expected(
                        "a colon to separate the argument name from its type.".to_string(),
                        self.cur_token().get_source_ref(),
                        None,
                    ));
                    // still attempt to parse the type to recover
                    self.parse_type()
                } else {
                    self.advance();
                    self.parse_type()
                };

                let arg_span = arg_ty.loc.clone().combine(arg_name_span);
                span = span.combine(arg_span.clone());
                args.push(FnArg {
                    name: arg_name,
                    ty: arg_ty,
                    loc: arg_span,
                })
            }

            cur = self.cur_token();

            if matches!(cur, Token::RParen(_)) {
                break;
            }

            if !matches!(cur, Token::Comma(_)) {
                self.report_error(ParseError::Expected(
                    "a comma to separate argument types or a right parenthesis to terminate the list of argument types.".to_string(),
                    cur.get_source_ref(),
                    None,
                ));
                break;
            }

            self.advance();
        }

        if matches!(self.cur_token(), Token::RParen(_)) {
            span = span.combine(self.cur_token().get_source_ref());
            self.advance();
        } else {
            self.report_error(ParseError::Expected(
                "a right parenthesis to terminate the list of argument types.".to_string(),
                self.cur_token().get_source_ref(),
                None,
            ));
        }

        let cur_t = self.cur_token();
        if !matches!(cur_t, Token::Minus(_)) {
            self.report_error(ParseError::Expected(
                "an arrow to separate the argument types from the return type.".to_string(),
                cur_t.get_source_ref(),
                None,
            ));
            return self.pcode.add_expr(Expr::ErrorNode {
                expectation: "an arrow to separate the argument types from the return type."
                    .to_string(),
                loc: span,
            });
        }

        self.advance();
        let cur_t = self.cur_token();
        if !matches!(cur_t, Token::Greater(_)) {
            self.report_error(ParseError::Expected(
                "an arrow to separate the argument types from the return type.".to_string(),
                cur_t.get_source_ref(),
                None,
            ));
            return self.pcode.add_expr(Expr::ErrorNode {
                expectation: "an arrow to separate the argument types from the return type."
                    .to_string(),
                loc: span,
            });
        }

        self.advance();
        let ret_type = self.parse_type();
        let ret_span = ret_type.loc.clone();
        span = span.combine(ret_span);

        let scope = self.scope;
        self.scope = ParseScope::Function;
        let body = self.next_instruction(false);
        self.scope = scope;
        let body_span = self.pcode.get_source_ref(body);
        span = span.combine(body_span);

        self.pcode.add_expr(Expr::NewFunction {
            name: "<anon>".to_string(),
            args,
            ret_ty: ret_type,
            code: body,
            loc: span,
        })
    }

    fn parse_struct(&mut self) -> ExprLoc {
        // struct CODE BLOCK
        let start = self.cur_token();
        let mut span = start.get_source_ref();
        self.advance();

        let scope = self.scope;
        self.scope = ParseScope::Struct;
        let block = self.parse_block(false);
        self.scope = scope;
        let block_span = self.pcode.get_source_ref(block);
        span = span.combine(block_span);

        self.pcode.add_expr(Expr::NewStruct {
            name: "<anon>".to_string(),
            code: block,
            loc: span,
        })
    }

    fn parse_mod(&mut self) -> ExprLoc {
        // mod CODE BLOCK
        let start = self.cur_token();
        let mut span = start.get_source_ref();
        self.advance();

        let scope = self.scope;
        self.scope = ParseScope::Mod;
        let block = self.parse_block(false);
        self.scope = scope;
        let block_span = self.pcode.get_source_ref(block);
        span = span.combine(block_span);

        self.pcode.add_expr(Expr::NewModule {
            name: "<anon>".to_string(),
            code: block,
            loc: span,
        })
    }

    fn parse_block(&mut self, change_scope: bool) -> InsLoc {
        let start = self.cur_token();
        let mut span = start.get_source_ref();
        self.advance();

        let mut code = vec![];

        let scope = if change_scope {
            let temp = self.scope;
            self.scope = ParseScope::Block;
            temp
        } else {
            self.scope
        };
        while !self.at_eof() {
            let cur = self.cur_token();
            if matches!(cur, Token::RCurly(_)) {
                span = span.combine(cur.get_source_ref());
                break;
            }

            let ins = self.next_instruction(true);
            let ins_span = self.pcode.get_source_ref(ins);
            span = span.combine(ins_span);
            code.push(ins);
        }
        if change_scope {
            self.scope = scope;
        }

        if !matches!(self.cur_token(), Token::RCurly(_)) {
            self.report_error(ParseError::UnterminatedCodeBlock(
                self.cur_token().get_source_ref(),
                None,
            ));
        } else {
            span = span.combine(self.cur_token().get_source_ref());
            self.advance();
        }

        self.add_ins(Ins::NewBlock { code, loc: span })
    }

    fn parse_type(&mut self) -> Type {
        let cur = self.cur_token();
        self.advance();
        match cur {
            Token::I8(loc) => Type {
                tag: Sig::I8,
                name: None,
                sub_types: vec![],
                loc,
                aux_type: None,
            },
            Token::I16(loc) => Type {
                tag: Sig::I16,
                name: None,
                sub_types: vec![],
                aux_type: None,
                loc,
            },
            Token::I32(loc) => Type {
                tag: Sig::I32,
                name: None,
                sub_types: vec![],
                aux_type: None,
                loc,
            },
            Token::I64(loc) => Type {
                tag: Sig::I64,
                name: None,
                sub_types: vec![],
                aux_type: None,
                loc,
            },
            Token::Int(loc) => Type {
                tag: Sig::Int,
                name: None,
                sub_types: vec![],
                aux_type: None,
                loc,
            },
            Token::U8(loc) => Type {
                tag: Sig::U8,
                name: None,
                sub_types: vec![],
                aux_type: None,
                loc,
            },
            Token::U16(loc) => Type {
                tag: Sig::U16,
                name: None,
                sub_types: vec![],
                aux_type: None,
                loc,
            },
            Token::U32(loc) => Type {
                tag: Sig::U32,
                name: None,
                sub_types: vec![],
                aux_type: None,
                loc,
            },
            Token::U64(loc) => Type {
                tag: Sig::U64,
                name: None,
                sub_types: vec![],
                aux_type: None,
                loc,
            },
            Token::UInt(loc) => Type {
                tag: Sig::UInt,
                name: None,
                sub_types: vec![],
                aux_type: None,
                loc,
            },
            Token::Char(loc) => Type {
                tag: Sig::Char,
                name: None,
                sub_types: vec![],
                aux_type: None,
                loc,
            },
            Token::Str(loc) => Type {
                tag: Sig::Str,
                name: None,
                sub_types: vec![],
                aux_type: None,
                loc,
            },
            Token::Identifier(name, loc) => Type {
                tag: Sig::Identifier,
                name: Some(name),
                sub_types: vec![],
                aux_type: None,
                loc,
            },
            Token::Void(loc) => Type {
                tag: Sig::Void,
                name: None,
                sub_types: vec![],
                aux_type: None,
                loc,
            },
            Token::LBracket(loc) => {
                todo!("Parser::parse_type: array types")
            }
            Token::Fn(loc) => {
                // this is a complex type of form:
                // fn (type, type, ...) -> type
                let cur_t = self.cur_token();
                let mut span = loc;
                if !matches!(cur_t, Token::LParen(_)) {
                    self.report_error(ParseError::Expected(
                        "a left parenthesis to begin the list of argument types.".to_string(),
                        cur_t.get_source_ref(),
                        None,
                    ));
                    return Type {
                        tag: Sig::ErrorType,
                        name: None,
                        sub_types: vec![],
                        aux_type: None,
                        loc: span,
                    };
                }

                self.advance();
                let mut args = vec![];
                while !self.at_eof() {
                    let cur = self.cur_token();
                    if !matches!(cur, Token::RParen(_)) {
                        let arg = self.parse_type();
                        args.push(arg);
                    }

                    if matches!(cur, Token::RParen(_)) {
                        break;
                    }

                    if !matches!(cur, Token::Comma(_)) {
                        self.report_error(ParseError::Expected(
                            "a comma to separate argument types or a right parenthesis to terminate the list of argument types.".to_string(),
                            cur.get_source_ref(),
                            None,
                        ));
                        break;
                    }

                    self.advance();
                }

                if matches!(self.cur_token(), Token::RParen(_)) {
                    span = span.combine(self.cur_token().get_source_ref());
                    self.advance();
                } else {
                    self.report_error(ParseError::Expected(
                        "a right parenthesis to terminate the list of argument types.".to_string(),
                        self.cur_token().get_source_ref(),
                        None,
                    ));
                }

                let cur_t = self.cur_token();
                if !matches!(cur_t, Token::Minus(_)) {
                    self.report_error(ParseError::Expected(
                        "an arrow to separate the argument types from the return type.".to_string(),
                        cur_t.get_source_ref(),
                        None,
                    ));
                    return Type {
                        tag: Sig::ErrorType,
                        name: None,
                        sub_types: vec![],
                        aux_type: None,
                        loc: span,
                    };
                }

                self.advance();
                let cur_t = self.cur_token();
                if !matches!(cur_t, Token::Greater(_)) {
                    self.report_error(ParseError::Expected(
                        "a right parenthesis to terminate the list of argument types.".to_string(),
                        cur_t.get_source_ref(),
                        None,
                    ));
                    return Type {
                        tag: Sig::ErrorType,
                        name: None,
                        sub_types: vec![],
                        aux_type: None,
                        loc: span,
                    };
                }

                self.advance();
                let ret_type = self.parse_type();
                let ret_span = ret_type.loc.clone();
                span = span.combine(ret_span);

                Type {
                    tag: Sig::Function,
                    name: None,
                    sub_types: args,
                    aux_type: Some(Box::new(ret_type)),
                    loc: span,
                }
            }
            _ => {
                self.report_error(ParseError::CannotParseAType(cur.get_source_ref()));
                Type {
                    tag: Sig::ErrorType,
                    name: None,
                    sub_types: vec![],
                    aux_type: None,
                    loc: cur.get_source_ref(),
                }
            }
        }
    }

    fn parse_expr(&mut self) -> ExprLoc {
        self.parse_or()
    }

    fn parse_or(&mut self) -> ExprLoc {
        let mut left = self.parse_and();

        while matches!(self.cur_token(), Token::Or(_)) {
            let op = self.cur_token();
            self.advance();
            let right = self.parse_and();
            let left_span = self.pcode.get_source_ref_expr(left);
            let right_span = self.pcode.get_source_ref_expr(right);
            let span = left_span.combine(right_span);
            left = self.pcode.add_expr(Expr::Or {
                lhs: left,
                rhs: right,
                loc: op.get_source_ref(),
            });
        }

        left
    }

    fn parse_and(&mut self) -> ExprLoc {
        let mut left = self.parse_equality();

        while matches!(self.cur_token(), Token::And(_)) {
            let op = self.cur_token();
            self.advance();
            let right = self.parse_equality();
            let left_span = self.pcode.get_source_ref_expr(left);
            let right_span = self.pcode.get_source_ref_expr(right);
            let span = left_span.combine(right_span);
            left = self.pcode.add_expr(Expr::And {
                lhs: left,
                rhs: right,
                loc: op.get_source_ref(),
            });
        }

        left
    }

    fn parse_equality(&mut self) -> ExprLoc {
        let mut left = self.parse_comparison();

        while matches!(self.cur_token(), Token::Equal(_) | Token::NotEqual(_)) {
            let op = self.cur_token();
            self.advance();
            let right = self.parse_comparison();
            let left_span = self.pcode.get_source_ref_expr(left);
            let right_span = self.pcode.get_source_ref_expr(right);
            let span = left_span.combine(right_span);

            let is_eq = matches!(op, Token::Equal(_));
            if is_eq {
                left = self.pcode.add_expr(Expr::Eq {
                    lhs: left,
                    rhs: right,
                    loc: op.get_source_ref(),
                });
            } else {
                left = self.pcode.add_expr(Expr::Neq {
                    lhs: left,
                    rhs: right,
                    loc: op.get_source_ref(),
                });
            }
        }

        left
    }

    fn parse_comparison(&mut self) -> ExprLoc {
        let mut left = self.parse_term();

        while matches!(
            self.cur_token(),
            Token::Less(_) | Token::LessEqual(_) | Token::Greater(_) | Token::GreaterEqual(_)
        ) {
            let op = self.cur_token();
            self.advance();
            let right = self.parse_term();
            let left_span = self.pcode.get_source_ref_expr(left);
            let right_span = self.pcode.get_source_ref_expr(right);
            let span = left_span.combine(right_span);
            match op {
                Token::Less(_) => {
                    left = self.pcode.add_expr(Expr::Lt {
                        lhs: left,
                        rhs: right,
                        loc: op.get_source_ref(),
                    });
                }
                Token::LessEqual(_) => {
                    left = self.pcode.add_expr(Expr::LtEq {
                        lhs: left,
                        rhs: right,
                        loc: op.get_source_ref(),
                    });
                }
                Token::Greater(_) => {
                    left = self.pcode.add_expr(Expr::Gt {
                        lhs: left,
                        rhs: right,
                        loc: op.get_source_ref(),
                    });
                }
                Token::GreaterEqual(_) => {
                    left = self.pcode.add_expr(Expr::GtEq {
                        lhs: left,
                        rhs: right,
                        loc: op.get_source_ref(),
                    });
                }
                _ => unreachable!("Parser::parse_comparison: unreachable op: {:?}", op),
            }
        }
        left
    }

    fn parse_term(&mut self) -> ExprLoc {
        let mut left = self.parse_factor();

        while matches!(self.cur_token(), Token::Plus(_) | Token::Minus(_)) {
            let op = self.cur_token();
            self.advance();
            let right = self.parse_factor();
            let left_span = self.pcode.get_source_ref_expr(left);
            let right_span = self.pcode.get_source_ref_expr(right);
            let span = left_span.combine(right_span);
            match op {
                Token::Plus(_) => {
                    left = self.pcode.add_expr(Expr::Add {
                        lhs: left,
                        rhs: right,
                        loc: op.get_source_ref(),
                    });
                }
                Token::Minus(_) => {
                    left = self.pcode.add_expr(Expr::Sub {
                        lhs: left,
                        rhs: right,
                        loc: op.get_source_ref(),
                    });
                }
                _ => unreachable!("Parser::parse_term: unreachable op: {:?}", op),
            }
        }
        left
    }

    fn parse_factor(&mut self) -> ExprLoc {
        let mut left = self.parse_unary();

        while matches!(
            self.cur_token(),
            Token::Star(_) | Token::Slash(_) | Token::Modulo(_)
        ) {
            let op = self.cur_token();
            self.advance();
            let right = self.parse_unary();
            let left_span = self.pcode.get_source_ref_expr(left);
            let right_span = self.pcode.get_source_ref_expr(right);
            let span = left_span.combine(right_span);
            match op {
                Token::Star(_) => {
                    left = self.pcode.add_expr(Expr::Mul {
                        lhs: left,
                        rhs: right,
                        loc: op.get_source_ref(),
                    });
                }
                Token::Slash(_) => {
                    left = self.pcode.add_expr(Expr::Div {
                        lhs: left,
                        rhs: right,
                        loc: op.get_source_ref(),
                    });
                }
                Token::Modulo(_) => {
                    left = self.pcode.add_expr(Expr::Mod {
                        lhs: left,
                        rhs: right,
                        loc: op.get_source_ref(),
                    });
                }
                _ => unreachable!("Parser::parse_factor: unreachable op: {:?}", op),
            }
        }
        left
    }

    fn parse_unary(&mut self) -> ExprLoc {
        let op = self.cur_token();
        match op {
            Token::Minus(_) => {
                self.advance();
                let right = self.parse_unary();
                let right_span = self.pcode.get_source_ref_expr(right);
                let span = op.get_source_ref().combine(right_span);
                self.pcode.add_expr(Expr::Negate {
                    loc: op.get_source_ref(),
                    expr: right,
                })
            }
            Token::Not(_) => {
                self.advance();
                let right = self.parse_unary();
                let right_span = self.pcode.get_source_ref_expr(right);
                let span = op.get_source_ref().combine(right_span);
                self.pcode.add_expr(Expr::Not {
                    loc: op.get_source_ref(),
                    expr: right,
                })
            }
            _ => self.parse_index_expr(),
        }
    }

    fn parse_index_expr(&mut self) -> ExprLoc {
        let mut left = self.parse_primary();
        while matches!(
            self.cur_token(),
            Token::LParen(_) | Token::LBracket(_) | Token::Dot(_)
        ) {
            let op = self.cur_token();
            self.advance();
            match op {
                // function call
                Token::LParen(_) => {
                    let mut args = vec![];
                    while !self.at_eof() {
                        let mut cur = self.cur_token();

                        if !matches!(cur, Token::RParen(_)) {
                            let arg = self.parse_expr();
                            args.push(arg);
                            cur = self.cur_token();
                        }

                        if matches!(cur, Token::RParen(_)) {
                            break;
                        }

                        if !matches!(cur, Token::Comma(_)) {
                            self.report_error(ParseError::Expected(
                                "a comma to separate arguments or a right parenthesis to terminate function call.".to_string(),
                                cur.get_source_ref(),
                                None,
                            ));
                            break;
                        }

                        self.advance();
                    }

                    let left_span = self.pcode.get_source_ref_expr(left);
                    let span = left_span.combine(self.cur_token().get_source_ref());

                    if matches!(self.cur_token(), Token::RParen(_)) {
                        self.advance();
                    } else {
                        self.report_error(ParseError::Expected(
                            "a right parenthesis.".to_string(),
                            self.cur_token().get_source_ref(),
                            None,
                        ));
                    }

                    left = self.pcode.add_expr(Expr::CallFunction {
                        func: left,
                        args,
                        loc: span,
                    });
                }
                // array index
                Token::LBracket(_) => {
                    let index = self.parse_expr();
                    let left_span = self.pcode.get_source_ref_expr(left);
                    let right_span = self.pcode.get_source_ref_expr(index);
                    let mut span = left_span.combine(right_span);
                    if matches!(self.cur_token(), Token::RBracket(_)) {
                        span = span.combine(self.cur_token().get_source_ref());
                        self.advance();
                    } else {
                        self.report_error(ParseError::Expected(
                            "a right bracket.".to_string(),
                            self.cur_token().get_source_ref(),
                            None,
                        ));
                    }
                    left = self.pcode.add_expr(Expr::IndexArray {
                        arr: left,
                        idx: index,
                        loc: span,
                    });
                }
                // struct field access | struct intialization
                // - struct field access: name.name
                // - struct initialization: name.(value, value, ...)
                Token::Dot(_) => {
                    let maybe_lparen = self.cur_token();
                    if matches!(self.cur_token(), Token::LParen(_)) {
                        self.advance();
                        let mut fields = vec![];
                        while !self.at_eof() {
                            let mut cur = self.cur_token();

                            if !matches!(cur, Token::RParen(_)) {
                                // field : value | field : field
                                let field = self.parse_primary();
                                let value = if matches!(self.cur_token(), Token::Colon(_)) {
                                    self.advance();
                                    self.parse_expr()
                                } else {
                                    field
                                };
                                fields.push((field, value));
                            }

                            cur = self.cur_token();

                            if matches!(cur, Token::RParen(_)) {
                                break;
                            }

                            if !matches!(cur, Token::Comma(_)) {
                                self.report_error(ParseError::Expected(
                                    "a comma to separate fields or a right parenthesis to terminate struct initialization.".to_string(),
                                    cur.get_source_ref(),
                                    None,
                                ));
                                break;
                            }

                            self.advance();
                        }

                        let left_span = self.pcode.get_source_ref_expr(left);
                        let span = left_span.combine(self.cur_token().get_source_ref());

                        if matches!(self.cur_token(), Token::RParen(_)) {
                            self.advance();
                        } else {
                            self.report_error(ParseError::Expected(
                                "a right parenthesis to terminate struct initialization."
                                    .to_string(),
                                self.cur_token().get_source_ref(),
                                None,
                            ));
                        }

                        left = self.pcode.add_expr(Expr::InitStruct {
                            struct_name: left,
                            fields,
                            loc: span,
                        });
                    } else {
                        let field = self.parse_ident();
                        let left_span = self.pcode.get_source_ref_expr(left);
                        let right_span = self.pcode.get_source_ref_expr(field);
                        let span = left_span.combine(right_span);
                        left = self.pcode.add_expr(Expr::AccessMember {
                            lhs: left,
                            rhs: field,
                            loc: span,
                        });
                    }
                }
                _ => unreachable!("Parser::parse_index_expr: unreachable op: {:?}", op),
            }
        }
        left
    }

    fn parse_ident(&mut self) -> ExprLoc {
        let cur = self.cur_token();
        match cur {
            Token::Identifier(name, loc) => {
                self.advance();
                self.pcode.add_expr(Expr::Ident { name, loc })
            }
            _ => {
                self.report_error(ParseError::Expected(
                    "an identifier.".to_string(),
                    cur.get_source_ref(),
                    None,
                ));
                self.pcode.add_expr(Expr::ErrorNode {
                    expectation: "an identifier".to_string(),
                    loc: cur.get_source_ref(),
                })
            }
        }
    }

    fn parse_directive_expr(&mut self) -> ExprLoc {
        // directive expressions take the form
        // @name(value, value, ...)
        let start = self.cur_token();
        self.advance();

        let dir_name = match self.cur_token() {
            Token::Identifier(name, loc) => {
                // skip past the directive name
                self.advance();
                name
            }
            _ => {
                self.report_error(ParseError::Expected(
                    "a directive name.".to_string(),
                    self.cur_token().get_source_ref(),
                    None,
                ));
                return self.pcode.add_expr(Expr::ErrorNode {
                    expectation: "a directive name".to_string(),
                    loc: start.get_source_ref(),
                });
            }
        };

        let mut args = vec![];

        // skip past the left parenthesis
        if matches!(self.cur_token(), Token::LParen(_)) {
            self.advance();
        } else {
            self.report_error(ParseError::Expected(
                "a left parenthesis to begin the list of directive arguments.".to_string(),
                start
                    .get_source_ref()
                    .combine(self.cur_token().get_source_ref()),
                None,
            ));
            return self.pcode.add_expr(Expr::ErrorNode {
                expectation: "a left parenthesis to begin the list of directive arguments"
                    .to_string(),
                loc: start
                    .get_source_ref()
                    .combine(self.cur_token().get_source_ref()),
            });
        }

        while !self.at_eof() {
            let mut cur = self.cur_token();
            if !matches!(cur, Token::RParen(_)) {
                let arg = self.parse_expr();
                args.push(arg);
            }
            cur = self.cur_token();

            if matches!(cur, Token::RParen(_)) {
                break;
            }

            if !matches!(cur, Token::Comma(_)) {
                self.report_error(ParseError::Expected(
                    "a comma to separate directive arguments or a right parenthesis to terminate the list of directive arguments.".to_string(),
                    cur.get_source_ref(),
                    None,
                ));
                break;
            }

            self.advance();
        }

        let span = start
            .get_source_ref()
            .combine(self.cur_token().get_source_ref());

        if matches!(self.cur_token(), Token::RParen(_)) {
            self.advance();
        } else {
            self.report_error(ParseError::Expected(
                "a right parenthesis to terminate the list of directive arguments.".to_string(),
                self.cur_token().get_source_ref(),
                None,
            ));
        }

        self.pcode.add_expr(Expr::Directive {
            name: dir_name,
            args,
            loc: span,
        })
    }

    fn parse_primary(&mut self) -> ExprLoc {
        let cur = self.cur_token();

        match cur {
            Token::CharLiteral(src, chr) => {
                self.advance();
                self.pcode.add_expr(Expr::Char { val: chr, loc: src })
            }
            Token::Identifier(_, _) => self.parse_ident(),
            Token::NumberLiteral(num, src) => {
                self.advance();
                self.pcode.add_expr(Expr::Number { val: num, loc: src })
            }
            Token::SingleLineStringLiteral(loc, str) => {
                self.advance();
                self.pcode.add_expr(Expr::Str { val: str, loc })
            }
            Token::At(_) => self.parse_directive_expr(),
            Token::MultiLineStringFragment(loc, fragment) => {
                // these are syntactic shortcuts for concatenating single line string
                // literals
                let mut str = fragment;
                let mut span = loc;
                self.advance();

                while !self.at_eof()
                    && matches!(self.cur_token(), Token::MultiLineStringFragment(_, _))
                {
                    match self.cur_token() {
                        Token::MultiLineStringFragment(_, fragment) => {
                            str.push_str(&fragment);
                            span = span.combine(self.cur_token().get_source_ref());
                            self.advance();
                        }
                        _ => break,
                    }
                }

                self.pcode.add_expr(Expr::Str {
                    val: str,
                    loc: span,
                })
            }
            Token::True(loc) => {
                self.advance();
                self.pcode.add_expr(Expr::Bool { val: true, loc })
            }
            Token::False(loc) => {
                self.advance();
                self.pcode.add_expr(Expr::Bool { val: false, loc })
            }
            Token::LParen(_) => {
                self.advance();
                let expr = self.parse_expr();
                let span = self.pcode.get_source_ref_expr(expr);
                if matches!(self.cur_token(), Token::RParen(_)) {
                    self.advance();
                } else {
                    self.report_error(ParseError::Expected(
                        "a right parenthesis.".to_string(),
                        self.cur_token().get_source_ref(),
                        None,
                    ));
                }
                expr
            }
            _ => {
                let src = cur.get_source_ref();
                self.report_error(ParseError::CannotParseAnExpression(src.clone()));
                self.pcode.add_expr(Expr::ErrorNode {
                    expectation: "an expression".to_string(),
                    loc: src,
                })
            }
        }
    }
}
