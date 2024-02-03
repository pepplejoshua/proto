#![allow(dead_code)]
#![allow(unused_variables)]

use crate::{
    lexer::{lexer::Lexer, token::Token},
    source::{
        errors::{LexError, ParseError, ParseWarning},
        source::SourceReporter,
    },
};

use super::pcode::{Expr, ExprLoc, InsLoc, PCode};

enum ParseScope {
    Global,
    Function,
    Struct,
    Mod,
    Block,
    Loop,
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

    fn parse_file(&mut self) {
        while !self.at_eof() {
            self.next_instruction();
        }
    }

    fn next_instruction(&mut self) -> InsLoc {
        let cur = self.cur_token();
        match cur {
            Token::SingleLineComment(src, comment) => {
                self.advance();
                self.next_instruction()
            }
            Token::At(_) => self.parse_directive_ins(),
            _ => {
                self.report_error(ParseError::Expected(
                    "an instruction. They usually begin with an identifier.".to_string(),
                    self.cur_token().get_source_ref(),
                    None,
                ));
                todo!()
            }
        }
    }

    fn parse_identifier_structure(&mut self) -> InsLoc {
        // identifier structures include:
        // - constant declarations:
        // IDENT : type? : value;
        // - variable declarations:
        // IDENT : type? = value;
        // assignments:
        // VALID_LHS = value;
        // where VALID_LHS is:
        // name[index] (array index)
        // name.name (struct field access)
        // - function calls:
        // VALID_LHS(args);

        // values can be:
        // - expressions
        // - functions:
        // name :: (args) -> ret_type { code }
        // - blocks:
        // - structs:
        // name :: { fields }
        // - modules:
        // name :: { code }
        todo!()
    }

    fn parse_directive_ins(&mut self) -> InsLoc {
        // directive instructions
        todo!()
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

                    left = self.pcode.add_expr(Expr::CallFn {
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
                            let cur = self.cur_token();

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
