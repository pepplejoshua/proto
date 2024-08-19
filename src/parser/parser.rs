use std::{f64::consts::PI, process::exit, rc::Rc};

use crate::{
    lexer::{
        lexer::Lexer,
        token::{SrcToken, TokenType},
    },
    source::{
        errors::{LexError, ParseError},
        source::{SourceRef, SourceReporter},
    },
};

use super::ast::{Expr, Ins};

pub enum DependencyTy {
    Func,
    Type,
    FuncType,
}

pub struct Dependency {
    ty: DependencyTy,
    name_loc: Rc<SourceRef>,
}

pub struct Parser {
    pub lexer: Lexer,
    pub lex_errs: Vec<LexError>,
    pub parse_errs: Vec<ParseError>,
    pub last_token: Option<SrcToken>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut p = Parser {
            lexer,
            lex_errs: vec![],
            parse_errs: vec![],
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
                    self.lex_errs.push(err);
                }
            }
        }
    }

    fn cur_token(&self) -> SrcToken {
        self.last_token.clone().unwrap()
    }

    fn is_at_eof(&self) -> bool {
        matches!(self.cur_token().ty, TokenType::Eof)
    }

    fn report_err(&mut self, err: ParseError) {
        self.parse_errs.push(err);

        if self.parse_errs.len() >= 10 {
            let reporter = SourceReporter::new(self.lexer.src.clone());
            for err in self.lex_errs.iter() {
                reporter.report_lexer_error(err);
            }
            for err in self.parse_errs.iter() {
                reporter.report_parser_error(err.clone());
            }
            let too_many_errs = ParseError::TooManyErrors(self.cur_token().get_source_ref());
            reporter.report_parser_error(too_many_errs);
            std::process::exit(1);
        }
    }

    fn consume_err(&mut self, expected: TokenType, err_expected_msg: &str) -> bool {
        if self.cur_token().ty == expected {
            self.advance();
            true
        } else {
            self.report_err(ParseError::Expected(
                format!("{err_expected_msg}"),
                self.cur_token().get_source_ref(),
                None,
            ));
            false
        }
    }

    fn consume(&mut self, expected: TokenType) -> bool {
        if self.cur_token().ty == expected {
            self.advance();
            true
        } else {
            unreachable!(
                "Use of consume() with mismatching tokens. Expected: {expected:?}, Found: {:?}.",
                self.cur_token().ty
            )
        }
    }

    fn parse_file(&mut self) -> Vec<Ins> {
        let mut instructions = vec![];
        while !self.is_at_eof() {
            let (ins, _deps) = self.next_ins(false);
            // determine if this instruction is allowed at this scope level
            instructions.push(ins);
        }

        return instructions;
    }

    fn next_ins(&mut self, require_terminator: bool) -> (Ins, Vec<Dependency>) {
        let cur = self.cur_token();
        let mut check_terminator = false;
        let (ins, deps) = match cur.ty {
            TokenType::Identifier => {
                todo!()
            }
            TokenType::Comment => {
                self.advance();
                (
                    Ins::SingleLineComment {
                        loc: cur.loc.clone(),
                    },
                    vec![],
                )
            }
            TokenType::Print | TokenType::Println => {
                check_terminator = true;
                self.parse_print()
            }
            TokenType::Break => {
                check_terminator = true;
                self.parse_loop_control_ins(0)
            }
            TokenType::Continue => {
                check_terminator = true;
                self.parse_loop_control_ins(1)
            }
            TokenType::Return => {
                check_terminator = true;
                self.parse_return()
            }
            TokenType::If => self.parse_if_conditional(),
            TokenType::LCurly => self.parse_block(),
            _ => {
                todo!()
            }
        };

        if require_terminator && check_terminator {
            self.consume_err(
                TokenType::Semicolon,
                "a semicolon (;) to terminate instruction",
            );
        } else if check_terminator {
            if self.cur_token().ty == TokenType::Semicolon {
                self.advance();
            }
        }
        (ins, deps)
    }

    fn parse_return(&mut self) -> (Ins, Vec<Dependency>) {
        let mut loc = self.cur_token().get_source_ref();
        self.consume(TokenType::Return);
        let cur = self.cur_token();
        if matches!(cur.ty, TokenType::Semicolon) {
            loc = loc.combine(cur.get_source_ref());
            (Ins::Return { expr: None, loc }, vec![])
        } else {
            let (val, deps) = self.parse_expr(None);
            loc = loc.combine(val.get_source_ref());
            (
                Ins::Return {
                    expr: Some(val),
                    loc,
                },
                deps,
            )
        }
    }

    fn parse_block(&mut self) -> (Ins, Vec<Dependency>) {
        todo!()
    }

    fn parse_if_conditional(&mut self) -> (Ins, Vec<Dependency>) {
        todo!()
    }

    fn parse_print(&mut self) -> (Ins, Vec<Dependency>) {
        todo!()
    }

    fn parse_loop_control_ins(&mut self, id: u8) -> (Ins, Vec<Dependency>) {
        if id == 0 {
            todo!()
        } else {
            todo!()
        }
    }

    fn parse_expr(&mut self, lhs: Option<Expr>) -> (Expr, Vec<Dependency>) {
        todo!()
    }
}
