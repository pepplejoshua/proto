#![allow(unused)]

use crate::{
    lexer::{lexer::Lexer, token::Token},
    source::{
        errors::{LexError, ParseError, ParseWarning},
        source::SourceReporter,
    },
    types::signature::Type,
};

use super::ast::{Expr, FileModule, Ins};
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParseScope {
    TopLevel,
    Function,
    Struct,
    Mod,
    Block,
    Loop,
}

pub struct Parser {
    pub lexer: Lexer,
    pub lex_errs: Vec<LexError>,
    pub parse_errs: Vec<ParseError>,
    pub parse_warns: Vec<ParseWarning>,
    pub file_mod: FileModule,
    scope: ParseScope,
    last_token: Option<Token>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut p = Parser {
            lexer,
            lex_errs: Vec::new(),
            parse_errs: Vec::new(),
            parse_warns: Vec::new(),
            file_mod: FileModule::new(),
            scope: ParseScope::TopLevel,
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
                Err(lex_err) => self.lex_errs.push(lex_err),
            }
        }
    }

    fn cur_token(&self) -> Token {
        self.last_token.clone().unwrap()
    }

    fn is_at_eof(&self) -> bool {
        matches!(self.cur_token(), Token::Eof(_))
    }

    fn report_error(&mut self, err: ParseError) {
        self.parse_errs.push(err);

        if self.parse_errs.len() > 10 {
            let reporter = SourceReporter::new(self.lexer.src.clone());
            for err in self.lex_errs.iter() {
                reporter.report_lexer_error(err);
            }

            for err in self.parse_errs.iter() {
                reporter.report_parser_error(err.clone());
            }

            for warn in self.parse_warns.iter() {
                reporter.report_parser_warning(warn.clone());
            }

            let too_many_errs = ParseError::TooManyErrors(self.cur_token().get_source_ref());
            reporter.report_parser_error(too_many_errs);
            std::process::exit(1);
        }
    }

    fn report_warning(&mut self, err: ParseWarning) {
        self.parse_warns.push(err);

        if self.parse_errs.len() > 10 {
            let reporter = SourceReporter::new(self.lexer.src.clone());
            for err in self.lex_errs.iter() {
                reporter.report_lexer_error(err);
            }

            for err in self.parse_errs.iter() {
                reporter.report_parser_error(err.clone());
            }

            for warn in self.parse_warns.iter() {
                reporter.report_parser_warning(warn.clone());
            }

            let too_many_errs = ParseError::TooManyErrors(self.cur_token().get_source_ref());
            reporter.report_parser_error(too_many_errs);
            std::process::exit(1);
        }
    }

    pub fn parse_file(&mut self) {
        while !self.is_at_eof() {
            self.next_ins(true);
        }
    }

    fn next_ins(&mut self, change_scope: bool) -> Ins {
        let cur = self.cur_token();
        match cur {
            Token::Fn(_) => todo!(),
            Token::Struct(_) => todo!(),
            Token::Mod(_) => todo!(),
            Token::SingleLineComment(loc, comment) => {
                self.advance();
                Ins::SingleLineComment { comment, loc }
            }
            Token::Return(_) => todo!(),
            Token::LCurly(_) => todo!(),
            _ => self.parse_expr_ins(),
        }
    }

    fn parse_return(&mut self) -> Ins {
        todo!()
    }

    fn parse_fn(&mut self) -> Ins {
        todo!()
    }

    fn parse_struct(&mut self) -> Ins {
        todo!()
    }

    fn parse_mod(&mut self) -> Ins {
        todo!()
    }

    fn parse_expr_ins(&mut self) -> Ins {
        todo!()
    }

    fn parse_block(&mut self, use_new_scope: bool) -> Ins {
        todo!()
    }

    fn parse_type(&mut self) -> Type {
        todo!()
    }

    fn parse_expr(&mut self) -> Expr {
        todo!()
    }

    fn parse_or(&mut self) -> Expr {
        todo!()
    }

    fn parse_and(&mut self) -> Expr {
        todo!()
    }

    fn parse_equality(&mut self) -> Expr {
        todo!()
    }

    fn parse_comparison(&mut self) -> Expr {
        todo!()
    }

    fn parse_term(&mut self) -> Expr {
        todo!()
    }

    fn parse_factor(&mut self) -> Expr {
        todo!()
    }

    fn parse_unary(&mut self) -> Expr {
        todo!()
    }

    fn parse_index_expr(&mut self) -> Expr {
        todo!()
    }

    fn parse_identifier(&mut self) -> Expr {
        todo!()
    }

    fn parse_primary(&mut self) -> Expr {
        todo!()
    }
}

#[cfg(test)]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct ParserTestResult {
    code_str_repr: Vec<String>,
    lexer_errors: Vec<LexError>,
    parser_errors: Vec<ParseError>,
    parse_warning: Vec<ParseWarning>,
}

#[cfg(test)]
#[test]
fn test_parser() {
    use crate::source::source::SourceFile;

    insta::glob!("parser_inputs/*.pr", |path| {
        // build the SourceFile from the proto file path
        let path = path.to_str().unwrap().to_string();
        let src = SourceFile::new(path);

        // build the lexer
        let lexer = Lexer::new(src);
        let mut parser = Parser::new(lexer);

        parser.parse_file();
        let file_mod_str = parser.file_mod.as_str();
        let code_str_repr = file_mod_str
            .split("\n")
            .map(|s| s.to_string())
            .collect::<Vec<String>>();
        let lexer_errors = parser.lex_errs.clone();
        let parser_errors = parser.parse_errs.clone();
        let parse_warning = parser.parse_warns.clone();

        let result = ParserTestResult {
            code_str_repr,
            lexer_errors,
            parser_errors,
            parse_warning,
        };

        insta::assert_yaml_snapshot!(result);
    });
}
