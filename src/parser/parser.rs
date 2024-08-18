use crate::{
    lexer::{
        lexer::Lexer,
        token::{SrcToken, TokenType},
    },
    source::{
        errors::{LexError, ParseError},
        source::SourceReporter,
    },
};

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

    fn next_ins(&mut self, use_new_scope: bool, require_terminator: bool) {}
}
