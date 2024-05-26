#![allow(unused)]

use crate::{
    lexer::{lexer::Lexer, token::Token},
    source::{
        errors::{LexError, ParseError, ParseWarning},
        source::SourceReporter,
    },
    types::signature::{Sig, Type},
};

use super::ast::{BinOpType, Expr, FileModule, Ins, UnaryOpType};
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
    lexed_token: Option<Token>,
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
            lexed_token: None,
        };

        p.advance();
        p
    }

    fn advance(&mut self) {
        loop {
            match self.lexer.next_token() {
                Ok(token) => {
                    self.lexed_token = Some(token);
                    break;
                }
                Err(lex_err) => self.lex_errs.push(lex_err),
            }
        }
    }

    fn cur_token(&self) -> Token {
        self.lexed_token.clone().unwrap()
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

    fn next_ins(&mut self, use_new_scope: bool) -> Ins {
        let cur = self.cur_token();
        match cur {
            Token::Fn(_) => self.parse_fn(),
            Token::Struct(_) => self.parse_struct(),
            Token::Mod(_) => self.parse_module(),
            Token::SingleLineComment(loc, comment) => {
                self.advance();
                Ins::SingleLineComment { comment, loc }
            }
            Token::Return(_) => self.parse_return(),
            Token::LCurly(_) => self.parse_block(use_new_scope),
            _ => self.parse_expr_ins(),
        }
    }

    fn parse_return(&mut self) -> Ins {
        if !matches!(self.scope, ParseScope::Function) {
            self.report_error(ParseError::ReturnInstructionOutsideFunction(
                self.cur_token().get_source_ref(),
            ));
            Ins::ErrorIns {
                msg: "found a return statement outside a function body".to_string(),
                loc: self.cur_token().get_source_ref(),
            }
        } else {
            let start_loc = self.cur_token().get_source_ref();
            self.advance();
            // check if there is semicolon, else we need a parse an expression
            let cur = self.cur_token();
            if matches!(cur, Token::Semicolon(_)) {
                // skip past the semicolon
                self.advance();
                let loc = start_loc.combine(cur.get_source_ref());
                Ins::Return { expr: None, loc }
            } else {
                let val = self.parse_expr();
                // TODO: determine if this triggered an error and react appropriately
                let mut return_loc = start_loc.combine(val.get_source_ref());
                let semi = self.cur_token();
                // then skip past the semicolon
                if !matches!(semi, Token::Semicolon(_)) {
                    self.report_error(ParseError::Expected(
                        "a semicolon to terminate the return statement.".to_string(),
                        return_loc.clone(),
                        None,
                    ));
                } else {
                    return_loc = return_loc.combine(semi.get_source_ref());
                }

                Ins::Return {
                    expr: Some(val),
                    loc: return_loc,
                }
            }
        }
    }

    fn parse_fn(&mut self) -> Ins {
        todo!()
    }

    fn parse_struct(&mut self) -> Ins {
        todo!()
    }

    fn parse_module(&mut self) -> Ins {
        todo!()
    }

    fn parse_expr_ins(&mut self) -> Ins {
        todo!()
    }

    fn parse_block(&mut self, use_new_scope: bool) -> Ins {
        let mut block_loc = self.cur_token().get_source_ref();
        self.advance();

        let mut code = vec![];

        let old_scope = if use_new_scope {
            let temp = self.scope;
            self.scope = ParseScope::Block;
            temp
        } else {
            self.scope
        };
        let mut saw_rcurly = false;
        while !self.is_at_eof() {
            let cur = self.cur_token();
            if matches!(cur, Token::RCurly(_)) {
                block_loc = block_loc.combine(cur.get_source_ref());
                saw_rcurly = true;
                break;
            }

            let ins = self.next_ins(true);
            code.push(ins);
        }
        let last_ins = code.last();
        if let Some(last_ins) = last_ins {
            block_loc = block_loc.combine(last_ins.get_source_ref());
        }

        if use_new_scope {
            self.scope = old_scope;
        }

        if !saw_rcurly {
            self.report_error(ParseError::UnterminatedCodeBlock(
                self.cur_token().get_source_ref(),
                None,
            ));
        } else {
            block_loc = block_loc.combine(self.cur_token().get_source_ref());
            self.advance();
        }

        Ins::Block {
            code,
            loc: block_loc,
        }
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
            Token::Bool(loc) => Type {
                tag: Sig::Bool,
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
            _ => {
                // TODO: is it wise to advance the cursor here?
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

    fn parse_expr(&mut self) -> Expr {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Expr {
        let mut lhs = self.parse_and();

        while matches!(self.cur_token(), Token::Or(_)) {
            self.advance();
            let rhs = self.parse_and();
            let or_span = lhs.get_source_ref().combine(rhs.get_source_ref());
            lhs = Expr::BinOp {
                op: BinOpType::Or,
                left: Box::new(lhs),
                right: Box::new(rhs),
                loc: or_span,
            }
        }

        lhs
    }

    fn parse_and(&mut self) -> Expr {
        let mut lhs = self.parse_equality();

        while matches!(self.cur_token(), Token::And(_)) {
            self.advance();
            let rhs = self.parse_equality();
            let and_span = lhs.get_source_ref().combine(rhs.get_source_ref());
            lhs = Expr::BinOp {
                op: BinOpType::And,
                left: Box::new(lhs),
                right: Box::new(rhs),
                loc: and_span,
            }
        }

        lhs
    }

    fn parse_equality(&mut self) -> Expr {
        let mut lhs = self.parse_comparison();

        while matches!(self.cur_token(), Token::Equal(_) | Token::NotEqual(_)) {
            let op = self.cur_token();
            self.advance();
            let rhs = self.parse_comparison();
            let eq_span = lhs.get_source_ref().combine(rhs.get_source_ref());

            lhs = Expr::BinOp {
                op: if matches!(op, Token::Equal(_)) {
                    BinOpType::Eq
                } else {
                    BinOpType::Neq
                },
                left: Box::new(lhs),
                right: Box::new(rhs),
                loc: eq_span,
            };
        }

        lhs
    }

    fn parse_comparison(&mut self) -> Expr {
        let mut lhs = self.parse_term();

        while matches!(
            self.cur_token(),
            Token::Less(_) | Token::Greater(_) | Token::LessEqual(_) | Token::GreaterEqual(_)
        ) {
            let op = self.cur_token();
            self.advance();
            let rhs = self.parse_term();
            let comp_span = lhs.get_source_ref().combine(rhs.get_source_ref());

            lhs = match op {
                Token::Less(_) => Expr::BinOp {
                    op: BinOpType::Lt,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                    loc: comp_span,
                },
                Token::Greater(_) => Expr::BinOp {
                    op: BinOpType::Gt,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                    loc: comp_span,
                },
                Token::LessEqual(_) => Expr::BinOp {
                    op: BinOpType::LtEq,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                    loc: comp_span,
                },
                Token::GreaterEqual(_) => Expr::BinOp {
                    op: BinOpType::GtEq,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                    loc: comp_span,
                },
                _ => unreachable!("Parser::parse_comparison: unexpected op: {op:?}"),
            };
        }

        lhs
    }

    fn parse_term(&mut self) -> Expr {
        let mut lhs = self.parse_factor();

        while matches!(self.cur_token(), Token::Plus(_) | Token::Minus(_)) {
            let op = self.cur_token();
            self.advance();
            let rhs = self.parse_factor();
            let term_span = lhs.get_source_ref().combine(rhs.get_source_ref());

            lhs = Expr::BinOp {
                op: if matches!(op, Token::Plus(_)) {
                    BinOpType::Add
                } else {
                    BinOpType::Sub
                },
                left: Box::new(lhs),
                right: Box::new(rhs),
                loc: term_span,
            };
        }

        lhs
    }

    fn parse_factor(&mut self) -> Expr {
        let mut lhs = self.parse_unary();

        while matches!(
            self.cur_token(),
            Token::Star(_) | Token::Slash(_) | Token::Modulo(_)
        ) {
            let op = self.cur_token();
            self.advance();
            let rhs = self.parse_unary();
            let factor_span = lhs.get_source_ref().combine(rhs.get_source_ref());

            lhs = match op {
                Token::Star(_) => Expr::BinOp {
                    op: BinOpType::Mult,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                    loc: factor_span,
                },
                Token::Slash(_) => Expr::BinOp {
                    op: BinOpType::Div,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                    loc: factor_span,
                },
                Token::Modulo(_) => Expr::BinOp {
                    op: BinOpType::Mod,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                    loc: factor_span,
                },
                _ => unreachable!("Parser::parse_factor: unexpected op: {op:?}"),
            };
        }

        lhs
    }

    fn parse_unary(&mut self) -> Expr {
        let op = self.cur_token();
        match op {
            Token::Minus(_) => {
                self.advance();
                let rhs = self.parse_unary();
                let un_span = op.get_source_ref().combine(rhs.get_source_ref());
                Expr::UnaryOp {
                    op: UnaryOpType::Negate,
                    expr: Box::new(rhs),
                    loc: un_span,
                }
            }
            Token::Not(_) => {
                self.advance();
                let rhs = self.parse_unary();
                let un_span = op.get_source_ref().combine(rhs.get_source_ref());
                Expr::UnaryOp {
                    op: UnaryOpType::Not,
                    expr: Box::new(rhs),
                    loc: un_span,
                }
            }
            _ => self.parse_index_expr(),
        }
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
