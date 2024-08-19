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
    types::signature::Ty,
};

use super::ast::{BinOpType, Expr, FnParam, Ins};

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
            TokenType::Break | TokenType::Continue => {
                check_terminator = true;
                self.parse_loop_control_ins(cur.ty)
            }
            TokenType::Return => {
                check_terminator = true;
                self.parse_return()
            }
            TokenType::If => self.parse_if_conditional(),
            TokenType::LCurly => self.parse_ins_block(),
            _ => {
                check_terminator = true;
                self.parse_expr_ins()
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

    fn parse_ins_block(&mut self) -> (Ins, Vec<Dependency>) {
        todo!()
    }

    fn parse_if_conditional(&mut self) -> (Ins, Vec<Dependency>) {
        todo!()
    }

    fn parse_print(&mut self) -> (Ins, Vec<Dependency>) {
        todo!()
    }

    fn parse_loop_control_ins(&mut self, ty: TokenType) -> (Ins, Vec<Dependency>) {
        let loc = self.cur_token().get_source_ref();
        self.consume(ty);
        if matches!(ty, TokenType::Break) {
            (Ins::Break { loc }, vec![])
        } else {
            (Ins::Continue { loc }, vec![])
        }
    }

    fn parse_type(&mut self) -> (Rc<Ty>, Vec<Dependency>) {
        todo!()
    }

    fn parse_expr_ins(&mut self) -> (Ins, Vec<Dependency>) {
        let (expr, mut deps) = self.parse_expr(None);
        let cur = self.cur_token();
        match cur.ty {
            TokenType::Assign => {
                self.advance();
                let (val, vdeps) = self.parse_expr(None);
                let span = expr.get_source_ref().combine(val.get_source_ref());
                deps.extend(vdeps);
                (
                    Ins::AssignTo {
                        target: expr,
                        value: val,
                        loc: span,
                    },
                    deps,
                )
            }
            TokenType::PlusAssign => {
                self.advance();
                let (val, vdeps) = self.parse_expr(None);
                let span = expr.get_source_ref().combine(val.get_source_ref());
                deps.extend(vdeps);
                (
                    Ins::AssignTo {
                        target: expr.clone(),
                        value: Expr::BinOp {
                            op: BinOpType::Add,
                            left: Box::new(expr),
                            right: Box::new(val),
                            loc: span.clone(),
                        },
                        loc: span,
                    },
                    deps,
                )
            }
            TokenType::MinusAssign => {
                self.advance();
                let (val, vdeps) = self.parse_expr(None);
                let span = expr.get_source_ref().combine(val.get_source_ref());
                deps.extend(vdeps);
                (
                    Ins::AssignTo {
                        target: expr.clone(),
                        value: Expr::BinOp {
                            op: BinOpType::Sub,
                            left: Box::new(expr),
                            right: Box::new(val),
                            loc: span.clone(),
                        },
                        loc: span,
                    },
                    deps,
                )
            }
            TokenType::StarAssign => {
                self.advance();
                let (val, vdeps) = self.parse_expr(None);
                let span = expr.get_source_ref().combine(val.get_source_ref());
                deps.extend(vdeps);
                (
                    Ins::AssignTo {
                        target: expr.clone(),
                        value: Expr::BinOp {
                            op: BinOpType::Mult,
                            left: Box::new(expr),
                            right: Box::new(val),
                            loc: span.clone(),
                        },
                        loc: span,
                    },
                    deps,
                )
            }
            TokenType::SlashAssign => {
                self.advance();
                let (val, vdeps) = self.parse_expr(None);
                let span = expr.get_source_ref().combine(val.get_source_ref());
                deps.extend(vdeps);
                (
                    Ins::AssignTo {
                        target: expr.clone(),
                        value: Expr::BinOp {
                            op: BinOpType::Div,
                            left: Box::new(expr),
                            right: Box::new(val),
                            loc: span.clone(),
                        },
                        loc: span,
                    },
                    deps,
                )
            }
            TokenType::ModuloAssign => {
                self.advance();
                let (val, vdeps) = self.parse_expr(None);
                let span = expr.get_source_ref().combine(val.get_source_ref());
                deps.extend(vdeps);
                (
                    Ins::AssignTo {
                        target: expr.clone(),
                        value: Expr::BinOp {
                            op: BinOpType::Mod,
                            left: Box::new(expr),
                            right: Box::new(val),
                            loc: span.clone(),
                        },
                        loc: span,
                    },
                    deps,
                )
            }
            _ => (
                Ins::ExprIns {
                    loc: expr.get_source_ref(),
                    expr,
                },
                deps,
            ),
        }
    }

    fn parse_fn_params(&mut self) -> (Vec<FnParam>, Vec<Dependency>) {
        todo!()
    }

    fn parse_expr(&mut self, lhs: Option<Expr>) -> (Expr, Vec<Dependency>) {
        if let Some(lhs) = lhs {
            self.parse_ternary(lhs)
        } else {
            self.parse_lambda_expr()
        }
    }

    fn parse_lambda_expr(&mut self) -> (Expr, Vec<Dependency>) {
        let cur = self.cur_token();
        match cur.ty {
            TokenType::BackSlash => {
                self.advance();
                let mut deps = vec![];
                let (params, pdeps) = self.parse_fn_params();
                deps.extend(pdeps);
                let (ret_type, rtdeps) = self.parse_type();
                deps.extend(rtdeps);
                let (body, bdeps) = self.next_ins(false);
                deps.extend(bdeps);
                let loc = cur.get_source_ref().combine(body.get_source_ref());

                (
                    Expr::Lambda {
                        params,
                        ret_type,
                        body: Box::new(body),
                        loc,
                    },
                    deps,
                )
            }
            _ => self.parse_optional_expr(),
        }
    }

    fn parse_optional_expr(&mut self) -> (Expr, Vec<Dependency>) {
        let cur = self.cur_token();
        match cur.ty {
            TokenType::Some => {
                todo!()
            }
            TokenType::None => {
                todo!()
            }
            _ => self.parse_ternary(None),
        }
    }

    fn parse_ternary(&mut self, lhs: Option<Expr>) -> (Expr, Vec<Dependency>) {
        todo!()
    }

    fn parse_or(&mut self, lhs: Option<Expr>) -> (Expr, Vec<Dependency>) {
        todo!()
    }

    fn parse_and(&mut self, lhs: Option<Expr>) -> (Expr, Vec<Dependency>) {
        todo!()
    }

    fn parse_equality(&mut self, lhs: Option<Expr>) -> (Expr, Vec<Dependency>) {
        todo!()
    }

    fn parse_comparison(&mut self, lhs: Option<Expr>) -> (Expr, Vec<Dependency>) {
        todo!()
    }

    fn parse_term(&mut self, lhs: Option<Expr>) -> (Expr, Vec<Dependency>) {
        todo!()
    }

    fn parse_factor(&mut self, lhs: Option<Expr>) -> (Expr, Vec<Dependency>) {
        todo!()
    }

    fn parse_unary(&mut self) -> (Expr, Vec<Dependency>) {
        todo!()
    }

    fn parse_ptr_deref_or_addr_of(&mut self) -> (Expr, Vec<Dependency>) {
        todo!()
    }

    fn parse_index_expr(&mut self, lhs: Option<Expr>) -> (Expr, Vec<Dependency>) {
        todo!()
    }

    fn parse_hashmap_pair(&mut self) -> (Expr, Vec<Dependency>) {
        todo!()
    }

    fn parse_identifier(&mut self) -> (Expr, Vec<Dependency>) {
        todo!()
    }

    fn parse_primary(&mut self) -> (Expr, Vec<Dependency>) {
        todo!()
    }
}
