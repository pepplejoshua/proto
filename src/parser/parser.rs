#![allow(unused)]

use std::{io::SeekFrom, rc::Rc};

use crate::{
    lexer::{
        lexer::Lexer,
        token::{SrcToken, TokenType},
    },
    parser::ast::UnaryOpType,
    source::{
        errors::{LexError, ParseError},
        source::{SourceRef, SourceReporter},
    },
    types::signature::Ty,
};

use super::ast::{BinOpType, Expr, FnParam, Ins};

pub enum DependencyTy {
    Name,
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

    fn expected_err_token(&mut self, err_expected_msg: &str) {
        self.report_err(ParseError::Expected(
            format!("{err_expected_msg}"),
            self.cur_token().get_source_ref(),
            None,
        ));
    }

    fn expected_err_parse(&mut self, err_expected_msg: &str, loc: Rc<SourceRef>) {
        self.report_err(ParseError::Expected(
            format!("{err_expected_msg}"),
            loc,
            None,
        ));
    }

    fn consume(&mut self, expected: TokenType, err_expected_msg: &str) -> bool {
        if self.cur_token().ty == expected {
            self.advance();
            true
        } else {
            self.expected_err_token(err_expected_msg);
            false
        }
    }

    pub fn parse_file(&mut self) -> Vec<Ins> {
        let mut instructions = vec![];
        while !self.is_at_eof() {
            let (ins, _deps) = self.next_ins(false);
            // determine if this instruction is allowed at this scope level
            instructions.push(ins);
        }

        return instructions;
    }

    fn next_ins(&mut self, require_terminator: bool) -> (Ins, Vec<Dependency>) {
        let token = self.cur_token();
        let mut check_terminator = false;
        let (ins, deps) = match token.ty {
            TokenType::Identifier => {
                self.advance();
                let cur = self.cur_token();
                match cur.ty {
                    TokenType::DoubleColon => {
                        // inferred const
                        self.advance();
                        // get init value
                        let (val, vdeps) = self.parse_expr(None);
                        check_terminator = true;
                        (
                            Ins::DeclConst {
                                loc: token.loc.combine(val.get_source_ref()),
                                name: Expr::Identifier { loc: token.loc },
                                ty: None,
                                init_val: val,
                            },
                            vdeps,
                        )
                    }
                    TokenType::ColonAssign => {
                        // inferred var
                        self.advance();
                        // get init value
                        let (val, vdeps) = self.parse_expr(None);
                        check_terminator = true;
                        (
                            Ins::DeclVar {
                                loc: token.loc.combine(val.get_source_ref()),
                                name: Expr::Identifier { loc: token.loc },
                                ty: None,
                                init_val: Some(val),
                            },
                            vdeps,
                        )
                    }
                    TokenType::Colon => {
                        // typed definition (const or var)
                        self.advance();
                        // parse a type
                        let (ty, tdeps) = self.parse_type();
                        match self.cur_token().ty {
                            TokenType::Colon => {
                                // a typed constant with init value
                                self.advance();
                                let (val, vdeps) = self.parse_expr(None);
                                check_terminator = true;
                                (
                                    Ins::DeclConst {
                                        loc: token.loc.combine(val.get_source_ref()),
                                        name: Expr::Identifier { loc: token.loc },
                                        ty: Some(ty),
                                        init_val: val,
                                    },
                                    vdeps,
                                )
                            }
                            TokenType::Assign => {
                                // a typed var with init value
                                self.advance();
                                let (val, vdeps) = self.parse_expr(None);
                                check_terminator = true;
                                (
                                    Ins::DeclVar {
                                        loc: token.loc.combine(val.get_source_ref()),
                                        name: Expr::Identifier { loc: token.loc },
                                        ty: Some(ty),
                                        init_val: Some(val),
                                    },
                                    vdeps,
                                )
                            }
                            _ => {
                                // an type var decl without init value
                                check_terminator = true;
                                (
                                    Ins::DeclVar {
                                        loc: token.loc.combine(ty.get_loc()),
                                        name: Expr::Identifier { loc: token.loc },
                                        ty: Some(ty),
                                        init_val: None,
                                    },
                                    vec![],
                                )
                            }
                        }
                    }
                    _ => {
                        // we will pass the identifier we have seen already
                        // to parse_expr_ins
                        self.parse_expr_ins(Some(Expr::Identifier {
                            loc: token.get_source_ref(),
                        }))
                    }
                }
            }
            TokenType::Comment => {
                self.advance();
                (
                    Ins::SingleLineComment {
                        loc: token.loc.clone(),
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
                self.parse_loop_control_ins(token.ty)
            }
            TokenType::Return => {
                check_terminator = true;
                self.parse_return()
            }
            TokenType::If => self.parse_if_conditional(),
            TokenType::For => self.parse_loop(),
            TokenType::LCurly => self.parse_ins_block(),
            _ => {
                check_terminator = true;
                self.parse_expr_ins(None)
            }
        };

        if require_terminator && check_terminator {
            self.consume(
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
        self.advance();
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
        let mut loc = self.cur_token().loc;
        self.consume(
            TokenType::LCurly,
            "a left curly brace ({) to begin the body of the block instruction.",
        );

        let mut code = vec![];
        let mut deps = vec![];
        while !self.is_at_eof() {
            if self.cur_token().ty == TokenType::RCurly {
                break;
            }

            let (ins, ideps) = self.next_ins(false);
            code.push(ins);
            deps.extend(ideps);
        }

        loc = loc.combine(self.cur_token().loc);
        self.consume(
            TokenType::RCurly,
            "a right curly brace (}) to terminate the body of the block instruction.",
        );
        (Ins::Block { code, loc }, deps)
    }

    fn parse_loop(&mut self) -> (Ins, Vec<Dependency>) {
        let mut loc = self.cur_token().loc;
        self.advance();

        match self.cur_token().ty {
            // infinite loop
            TokenType::LCurly => {
                let (block, bdeps) = self.parse_ins_block();
                loc = loc.combine(block.get_source_ref());
                (
                    Ins::InfiniteLoop {
                        block: Box::new(block),
                        loc,
                    },
                    bdeps,
                )
            }
            // conventional loop
            TokenType::LParen => {
                self.advance();
                let (loop_var, mut ldeps) = self.next_ins(false);
                if !matches!(loop_var, Ins::DeclVar { .. }) {
                    self.expected_err_parse(
                        "a variable declaration instruction for the loop variable.",
                        loop_var.get_source_ref(),
                    );
                }
                let (loop_cond, lcdeps) = self.parse_expr(None);
                ldeps.extend(lcdeps);
                self.consume(
                    TokenType::Semicolon,
                    "a semicolon to terminate the condition expression of the loop.",
                );
                let (loop_update, ludeps) = self.next_ins(false);
                ldeps.extend(ludeps);
                self.consume(
                    TokenType::RParen,
                    "a right parenthesis [)] to terminate the conventional loop header.",
                );
                let (body, bdeps) = self.parse_ins_block();
                ldeps.extend(bdeps);

                loc = loc.combine(body.get_source_ref());
                (
                    Ins::RegLoop {
                        init: Box::new(loop_var),
                        loop_cond,
                        update: Box::new(loop_update),
                        block: Box::new(body),
                        loc,
                    },
                    ldeps,
                )
            }
            _ => {
                let (target_expr, mut tdeps) = self.parse_expr(None);
                match self.cur_token().ty {
                    //iterator loop
                    TokenType::In => {
                        self.advance();
                        let (loop_target, ltdeps) = self.parse_expr(None);
                        tdeps.extend(ltdeps);
                        let (body, bdeps) = self.parse_ins_block();
                        tdeps.extend(bdeps);
                        loc = loc.combine(body.get_source_ref());
                        (
                            Ins::ForInLoop {
                                loop_var: target_expr,
                                loop_target,
                                block: Box::new(body),
                                loc,
                            },
                            tdeps,
                        )
                    }
                    // while with a post code
                    TokenType::Colon => {
                        self.advance();
                        self.consume(
                            TokenType::LParen,
                            "a left parenthesis [(] to start the loop post-instruction.",
                        );
                        let (post_code, pdeps) = self.next_ins(false);
                        self.consume(
                            TokenType::RParen,
                            "a right parenthesis [)] to terminate the loop post-instruction.",
                        );
                        tdeps.extend(pdeps);
                        let (body, bdeps) = self.parse_ins_block();
                        tdeps.extend(bdeps);
                        loc = loc.combine(body.get_source_ref());
                        (
                            Ins::WhileLoop {
                                cond: target_expr,
                                post_code: Some(Box::new(post_code)),
                                block: Box::new(body),
                                loc,
                            },
                            tdeps,
                        )
                    }
                    _ => {
                        let (body, bdeps) = self.parse_ins_block();
                        loc = loc.combine(body.get_source_ref());
                        tdeps.extend(bdeps);
                        (
                            Ins::WhileLoop {
                                cond: target_expr,
                                post_code: None,
                                block: Box::new(body),
                                loc,
                            },
                            tdeps,
                        )
                    }
                }
            }
        }
    }

    fn parse_if_conditional(&mut self) -> (Ins, Vec<Dependency>) {
        let mut loc = self.cur_token().loc;
        self.advance();

        let mut conds_and_code = vec![];
        let mut deps = vec![];
        // get the first condition and body
        let (first_cond, edeps) = self.parse_expr(None);
        deps.extend(edeps);
        let (first_body, bdeps) = self.parse_ins_block();
        deps.extend(bdeps);
        conds_and_code.push((Some(first_cond), first_body));

        while !self.is_at_eof() {
            if self.cur_token().ty != TokenType::Else {
                break;
            }
            self.advance();
            if self.cur_token().ty == TokenType::If {
                self.advance();
                let (else_if_cond, eideps) = self.parse_expr(None);
                deps.extend(eideps);
                let (else_if_code, eicdeps) = self.parse_ins_block();
                deps.extend(eicdeps);
                conds_and_code.push((Some(else_if_cond), else_if_code));
            } else {
                let (else_body, edeps) = self.parse_ins_block();
                deps.extend(edeps);
                conds_and_code.push((None, else_body));
                break;
            }
        }

        loc = loc.combine(conds_and_code.last().unwrap().1.get_source_ref());

        (
            Ins::IfConditional {
                conds_and_code,
                loc,
            },
            deps,
        )
    }

    fn parse_print(&mut self) -> (Ins, Vec<Dependency>) {
        let is_println = self.cur_token().ty == TokenType::Print;
        let mut loc = self.cur_token().loc;
        self.advance();

        self.consume(
            TokenType::LParen,
            "a left parenthesis [(] to start argument list.",
        );

        let (output, odeps) = self.parse_expr(None);
        loc = loc.combine(self.cur_token().loc);
        self.consume(
            TokenType::RParen,
            "a right parenthesis [)] to terminate argument list.",
        );

        (
            Ins::PrintIns {
                is_println,
                output,
                loc,
            },
            odeps,
        )
    }

    fn parse_loop_control_ins(&mut self, ty: TokenType) -> (Ins, Vec<Dependency>) {
        let loc = self.cur_token().get_source_ref();
        self.advance();
        if ty == TokenType::Break {
            (Ins::Break { loc }, vec![])
        } else {
            (Ins::Continue { loc }, vec![])
        }
    }

    fn parse_type(&mut self) -> (Rc<Ty>, Vec<Dependency>) {
        let start_loc = self.cur_token().get_source_ref();
        let mut deps = Vec::new();
        let cur = self.cur_token();
        match cur.ty {
            TokenType::LBracket => {
                self.advance();

                if self.cur_token().ty == TokenType::RBracket {
                    // Slice type
                    self.advance();
                    let (sub_ty, sub_deps) = self.parse_type();
                    deps.extend(sub_deps);
                    let end_loc = sub_ty.get_loc();
                    let loc = start_loc.combine(end_loc);

                    return (Rc::new(Ty::Slice { sub_ty, loc }), deps);
                } else {
                    // Static array type
                    let (size, sdeps) = self.parse_primary();
                    deps.extend(sdeps);
                    self.consume(TokenType::RBracket, "a right bracket (]) after array size");

                    let (sub_ty, sub_deps) = self.parse_type();
                    deps.extend(sub_deps);
                    let end_loc = sub_ty.get_loc();
                    let loc = start_loc.combine(end_loc);

                    return (Rc::new(Ty::StaticArray { sub_ty, size, loc }), deps);
                }
            }
            TokenType::QuestionMark => {
                self.advance();
                let (ty, tdeps) = self.parse_type();
                (
                    Rc::new(Ty::Optional {
                        loc: cur.get_source_ref().combine(ty.get_loc()),
                        sub_ty: ty,
                    }),
                    tdeps,
                )
            }
            _ => self.parse_base_type(),
        }
    }

    fn parse_base_type(&mut self) -> (Rc<Ty>, Vec<Dependency>) {
        let token = self.cur_token();
        let loc = token.get_source_ref();

        let ty = match token.ty {
            TokenType::I8 => Ty::Signed {
                size: 8,
                is_int: false,
                loc: loc.clone(),
            },
            TokenType::I16 => Ty::Signed {
                size: 16,
                is_int: false,
                loc: loc.clone(),
            },
            TokenType::I32 => Ty::Signed {
                size: 32,
                is_int: false,
                loc: loc.clone(),
            },
            TokenType::I64 => Ty::Signed {
                size: 64,
                is_int: false,
                loc: loc.clone(),
            },
            TokenType::Int => Ty::Signed {
                size: Ty::get_platform_size(),
                is_int: true,
                loc: loc.clone(),
            },
            TokenType::U8 => Ty::Unsigned {
                size: 8,
                is_uint: false,
                loc: loc.clone(),
            },
            TokenType::U16 => Ty::Unsigned {
                size: 16,
                is_uint: false,
                loc: loc.clone(),
            },
            TokenType::U32 => Ty::Unsigned {
                size: 32,
                is_uint: false,
                loc: loc.clone(),
            },
            TokenType::U64 => Ty::Unsigned {
                size: 64,
                is_uint: false,
                loc: loc.clone(),
            },
            TokenType::UInt => Ty::Unsigned {
                size: Ty::get_platform_size(),
                is_uint: true,
                loc: loc.clone(),
            },
            TokenType::Char => Ty::Char { loc: loc.clone() },
            TokenType::Str => Ty::Str { loc: loc.clone() },
            TokenType::Void => Ty::Void { loc: loc.clone() },
            TokenType::Bool => Ty::Bool { loc: loc.clone() },
            TokenType::Type => Ty::Type { loc: loc.clone() },
            TokenType::Identifier => {
                self.advance();
                return (
                    Rc::new(Ty::NamedType {
                        name: self.lexer.src.text[token.loc.flat_start..token.loc.flat_end]
                            .to_string(),
                        loc: loc.clone(),
                    }),
                    vec![Dependency {
                        ty: DependencyTy::Type,
                        name_loc: loc,
                    }],
                );
            }
            _ => {
                self.expected_err_token("a type.");
                Ty::ErrorType { loc: loc.clone() }
            }
        };

        self.advance();
        (Rc::new(ty), vec![])
    }

    fn parse_expr_ins(&mut self, expr: Option<Expr>) -> (Ins, Vec<Dependency>) {
        let (expr, mut deps) = self.parse_expr(expr);
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
        self.consume(
            TokenType::LParen,
            "a left parenthesis [(] to begin the list of parameters.",
        );

        let mut params = vec![];
        let mut deps = vec![];
        while !self.is_at_eof() {
            if self.cur_token().ty == TokenType::RParen {
                break;
            }

            let (param_name, _) = self.parse_identifier();
            let (param_ty, pdeps) = self.parse_type();
            deps.extend(pdeps);

            let loc = param_name.get_source_ref().combine(param_ty.get_loc());
            params.push(FnParam {
                name: param_name,
                given_ty: param_ty,
                loc,
            })
        }

        self.consume(
            TokenType::RParen,
            "a right parenthesis [)] to terminate the list of parameters.",
        );

        (params, deps)
    }

    fn parse_expr(&mut self, lhs: Option<Expr>) -> (Expr, Vec<Dependency>) {
        if lhs.is_some() {
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
                let (body, bdeps) = self.parse_ins_block();
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
                self.advance();
                let (val, vdeps) = self.parse_expr(None);
                (
                    Expr::OptionalExpr {
                        loc: cur.get_source_ref().combine(val.get_source_ref()),
                        val: Some(Box::new(val)),
                    },
                    vdeps,
                )
            }
            TokenType::None => {
                self.advance();
                (
                    Expr::OptionalExpr {
                        loc: cur.get_source_ref(),
                        val: None,
                    },
                    vec![],
                )
            }
            _ => self.parse_ternary(None),
        }
    }

    fn parse_ternary(&mut self, lhs: Option<Expr>) -> (Expr, Vec<Dependency>) {
        let (lhs, mut deps) = self.parse_or(lhs);

        if matches!(self.cur_token().ty, TokenType::QuestionMark) {
            self.advance();
            let (then, tdeps) = self.parse_expr(None);
            deps.extend(tdeps);
            self.consume(
                TokenType::Colon,
                "a colon (:) to separate both branches of the ternary condition expression.",
            );
            let (otherwise, odeps) = self.parse_expr(None);
            deps.extend(odeps);
            return (
                Expr::ConditionalExpr {
                    loc: lhs.get_source_ref().combine(otherwise.get_source_ref()),
                    cond: Box::new(lhs),
                    then: Box::new(then),
                    otherwise: Box::new(otherwise),
                },
                deps,
            );
        }
        (lhs, deps)
    }

    fn parse_or(&mut self, lhs: Option<Expr>) -> (Expr, Vec<Dependency>) {
        let (mut lhs, mut deps) = self.parse_and(lhs);

        while matches!(self.cur_token().ty, TokenType::Or) {
            self.advance();
            let (rhs, rdeps) = self.parse_and(None);
            let loc = lhs.get_source_ref().combine(rhs.get_source_ref());
            lhs = Expr::BinOp {
                op: BinOpType::Or,
                left: Box::new(lhs),
                right: Box::new(rhs),
                loc,
            };
            deps.extend(rdeps);
        }
        (lhs, deps)
    }

    fn parse_and(&mut self, lhs: Option<Expr>) -> (Expr, Vec<Dependency>) {
        let (mut lhs, mut deps) = self.parse_equality(lhs);

        while matches!(self.cur_token().ty, TokenType::And) {
            self.advance();
            let (rhs, rdeps) = self.parse_equality(None);
            let loc = lhs.get_source_ref().combine(rhs.get_source_ref());
            lhs = Expr::BinOp {
                op: BinOpType::And,
                left: Box::new(lhs),
                right: Box::new(rhs),
                loc,
            };
            deps.extend(rdeps);
        }
        (lhs, deps)
    }

    fn parse_equality(&mut self, lhs: Option<Expr>) -> (Expr, Vec<Dependency>) {
        let (mut lhs, mut deps) = self.parse_comparison(lhs);

        while matches!(self.cur_token().ty, TokenType::Equal | TokenType::NotEqual) {
            let op = if self.cur_token().ty == TokenType::Equal {
                BinOpType::Eq
            } else {
                BinOpType::Neq
            };
            self.advance();
            let (rhs, rdeps) = self.parse_comparison(None);
            let loc = lhs.get_source_ref().combine(rhs.get_source_ref());
            lhs = Expr::BinOp {
                op,
                left: Box::new(lhs),
                right: Box::new(rhs),
                loc,
            };
            deps.extend(rdeps);
        }
        (lhs, deps)
    }

    fn parse_comparison(&mut self, lhs: Option<Expr>) -> (Expr, Vec<Dependency>) {
        let (mut lhs, mut deps) = self.parse_term(lhs);

        while matches!(
            self.cur_token().ty,
            TokenType::Less | TokenType::LessEqual | TokenType::Greater | TokenType::GreaterEqual
        ) {
            let op = match self.cur_token().ty {
                TokenType::Less => BinOpType::Lt,
                TokenType::LessEqual => BinOpType::LtEq,
                TokenType::Greater => BinOpType::Gt,
                TokenType::GreaterEqual => BinOpType::GtEq,
                _ => unreachable!("Non-comparison operator entered loop."),
            };
            self.advance();
            let (rhs, rdeps) = self.parse_term(None);
            let loc = lhs.get_source_ref().combine(rhs.get_source_ref());
            lhs = Expr::BinOp {
                op,
                left: Box::new(lhs),
                right: Box::new(rhs),
                loc,
            };
            deps.extend(rdeps);
        }
        (lhs, deps)
    }

    fn parse_term(&mut self, lhs: Option<Expr>) -> (Expr, Vec<Dependency>) {
        let (mut lhs, mut deps) = self.parse_factor(lhs);

        while matches!(self.cur_token().ty, TokenType::Plus | TokenType::Minus) {
            let op = if self.cur_token().ty == TokenType::Plus {
                BinOpType::Add
            } else {
                BinOpType::Sub
            };
            self.advance();
            let (rhs, rdeps) = self.parse_factor(None);
            let loc = lhs.get_source_ref().combine(rhs.get_source_ref());
            lhs = Expr::BinOp {
                op,
                left: Box::new(lhs),
                right: Box::new(rhs),
                loc,
            };
            deps.extend(rdeps);
        }
        (lhs, deps)
    }

    fn parse_factor(&mut self, lhs: Option<Expr>) -> (Expr, Vec<Dependency>) {
        let (mut lhs, mut deps) = if lhs.is_some() {
            self.parse_index_expr(lhs)
        } else {
            self.parse_unary()
        };

        while matches!(
            self.cur_token().ty,
            TokenType::Star | TokenType::Slash | TokenType::Modulo
        ) {
            let op = match self.cur_token().ty {
                TokenType::Star => BinOpType::Mult,
                TokenType::Slash => BinOpType::Div,
                TokenType::Modulo => BinOpType::Mod,
                _ => unreachable!("Non-factor operator entered loop."),
            };
            self.advance();
            let (rhs, rdeps) = self.parse_unary();
            let loc = lhs.get_source_ref().combine(rhs.get_source_ref());
            lhs = Expr::BinOp {
                op,
                left: Box::new(lhs),
                right: Box::new(rhs),
                loc,
            };
            deps.extend(rdeps);
        }
        (lhs, deps)
    }

    fn parse_unary(&mut self) -> (Expr, Vec<Dependency>) {
        let op = self.cur_token();
        match op.ty {
            TokenType::Minus | TokenType::Not => {
                self.advance();
                let (rhs, rdeps) = self.parse_unary();
                let loc = op.get_source_ref().combine(rhs.get_source_ref());
                let op = if op.ty == TokenType::Minus {
                    UnaryOpType::Negate
                } else {
                    UnaryOpType::Not
                };
                (
                    Expr::UnaryOp {
                        op,
                        expr: Box::new(rhs),
                        loc,
                    },
                    rdeps,
                )
            }
            _ => self.parse_ptr_deref_or_addr_of(),
        }
    }

    fn parse_ptr_deref_or_addr_of(&mut self) -> (Expr, Vec<Dependency>) {
        let op = self.cur_token();
        match op.ty {
            TokenType::Star => {
                self.advance();
                let (rhs, rdeps) = self.parse_ptr_deref_or_addr_of();
                let loc = op.get_source_ref().combine(rhs.get_source_ref());
                (
                    Expr::DerefPtr {
                        loc,
                        target: Box::new(rhs),
                    },
                    rdeps,
                )
            }
            TokenType::Ampersand => {
                self.advance();
                let (rhs, rdeps) = self.parse_index_expr(None);
                let loc = op.get_source_ref().combine(rhs.get_source_ref());
                (
                    Expr::DerefPtr {
                        loc,
                        target: Box::new(rhs),
                    },
                    rdeps,
                )
            }
            _ => self.parse_index_expr(None),
        }
    }

    fn parse_comma_sep_exprs(&mut self, terminator_ty: TokenType) -> (Vec<Expr>, Vec<Dependency>) {
        let mut args = vec![];
        let mut deps = vec![];

        while !self.is_at_eof() {
            if self.cur_token().ty == terminator_ty {
                break;
            }

            let (arg, adeps) = self.parse_expr(None);
            deps.extend(adeps);
            args.push(arg);

            if matches!(self.cur_token().ty, TokenType::Comma) {
                self.advance();
            }
        }
        (args, deps)
    }

    fn parse_index_expr(&mut self, lhs: Option<Expr>) -> (Expr, Vec<Dependency>) {
        let (mut lhs, mut deps) = if let Some(lhs) = lhs {
            // we have no other sub trees that will start with an expression
            (lhs, vec![])
        } else {
            self.parse_primary()
        };

        while matches!(
            self.cur_token().ty,
            TokenType::LParen | TokenType::LBracket | TokenType::Dot
        ) {
            let op = self.cur_token();
            self.advance();
            match op.ty {
                TokenType::LParen => {
                    let (args, adeps) = self.parse_comma_sep_exprs(TokenType::RParen);
                    deps.extend(adeps);
                    let loc = if args.is_empty() {
                        op.get_source_ref()
                            .combine(self.cur_token().get_source_ref())
                    } else {
                        op.get_source_ref()
                            .combine(args.last().unwrap().get_source_ref())
                    };
                    self.consume(
                        TokenType::RParen,
                        "a right parenthesis [)] to terminate the function call.",
                    );

                    if let Expr::Identifier { loc } = &lhs {
                        deps.push(Dependency {
                            ty: DependencyTy::Func,
                            name_loc: loc.clone(),
                        });
                    }

                    if let Expr::AccessMember { mem, .. } = &lhs {
                        deps.push(Dependency {
                            ty: DependencyTy::Func,
                            name_loc: mem.get_source_ref(),
                        });
                    }

                    lhs = Expr::CallFn {
                        func: Box::new(lhs),
                        args,
                        loc,
                    };
                }
                TokenType::LBracket => {
                    if self.cur_token().ty == TokenType::Colon {
                        self.advance();
                        let (end_excl, edeps) = match self.cur_token().ty {
                            TokenType::RBracket => (None, vec![]),
                            _ => {
                                let (end, edeps) = self.parse_expr(None);
                                (Some(Box::new(end)), edeps)
                            }
                        };
                        println!("{:?}", end_excl);
                        deps.extend(edeps);
                        let loc = op.loc.combine(self.cur_token().loc);
                        self.consume(
                            TokenType::RBracket,
                            "a right bracket (]) to terminate the slice expression.",
                        );
                        lhs = Expr::MakeSlice {
                            target: Box::new(lhs),
                            start: None,
                            end: end_excl,
                            loc,
                        };
                        continue;
                    }

                    let (index_expr, ideps) = self.parse_expr(None);
                    deps.extend(ideps);
                    let mut loc = op.loc;

                    match self.cur_token().ty {
                        TokenType::RBracket => {
                            loc = loc.combine(self.cur_token().loc);
                            self.advance();
                            lhs = Expr::IndexInto {
                                target: Box::new(lhs),
                                index: Box::new(index_expr),
                                loc,
                            };
                        }
                        TokenType::Colon => {
                            self.advance();
                            let (end_excl, edeps) = match self.cur_token().ty {
                                TokenType::RBracket => (None, vec![]),
                                _ => {
                                    let (end, edeps) = self.parse_expr(None);
                                    (Some(Box::new(end)), edeps)
                                }
                            };
                            deps.extend(edeps);
                            loc = loc.combine(self.cur_token().loc);
                            self.consume(
                                TokenType::RBracket,
                                "a right parenthesis (]) to terminate the slice expression.",
                            );
                            lhs = Expr::MakeSlice {
                                target: Box::new(lhs),
                                start: Some(Box::new(index_expr)),
                                end: end_excl,
                                loc,
                            };
                        }
                        _ => {
                            self.consume(TokenType::RBracket, concat!("a right bracket (]) to terminate index expression or a colon to ", "separate start and exclusive end of the slice expression."));
                            loc = loc.combine(index_expr.get_source_ref());
                            lhs = Expr::IndexInto {
                                target: Box::new(lhs),
                                index: Box::new(index_expr),
                                loc,
                            }
                        }
                    }
                }
                TokenType::Dot => {
                    let (member, mdeps) = self.parse_primary();
                    deps.extend(mdeps);
                    let loc = op.loc.combine(member.get_source_ref());
                    lhs = Expr::AccessMember {
                        target: Box::new(lhs),
                        mem: Box::new(member),
                        loc,
                    };
                }
                _ => unreachable!("unexpected operator in parse_index_expr loop: {:#?}", op),
            }
        }
        (lhs, deps)
    }

    fn parse_primary(&mut self) -> (Expr, Vec<Dependency>) {
        let cur = self.cur_token();
        match cur.ty {
            TokenType::Identifier => self.parse_identifier(),
            TokenType::Character => {
                self.advance();
                (Expr::Char { loc: cur.loc }, vec![])
            }
            TokenType::String => {
                self.advance();
                (Expr::Str { loc: cur.loc }, vec![])
            }
            TokenType::Integer => {
                self.advance();
                (Expr::Integer { loc: cur.loc }, vec![])
            }
            TokenType::Decimal => {
                self.advance();
                (Expr::Decimal { loc: cur.loc }, vec![])
            }
            TokenType::True => {
                self.advance();
                (Expr::Bool { loc: cur.loc }, vec![])
            }
            TokenType::False => {
                self.advance();
                (Expr::Bool { loc: cur.loc }, vec![])
            }
            TokenType::LBracket => {
                // we are parsing either an array type, or an array literal,
                // with a preceding array type
                let (arr_ty, mut adeps) = self.parse_type();
                if self.cur_token().ty == TokenType::LCurly {
                    self.advance();
                    let (items, ideps) = self.parse_comma_sep_exprs(TokenType::RCurly);
                    let loc = cur.loc.combine(self.cur_token().get_source_ref());
                    self.consume(
                        TokenType::RCurly,
                        "a right curly brace (}) to terminate the array literal.",
                    );
                    adeps.extend(ideps);
                    (
                        Expr::StaticArray {
                            ty: arr_ty,
                            items,
                            loc,
                        },
                        adeps,
                    )
                } else {
                    (Expr::TypeAsExpr { ty: arr_ty }, adeps)
                }
            }
            TokenType::LParen => {
                self.advance();
                let (expr, mut deps) = self.parse_expr(None);

                if matches!(self.cur_token().ty, TokenType::Comma) {
                    self.advance();
                    let (mut exprs, edeps) = self.parse_comma_sep_exprs(TokenType::RParen);
                    deps.extend(edeps);
                    let loc = cur
                        .get_source_ref()
                        .combine(self.cur_token().get_source_ref());
                    self.consume(
                        TokenType::RParen,
                        "a right parenthesis [)] to terminate the tuple expression.",
                    );
                    exprs.insert(0, expr);
                    return (Expr::Tuple { items: exprs, loc }, deps);
                } else {
                    let loc = cur
                        .get_source_ref()
                        .combine(self.cur_token().get_source_ref());
                    self.consume(
                        TokenType::RParen,
                        "a right parenthesis [)] to terminate the grouped expression.",
                    );
                    return (
                        Expr::GroupedExpr {
                            inner: Box::new(expr),
                            loc,
                        },
                        deps,
                    );
                }
            }
            TokenType::Underscore => {
                self.advance();
                (
                    Expr::Underscore {
                        loc: cur.get_source_ref(),
                    },
                    vec![],
                )
            }
            TokenType::BackTick => {
                todo!()
            }
            TokenType::I8
            | TokenType::I16
            | TokenType::I32
            | TokenType::I64
            | TokenType::Int
            | TokenType::U8
            | TokenType::U16
            | TokenType::U32
            | TokenType::U64
            | TokenType::UInt
            | TokenType::Str
            | TokenType::Char
            | TokenType::Void
            | TokenType::Bool
            | TokenType::Type => {
                let (ty, tdeps) = self.parse_base_type();
                (Expr::TypeAsExpr { ty }, tdeps)
            }
            _ => {
                let span = cur.get_source_ref();
                self.report_err(ParseError::CannotParseAnExpression(span.clone()));
                (Expr::ErrorExpr { loc: span }, vec![])
            }
        }
    }

    fn parse_identifier(&mut self) -> (Expr, Vec<Dependency>) {
        let loc = self.cur_token().get_source_ref();
        self.consume(TokenType::Identifier, "an identifier.");
        (
            Expr::Identifier { loc: loc.clone() },
            vec![Dependency {
                ty: DependencyTy::Name,
                name_loc: loc,
            }],
        )
    }
}
