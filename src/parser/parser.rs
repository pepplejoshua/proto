#![allow(unused)]

use std::{
    collections::{HashMap, HashSet, VecDeque},
    rc::Rc,
};

use crate::{
    lexer::{
        lexer::Lexer,
        token::{SrcToken, TokenType},
    },
    parser::{ast::UnaryOpType, type_signature::Ty},
    source::{
        errors::{LexError, ParseError},
        source::{SourceRef, SourceReporter},
    },
};

use super::ast::{BinOpType, Expr, FnParam, Ins};

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
            let ins = self.next_ins(false);

            if ins.get_id(&self.lexer.src).is_some() {
                instructions.push(ins);
            } else {
                self.report_err(ParseError::ParsedInstructionIsNotAllowedAtThisLevel {
                    level: "top level".into(),
                    src: ins.get_source_ref(),
                });
            }
        }

        instructions
    }

    fn next_ins(&mut self, require_terminator: bool) -> Ins {
        let token = self.cur_token();
        let mut check_terminator = false;
        let ins = match token.ty {
            TokenType::Identifier => {
                self.advance();
                let cur = self.cur_token();
                self.parse_expr_ins(Some(Expr::Identifier {
                    name: Rc::new(token.as_str(&self.lexer.src)),
                    loc: token.get_source_ref(),
                }))
            }
            TokenType::Var => {
                check_terminator = true;
                self.parse_variable_decl(true)
            }
            TokenType::Const => {
                check_terminator = true;
                self.parse_variable_decl(false)
            }
            TokenType::Pub => {
                self.advance();
                let mut ins = self.next_ins(require_terminator);
                let loc = token.loc.combine(ins.get_source_ref());
                if ins.get_id(&self.lexer.src).is_none() {
                    self.report_err(ParseError::MalformedPubDeclaration { src: loc.clone() });
                }
                ins.make_public();
                ins
            }
            TokenType::Fn => self.parse_fn_decl(),
            TokenType::Type => {
                check_terminator = true;
                self.parse_type_alias()
            }
            TokenType::Struct => self.parse_struct_decl(),
            TokenType::Comment => {
                self.advance();
                Ins::SingleLineComment {
                    content: Rc::new(token.as_str(&self.lexer.src)),
                    loc: token.loc.clone(),
                }
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
                "a semicolon (;) to terminate instruction.",
            );
        } else if check_terminator {
            if self.cur_token().ty == TokenType::Semicolon {
                self.advance();
            }
        }
        ins
    }

    fn parse_variable_decl(&mut self, is_mutable: bool) -> Ins {
        let mut loc = self.cur_token().loc.clone();
        self.advance();
        let name = self.parse_identifier();

        let ty = if self.cur_token().ty != TokenType::Assign {
            Some(self.parse_type())
        } else {
            None
        };

        self.consume(
            TokenType::Assign,
            "an assignment token (=) to separate the name (and optional type) and the initialization value.",
        );

        let init_val = if is_mutable && self.cur_token().ty == TokenType::Underscore {
            if !is_mutable {
                self.report_err(ParseError::MalformedDeclaration(
                    "Cannot use _ to initialize a constant".into(),
                    self.cur_token().get_source_ref(),
                ));
            }
            if ty.is_none() {
                self.report_err(ParseError::MalformedDeclaration(
                    "Variable initialized with _ must have explicit type annotation.".into(),
                    self.cur_token().get_source_ref(),
                ));
            }
            loc = loc.combine(self.cur_token().loc);
            self.advance();
            None
        } else {
            let val = self.parse_expr(None);
            loc = loc.combine(val.get_source_ref());
            Some(val)
        };

        Ins::DeclVariable {
            name,
            ty,
            init_val,
            loc,
            is_mutable,
            is_public: false,
        }
    }

    fn parse_return(&mut self) -> Ins {
        let mut loc = self.cur_token().get_source_ref();
        self.advance();
        let cur = self.cur_token();
        if matches!(cur.ty, TokenType::Semicolon) {
            loc = loc.combine(cur.get_source_ref());
            Ins::Return { expr: None, loc }
        } else {
            let val = self.parse_expr(None);
            loc = loc.combine(val.get_source_ref());
            Ins::Return {
                expr: Some(val),
                loc,
            }
        }
    }

    fn parse_ins_block(&mut self) -> Ins {
        let mut loc = self.cur_token().loc;
        self.consume(
            TokenType::LCurly,
            "a left curly brace ({) to begin the body of the block instruction.",
        );

        let mut code = vec![];
        while !self.is_at_eof() {
            if self.cur_token().ty == TokenType::RCurly {
                break;
            }

            let ins = self.next_ins(false);
            code.push(ins);
        }

        loc = loc.combine(self.cur_token().loc);
        self.consume(
            TokenType::RCurly,
            "a right curly brace (}) to terminate the body of the block instruction.",
        );
        Ins::Block { code, loc }
    }

    fn parse_loop(&mut self) -> Ins {
        let mut loc = self.cur_token().loc;
        self.advance();

        match self.cur_token().ty {
            // infinite loop
            TokenType::LCurly => {
                let block = self.parse_ins_block();
                loc = loc.combine(block.get_source_ref());
                Ins::InfiniteLoop {
                    block: Box::new(block),
                    loc,
                }
            }
            // conventional loop
            TokenType::LParen => {
                self.advance();
                let loop_var = self.next_ins(false);
                if let Ins::DeclVariable { is_mutable, .. } = loop_var {
                    if !is_mutable {
                        self.expected_err_parse(
                            "a variable declaration instruction for the loop variable.",
                            loop_var.get_source_ref(),
                        );
                    }
                } else {
                    self.expected_err_parse(
                        "a variable declaration instruction for the loop variable.",
                        loop_var.get_source_ref(),
                    );
                }
                let loop_cond = self.parse_expr(None);
                self.consume(
                    TokenType::Semicolon,
                    "a semicolon to terminate the condition expression of the loop.",
                );
                let loop_update = self.next_ins(false);
                self.consume(
                    TokenType::RParen,
                    "a right parenthesis [)] to terminate the conventional loop header.",
                );
                let body = self.parse_ins_block();

                loc = loc.combine(body.get_source_ref());
                Ins::RegLoop {
                    init: Box::new(loop_var),
                    loop_cond,
                    update: Box::new(loop_update),
                    block: Box::new(body),
                    loc,
                }
            }
            _ => {
                let target_expr = self.parse_expr(None);
                match self.cur_token().ty {
                    //iterator loop
                    TokenType::In => {
                        self.advance();
                        let loop_target = self.parse_expr(None);
                        let body = self.parse_ins_block();
                        loc = loc.combine(body.get_source_ref());
                        Ins::ForInLoop {
                            loop_var: target_expr,
                            loop_target,
                            block: Box::new(body),
                            loc,
                        }
                    }
                    // while with a post code
                    TokenType::Colon => {
                        self.advance();
                        self.consume(
                            TokenType::LParen,
                            "a left parenthesis [(] to start the loop post-instruction.",
                        );
                        let post_code = self.next_ins(false);
                        self.consume(
                            TokenType::RParen,
                            "a right parenthesis [)] to terminate the loop post-instruction.",
                        );
                        let body = self.parse_ins_block();
                        loc = loc.combine(body.get_source_ref());
                        Ins::WhileLoop {
                            cond: target_expr,
                            post_code: Some(Box::new(post_code)),
                            block: Box::new(body),
                            loc,
                        }
                    }
                    _ => {
                        let body = self.parse_ins_block();
                        loc = loc.combine(body.get_source_ref());
                        Ins::WhileLoop {
                            cond: target_expr,
                            post_code: None,
                            block: Box::new(body),
                            loc,
                        }
                    }
                }
            }
        }
    }

    fn parse_if_conditional(&mut self) -> Ins {
        let mut loc = self.cur_token().loc;
        self.advance();

        let mut conds_and_code = vec![];
        // get the first condition and body
        let first_cond = self.parse_expr(None);
        let first_body = self.parse_ins_block();
        conds_and_code.push((Some(first_cond), first_body));

        while !self.is_at_eof() {
            if self.cur_token().ty != TokenType::Else {
                break;
            }
            self.advance();
            if self.cur_token().ty == TokenType::If {
                self.advance();
                let else_if_cond = self.parse_expr(None);
                let else_if_code = self.parse_ins_block();
                conds_and_code.push((Some(else_if_cond), else_if_code));
            } else {
                let else_body = self.parse_ins_block();
                conds_and_code.push((None, else_body));
                break;
            }
        }

        loc = loc.combine(conds_and_code.last().unwrap().1.get_source_ref());

        Ins::IfConditional {
            conds_and_code,
            loc,
        }
    }

    fn parse_print(&mut self) -> Ins {
        let is_println = self.cur_token().ty == TokenType::Print;
        let mut loc = self.cur_token().loc;
        self.advance();

        self.consume(
            TokenType::LParen,
            "a left parenthesis [(] to start argument list.",
        );

        let output = self.parse_expr(None);
        loc = loc.combine(self.cur_token().loc);
        self.consume(
            TokenType::RParen,
            "a right parenthesis [)] to terminate argument list.",
        );

        Ins::PrintIns {
            is_println,
            output,
            loc,
        }
    }

    fn parse_loop_control_ins(&mut self, ty: TokenType) -> Ins {
        let loc = self.cur_token().get_source_ref();
        self.advance();
        if ty == TokenType::Break {
            Ins::Break { loc }
        } else {
            Ins::Continue { loc }
        }
    }

    fn parse_type(&mut self) -> Rc<Ty> {
        let start_loc = self.cur_token().get_source_ref();
        let cur = self.cur_token();
        match cur.ty {
            TokenType::LBracket => {
                self.advance();

                if self.cur_token().ty == TokenType::RBracket {
                    // Slice type
                    self.advance();
                    let sub_ty = self.parse_type();
                    let end_loc = sub_ty.get_loc();
                    let loc = start_loc.combine(end_loc);

                    return Rc::new(Ty::Slice { sub_ty, loc });
                } else {
                    // Static array type
                    let size = self.parse_primary();
                    self.consume(TokenType::RBracket, "a right bracket (]) after array size");

                    let sub_ty = self.parse_type();
                    let end_loc = sub_ty.get_loc();
                    let loc = start_loc.combine(end_loc);

                    return Rc::new(Ty::StaticArray { sub_ty, size, loc });
                }
            }
            TokenType::QuestionMark => {
                self.advance();
                let ty = self.parse_type();
                Rc::new(Ty::Optional {
                    loc: cur.get_source_ref().combine(ty.get_loc()),
                    sub_ty: ty,
                })
            }
            TokenType::Star => {
                self.advance();
                let ty = self.parse_type();
                Rc::new(Ty::Pointer {
                    loc: cur.get_source_ref().combine(ty.get_loc()),
                    sub_ty: ty,
                })
            }
            TokenType::Identifier => {
                self.advance();
                if self.cur_token().ty == TokenType::Dot {
                    let mut current = Ty::NamedType {
                        name: Rc::new(cur.as_str(&self.lexer.src)),
                        loc: cur.loc.clone(),
                    };

                    while self.cur_token().ty == TokenType::Dot {
                        self.advance();
                        let member = self.parse_identifier();
                        let loc = current.get_loc().combine(member.get_source_ref());
                        current = Ty::AccessMemberType {
                            target: Rc::new(current),
                            mem: Rc::new(Ty::NamedType {
                                name: Rc::new(member.as_str()),
                                loc: member.get_source_ref(),
                            }),
                            loc,
                        };
                    }
                    return Rc::new(current);
                }
                return Rc::new(Ty::NamedType {
                    name: Rc::new(cur.as_str(&self.lexer.src)),
                    loc: cur.loc.clone(),
                });
            }
            _ => self.parse_base_type(),
        }
    }

    fn parse_base_type(&mut self) -> Rc<Ty> {
        let token = self.cur_token();
        let loc = token.get_source_ref();
        self.advance();

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
            _ => {
                self.expected_err_token("a type.");
                Ty::ErrorType { loc: loc.clone() }
            }
        };
        Rc::new(ty)
    }

    fn parse_expr_ins(&mut self, expr: Option<Expr>) -> Ins {
        let expr = self.parse_expr(expr);
        let cur = self.cur_token();
        match cur.ty {
            TokenType::Assign => {
                self.advance();
                let val = self.parse_expr(None);
                let span = expr.get_source_ref().combine(val.get_source_ref());
                Ins::AssignTo {
                    target: expr,
                    value: val,
                    loc: span,
                }
            }
            TokenType::PlusAssign => {
                self.advance();
                let val = self.parse_expr(None);
                let span = expr.get_source_ref().combine(val.get_source_ref());
                Ins::AssignTo {
                    target: expr.clone(),
                    value: Expr::BinOp {
                        op: BinOpType::Add,
                        left: Box::new(expr),
                        right: Box::new(val),
                        loc: span.clone(),
                    },
                    loc: span,
                }
            }
            TokenType::MinusAssign => {
                self.advance();
                let val = self.parse_expr(None);
                let span = expr.get_source_ref().combine(val.get_source_ref());
                Ins::AssignTo {
                    target: expr.clone(),
                    value: Expr::BinOp {
                        op: BinOpType::Sub,
                        left: Box::new(expr),
                        right: Box::new(val),
                        loc: span.clone(),
                    },
                    loc: span,
                }
            }
            TokenType::StarAssign => {
                self.advance();
                let val = self.parse_expr(None);
                let span = expr.get_source_ref().combine(val.get_source_ref());
                Ins::AssignTo {
                    target: expr.clone(),
                    value: Expr::BinOp {
                        op: BinOpType::Mult,
                        left: Box::new(expr),
                        right: Box::new(val),
                        loc: span.clone(),
                    },
                    loc: span,
                }
            }
            TokenType::SlashAssign => {
                self.advance();
                let val = self.parse_expr(None);
                let span = expr.get_source_ref().combine(val.get_source_ref());
                Ins::AssignTo {
                    target: expr.clone(),
                    value: Expr::BinOp {
                        op: BinOpType::Div,
                        left: Box::new(expr),
                        right: Box::new(val),
                        loc: span.clone(),
                    },
                    loc: span,
                }
            }
            TokenType::ModuloAssign => {
                self.advance();
                let val = self.parse_expr(None);
                let span = expr.get_source_ref().combine(val.get_source_ref());
                Ins::AssignTo {
                    target: expr.clone(),
                    value: Expr::BinOp {
                        op: BinOpType::Mod,
                        left: Box::new(expr),
                        right: Box::new(val),
                        loc: span.clone(),
                    },
                    loc: span,
                }
            }
            _ => Ins::ExprIns {
                loc: expr.get_source_ref(),
                expr,
            },
        }
    }

    fn parse_type_alias(&mut self) -> Ins {
        let mut loc = self.cur_token().loc.clone();
        self.advance();
        let alias_name = self.parse_identifier();
        self.consume(
            TokenType::Assign,
            "an assignment token (=) to separate type alias name and type.",
        );
        let ty = self.parse_type();
        loc = loc.combine(ty.get_loc());
        Ins::DeclTypeAlias {
            name: alias_name,
            ty,
            loc,
            is_public: false,
        }
    }

    fn parse_struct_decl(&mut self) -> Ins {
        todo!()
    }

    fn parse_fn_decl(&mut self) -> Ins {
        let mut loc = self.cur_token().loc.clone();
        self.advance();
        let fn_name = self.parse_identifier();
        let fn_params = self.parse_fn_params();
        let ret_type = self.parse_type();
        let body = self.parse_ins_block();
        loc = loc.combine(body.get_source_ref());
        Ins::DeclFunc {
            name: fn_name,
            params: fn_params,
            ret_ty: ret_type,
            body: Rc::new(body),
            is_public: false,
            loc,
        }
    }

    fn parse_fn_params(&mut self) -> Vec<FnParam> {
        self.consume(
            TokenType::LParen,
            "a left parenthesis [(] to begin the list of parameters.",
        );

        let mut params = vec![];
        while !self.is_at_eof() {
            if self.cur_token().ty == TokenType::RParen {
                break;
            }

            let (is_comptime, is_mutable) = if self.cur_token().ty == TokenType::Comptime {
                self.advance();
                (true, false)
            } else if self.cur_token().ty == TokenType::Var {
                self.advance();
                (false, true)
            } else {
                (false, false)
            };

            let param_name = self.parse_identifier();
            let is_self = param_name.as_str() == "self";
            let param_ty = self.parse_type();

            let loc = param_name.get_source_ref().combine(param_ty.get_loc());
            params.push(FnParam {
                name: param_name,
                given_ty: param_ty,
                is_self,
                is_comptime,
                is_mutable,
                loc,
            });

            if matches!(self.cur_token().ty, TokenType::Comma) {
                self.advance();
            }
        }

        self.consume(
            TokenType::RParen,
            "a right parenthesis [)] to terminate the list of parameters.",
        );

        params
    }

    fn parse_expr(&mut self, lhs: Option<Expr>) -> Expr {
        if lhs.is_some() {
            self.parse_ternary(lhs)
        } else {
            self.parse_lambda_or_unary_dot()
        }
    }

    fn parse_initializer_list(&mut self, target: Option<Rc<Expr>>, dot: Rc<SourceRef>) -> Expr {
        self.consume(
            TokenType::LCurly,
            "a left curly brace ({) to start the initializer list.",
        );

        let mut pairs = vec![];
        while !self.is_at_eof() {
            if self.cur_token().ty == TokenType::RCurly {
                break;
            }

            let expr = self.parse_expr(None);

            if self.cur_token().ty == TokenType::Assign {
                self.advance();
                let val = self.parse_expr(None);
                pairs.push((Box::new(expr), Some(Box::new(val))));
            } else {
                pairs.push((Box::new(expr), None));
            }

            if matches!(self.cur_token().ty, TokenType::Comma) {
                self.advance();
            }
        }
        let loc = dot.combine(self.cur_token().loc);
        self.consume(
            TokenType::RCurly,
            "a right curly brace (}) to terminate the initializer list.",
        );

        Expr::InitializerList { target, pairs, loc }
    }

    fn parse_lambda_or_unary_dot(&mut self) -> Expr {
        let cur = self.cur_token();
        match cur.ty {
            TokenType::BackSlash => {
                self.advance();
                let params = self.parse_fn_params();
                let ret_type = self.parse_type();
                let body = self.parse_ins_block();
                let loc = cur.get_source_ref().combine(body.get_source_ref());

                Expr::Lambda {
                    params,
                    ret_type,
                    body: Box::new(body),
                    loc,
                }
            }
            TokenType::Dot => {
                let start = cur.loc.clone();
                self.advance();
                self.parse_initializer_list(None, start)
            }
            _ => self.parse_optional_expr_or_comptime_expr(),
        }
    }

    fn parse_optional_expr_or_comptime_expr(&mut self) -> Expr {
        let cur = self.cur_token();
        match cur.ty {
            TokenType::Some => {
                self.advance();
                let val = self.parse_expr(None);
                Expr::OptionalExpr {
                    loc: cur.get_source_ref().combine(val.get_source_ref()),
                    val: Some(Box::new(val)),
                }
            }
            TokenType::None => {
                self.advance();
                Expr::OptionalExpr {
                    loc: cur.get_source_ref(),
                    val: None,
                }
            }
            TokenType::Comptime => {
                self.advance();
                let comptime_expr = self.parse_ternary(None);
                let loc = cur.get_source_ref().combine(comptime_expr.get_source_ref());
                Expr::ComptimeExpr {
                    val: Box::new(comptime_expr),
                    loc,
                }
            }
            _ => self.parse_ternary(None),
        }
    }

    fn parse_ternary(&mut self, lhs: Option<Expr>) -> Expr {
        let lhs = self.parse_or(lhs);

        if matches!(self.cur_token().ty, TokenType::QuestionMark) {
            self.advance();
            let then = self.parse_expr(None);
            self.consume(
                TokenType::Colon,
                "a colon (:) to separate both branches of the ternary condition expression.",
            );
            let otherwise = self.parse_expr(None);
            return Expr::ConditionalExpr {
                loc: lhs.get_source_ref().combine(otherwise.get_source_ref()),
                cond: Box::new(lhs),
                then: Box::new(then),
                otherwise: Box::new(otherwise),
            };
        }
        lhs
    }

    fn parse_or(&mut self, lhs: Option<Expr>) -> Expr {
        let mut lhs = self.parse_and(lhs);

        while matches!(self.cur_token().ty, TokenType::Or) {
            self.advance();
            let rhs = self.parse_and(None);
            let loc = lhs.get_source_ref().combine(rhs.get_source_ref());
            lhs = Expr::BinOp {
                op: BinOpType::Or,
                left: Box::new(lhs),
                right: Box::new(rhs),
                loc,
            };
        }
        lhs
    }

    fn parse_and(&mut self, lhs: Option<Expr>) -> Expr {
        let mut lhs = self.parse_equality(lhs);

        while matches!(self.cur_token().ty, TokenType::And) {
            self.advance();
            let rhs = self.parse_equality(None);
            let loc = lhs.get_source_ref().combine(rhs.get_source_ref());
            lhs = Expr::BinOp {
                op: BinOpType::And,
                left: Box::new(lhs),
                right: Box::new(rhs),
                loc,
            };
        }
        lhs
    }

    fn parse_equality(&mut self, lhs: Option<Expr>) -> Expr {
        let mut lhs = self.parse_comparison(lhs);

        while matches!(self.cur_token().ty, TokenType::Equal | TokenType::NotEqual) {
            let op = if self.cur_token().ty == TokenType::Equal {
                BinOpType::Eq
            } else {
                BinOpType::Neq
            };
            self.advance();
            let rhs = self.parse_comparison(None);
            let loc = lhs.get_source_ref().combine(rhs.get_source_ref());
            lhs = Expr::BinOp {
                op,
                left: Box::new(lhs),
                right: Box::new(rhs),
                loc,
            };
        }
        lhs
    }

    fn parse_comparison(&mut self, lhs: Option<Expr>) -> Expr {
        let mut lhs = self.parse_term(lhs);

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
            let rhs = self.parse_term(None);
            let loc = lhs.get_source_ref().combine(rhs.get_source_ref());
            lhs = Expr::BinOp {
                op,
                left: Box::new(lhs),
                right: Box::new(rhs),
                loc,
            };
        }
        lhs
    }

    fn parse_term(&mut self, lhs: Option<Expr>) -> Expr {
        let mut lhs = self.parse_factor(lhs);

        while matches!(self.cur_token().ty, TokenType::Plus | TokenType::Minus) {
            let op = if self.cur_token().ty == TokenType::Plus {
                BinOpType::Add
            } else {
                BinOpType::Sub
            };
            self.advance();
            let rhs = self.parse_factor(None);
            let loc = lhs.get_source_ref().combine(rhs.get_source_ref());
            lhs = Expr::BinOp {
                op,
                left: Box::new(lhs),
                right: Box::new(rhs),
                loc,
            };
        }
        lhs
    }

    fn parse_factor(&mut self, lhs: Option<Expr>) -> Expr {
        let mut lhs = if lhs.is_some() {
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
            let rhs = self.parse_unary();
            let loc = lhs.get_source_ref().combine(rhs.get_source_ref());
            lhs = Expr::BinOp {
                op,
                left: Box::new(lhs),
                right: Box::new(rhs),
                loc,
            };
        }
        lhs
    }

    fn parse_unary(&mut self) -> Expr {
        let op = self.cur_token();
        match op.ty {
            TokenType::Minus | TokenType::Not => {
                self.advance();
                let rhs = self.parse_unary();
                let loc = op.get_source_ref().combine(rhs.get_source_ref());
                let op = if op.ty == TokenType::Minus {
                    UnaryOpType::Negate
                } else {
                    UnaryOpType::Not
                };
                Expr::UnaryOp {
                    op,
                    expr: Box::new(rhs),
                    loc,
                }
            }
            _ => self.parse_ptr_deref_or_addr_of(),
        }
    }

    fn parse_ptr_deref_or_addr_of(&mut self) -> Expr {
        let op = self.cur_token();
        match op.ty {
            TokenType::Star => {
                self.advance();
                let rhs = self.parse_ptr_deref_or_addr_of();
                let loc = op.get_source_ref().combine(rhs.get_source_ref());
                Expr::DerefPtr {
                    loc,
                    target: Box::new(rhs),
                }
            }
            TokenType::Ampersand => {
                self.advance();
                let rhs = self.parse_index_expr(None);
                let loc = op.get_source_ref().combine(rhs.get_source_ref());
                Expr::MakePtrFromAddrOf {
                    loc,
                    target: Box::new(rhs),
                }
            }
            _ => self.parse_index_expr(None),
        }
    }

    fn parse_comma_sep_exprs(&mut self, terminator_ty: TokenType) -> Vec<Expr> {
        let mut args = vec![];

        while !self.is_at_eof() {
            if self.cur_token().ty == terminator_ty {
                break;
            }

            let arg = self.parse_expr(None);
            args.push(arg);

            if matches!(self.cur_token().ty, TokenType::Comma) {
                self.advance();
            }
        }
        args
    }

    fn parse_index_expr(&mut self, lhs: Option<Expr>) -> Expr {
        let mut lhs = if let Some(lhs) = lhs {
            // we have no other sub trees that will start with an expression
            lhs
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
                    let args = self.parse_comma_sep_exprs(TokenType::RParen);
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

                    lhs = Expr::CallFn {
                        func: Box::new(lhs),
                        args,
                        loc,
                    };
                }
                TokenType::LBracket => {
                    if self.cur_token().ty == TokenType::Colon {
                        self.advance();
                        let end_excl = match self.cur_token().ty {
                            TokenType::RBracket => None,
                            _ => {
                                let end = self.parse_expr(None);
                                Some(Box::new(end))
                            }
                        };
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

                    let index_expr = self.parse_expr(None);
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
                            let end_excl = match self.cur_token().ty {
                                TokenType::RBracket => None,
                                _ => {
                                    let end = self.parse_expr(None);
                                    Some(Box::new(end))
                                }
                            };
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
                    if self.cur_token().ty == TokenType::LCurly {
                        lhs = self.parse_initializer_list(Some(Rc::new(lhs)), op.loc.clone());
                        return lhs;
                    } else {
                        let member = self.parse_primary();
                        let loc = op.loc.combine(member.get_source_ref());
                        lhs = Expr::AccessMember {
                            target: Box::new(lhs),
                            mem: Box::new(member),
                            loc,
                        };
                    }
                }
                _ => unreachable!("unexpected operator in parse_index_expr loop: {:#?}", op),
            }
        }
        lhs
    }

    fn parse_primary(&mut self) -> Expr {
        let cur = self.cur_token();
        match cur.ty {
            TokenType::Identifier => self.parse_identifier(),
            TokenType::Character => {
                self.advance();
                Expr::Char {
                    content: Rc::new(cur.as_str(&self.lexer.src)),
                    loc: cur.loc,
                }
            }
            TokenType::String => {
                self.advance();
                Expr::Str {
                    content: Rc::new(cur.as_str(&self.lexer.src)),
                    loc: cur.loc,
                }
            }
            TokenType::Integer => {
                self.advance();
                // make sure to strip all underscores from integer token
                let integer: String = cur
                    .as_str(&self.lexer.src)
                    .chars()
                    .filter(|&ch| ch != '_')
                    .collect();

                Expr::Integer {
                    content: Rc::new(integer),
                    loc: cur.loc,
                }
            }
            TokenType::Decimal => {
                self.advance();
                // make sure to strip all underscores from decimal token
                let mut decimal: String = cur
                    .as_str(&self.lexer.src)
                    .chars()
                    .filter(|&ch| ch != '_')
                    .collect();
                // ensure the decimal has a mantissa part. Otherwise, append a '0'
                if decimal.chars().last() == Some('.') {
                    decimal.push('0');
                }

                Expr::Decimal {
                    content: Rc::new(decimal),
                    loc: cur.loc,
                }
            }
            TokenType::True => {
                self.advance();
                Expr::Bool {
                    val: true,
                    loc: cur.loc,
                }
            }
            TokenType::False => {
                self.advance();
                Expr::Bool {
                    val: false,
                    loc: cur.loc,
                }
            }
            TokenType::LBracket => {
                // we are parsing either an array type, or an array literal,
                // with a preceding array type
                let arr_ty = self.parse_type();
                Expr::TypeAsExpr { ty: arr_ty }
            }
            TokenType::LParen => {
                self.advance();
                let expr = self.parse_expr(None);

                if matches!(self.cur_token().ty, TokenType::Comma) {
                    self.advance();
                    let mut exprs = self.parse_comma_sep_exprs(TokenType::RParen);
                    let loc = cur
                        .get_source_ref()
                        .combine(self.cur_token().get_source_ref());
                    self.consume(
                        TokenType::RParen,
                        "a right parenthesis [)] to terminate the tuple expression.",
                    );
                    exprs.insert(0, expr);
                    return Expr::Tuple { items: exprs, loc };
                } else {
                    let loc = cur
                        .get_source_ref()
                        .combine(self.cur_token().get_source_ref());
                    self.consume(
                        TokenType::RParen,
                        "a right parenthesis [)] to terminate the grouped expression.",
                    );
                    return Expr::GroupedExpr {
                        inner: Box::new(expr),
                        loc,
                    };
                }
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
            | TokenType::Bool => {
                let ty = self.parse_base_type();
                Expr::TypeAsExpr { ty }
            }
            _ => {
                let span = cur.get_source_ref();
                self.report_err(ParseError::CannotParseAnExpression(span.clone()));
                Expr::ErrorExpr { loc: span }
            }
        }
    }

    fn parse_identifier(&mut self) -> Expr {
        let loc = self.cur_token().get_source_ref();
        let name = Rc::new(self.cur_token().as_str(&self.lexer.src));
        self.consume(TokenType::Identifier, "an identifier.");
        Expr::Identifier {
            name,
            loc: loc.clone(),
        }
    }
}

#[cfg(test)]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct ParserTestResult {
    code_str_repr: Vec<String>,
    lexer_errors: Vec<String>,
    parser_errors: Vec<String>,
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

        let top_level = parser.parse_file();
        let code_str_repr = top_level
            .iter()
            .map(|i| i.as_str())
            .collect::<Vec<String>>();
        let lexer_errors = parser
            .lex_errs
            .iter()
            .map(|e| format!("{:?}", e))
            .collect::<Vec<String>>();
        let parser_errors = parser
            .parse_errs
            .iter()
            .map(|e| format!("{:?}", e))
            .collect::<Vec<String>>();

        let result = ParserTestResult {
            code_str_repr,
            lexer_errors,
            parser_errors,
        };

        insta::assert_yaml_snapshot!(result);
    });
}
