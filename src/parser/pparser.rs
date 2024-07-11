#![allow(unused)]

use std::{collections::HashMap, f64::consts::PI, iter::MapWhile, rc::Rc};

use crate::{
    lexer::{lexer::Lexer, token::Token},
    parser::ast::FnParam,
    source::{
        errors::{LexError, ParseError, ParseWarning},
        source::{SourceRef, SourceReporter},
    },
    types::signature::Ty,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DepTy {
    Func,
    Struct,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Dep {
    ty: DepTy,
    to: String,
    loc: Rc<SourceRef>,
}

pub struct Parser {
    pub lexer: Lexer,
    pub lex_errs: Vec<LexError>,
    pub parse_errs: Vec<ParseError>,
    pub parse_warns: Vec<ParseWarning>,
    pub file_mod: FileModule,
    cur_deps: Vec<Dep>,
    scope: Vec<ParseScope>,
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
            scope: vec![ParseScope::TopLevel],
            lexed_token: None,
            cur_deps: vec![],
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
        let mut ood = HashMap::new();
        let mut instructions = vec![];
        while !self.is_at_eof() {
            let ins = self.next_ins(true, false);
            let ins_id = ins.get_id();

            if ins_id.is_some() {
                ood.insert(ins_id.unwrap(), self.cur_deps.clone());
            }
            self.cur_deps.clear();
            instructions.push(ins);
        }

        // for (ins_s, deps) in ood.iter() {
        //     println!("{ins_s}");
        //     for dep in deps.iter() {
        //         println!("  - {}", dep.to);
        //     }
        //     println!()
        // }

        // topological sort will be done here. It will detect cycles and report them
        let mut ood = self.topological_sort(&mut ood);

        if !ood.is_empty() {
            // sort the instructions based on the topological sort
            let mut sorted_ins = vec![];
            for node in ood.iter() {
                let node = node.as_str();
                let ins = instructions
                    .iter()
                    .find(|ins| ins.get_id().is_some() && ins.get_id().unwrap() == node);
                if let Some(ins) = ins {
                    sorted_ins.push(ins.clone());
                }
            }

            self.file_mod.top_level = sorted_ins;
        } else {
            self.file_mod.top_level = instructions;
        }
    }

    fn topological_sort(&mut self, ood: &mut HashMap<String, Vec<Dep>>) -> Vec<String> {
        let mut sorted = vec![];
        let mut visited = HashMap::new();
        let mut cycle = vec![];
        let mut cycle_found = false;
        let mut found_error = false;

        for node in ood.keys() {
            if !visited.contains_key(node) {
                if let Some(_) = self.visit(
                    node,
                    ood,
                    &mut visited,
                    &mut sorted,
                    &mut cycle,
                    &mut cycle_found,
                ) {
                    found_error = true;
                    cycle_found = false;
                    cycle.clear();
                    continue;
                }
            }
        }
        if found_error {
            vec![]
        } else {
            sorted
        }
    }

    fn visit(
        &mut self,
        node: &str,
        ood: &HashMap<String, Vec<Dep>>,
        visited: &mut HashMap<String, bool>,
        sorted: &mut Vec<String>,
        cycle: &mut Vec<String>,
        cycle_found: &mut bool,
    ) -> Option<()> {
        if let Some(&is_visited) = visited.get(node) {
            if is_visited {
                return None;
            } else {
                return Some(());
            }
        } else {
        }

        visited.insert(node.to_string(), false);
        cycle.push(node.to_string());

        if let Some(deps) = ood.get(node) {
            for dep in deps.iter() {
                // self dependency is allowed
                if dep.to == node {
                    continue;
                }
                if let Some(_) = self.visit(&dep.to, ood, visited, sorted, cycle, cycle_found) {
                    if !*cycle_found {
                        cycle.push(dep.to.clone());
                        let mut cycle_str = cycle.join(" -> ");
                        self.report_error(ParseError::CyclicalDependencyBetweenNodes {
                            cycle: cycle_str,
                            src: dep.loc.clone(),
                        });
                        // pop dep.to
                        cycle.pop();
                        // pop node
                        cycle.pop();
                        *cycle_found = true;
                    }
                    visited.insert(node.to_string(), true);
                    return Some(());
                }
            }
        }

        visited.insert(node.to_string(), true);
        cycle.pop();
        sorted.push(node.to_string());
        None
    }

    fn next_ins(&mut self, use_new_scope: bool, require_term: bool) -> Ins {
        let cur = self.cur_token();
        let mut check_term = false;
        let ins = match cur {
            Token::Defer(_) => self.parse_defer(),
            Token::Fn(_) => self.parse_fn(),
            Token::For(_) => self.parse_loop(),
            Token::Struct(_) => self.parse_struct(),
            Token::Mod(_) => self.parse_module(),
            Token::Print(_) | Token::Println(_) => {
                check_term = true;
                self.parse_print()
            }
            Token::Break(_) => {
                check_term = true;
                self.parse_loop_control_ins()
            }
            Token::Continue(_) => {
                check_term = true;
                self.parse_loop_control_ins()
            }
            Token::SingleLineComment(loc, comment) => {
                self.advance();
                Ins::SingleLineComment { comment, loc }
            }
            Token::Return(_) => {
                check_term = true;
                self.parse_return()
            }
            Token::If(_) => self.parse_if_conditional(),
            Token::LCurly(_) => self.parse_block(use_new_scope),
            _ => {
                check_term = true;
                self.parse_expr_ins()
            }
        };

        if require_term && check_term {
            if !matches!(self.cur_token(), Token::Semicolon(_)) {
                self.report_error(ParseError::Expected(
                    "a semi-colon to terminate this instruction.".into(),
                    ins.get_source_ref(),
                    None,
                ));
            } else {
                self.advance();
            }
        } else if check_term {
            if matches!(self.cur_token(), Token::Semicolon(_)) {
                self.advance();
            }
        }
        ins
    }

    fn parse_loop_control_ins(&mut self) -> Ins {
        let cur = self.cur_token();
        let mut span = cur.get_source_ref();
        self.advance();

        if matches!(cur, Token::Break(_)) {
            Ins::Break { loc: span }
        } else {
            Ins::Continue { loc: span }
        }
    }

    fn parse_loop(&mut self) -> Ins {
        let mut span = self.cur_token().get_source_ref();
        self.advance();

        // we are parsing an infinite loop
        if matches!(self.cur_token(), Token::LCurly(_)) {
            self.scope.push(ParseScope::Loop);
            let block = self.parse_block(false);
            self.scope.pop();
            span = span.combine(block.get_source_ref());
            return Ins::InfiniteLoop {
                block: Box::new(block),
                loc: span,
            };
        }

        // we are dealing with a conventional loop
        if matches!(self.cur_token(), Token::LParen(_)) {
            self.advance();
            let loop_var = self.next_ins(false, true);
            if !matches!(loop_var, Ins::DeclVar { .. }) {
                // report error
                self.report_error(ParseError::Expected(
                    "a variable declaration instruction for the loop variable.".into(),
                    loop_var.get_source_ref(),
                    None,
                ));
            }
            let loop_cond = self.parse_expr();
            if !matches!(self.cur_token(), Token::Semicolon(_)) {
                // report error
                self.report_error(ParseError::Expected(
                    "a semicolon to terminate the condition expression of the loop".into(),
                    loop_var.get_source_ref(),
                    None,
                ));
            } else {
                self.advance();
            }
            let loop_update = self.next_ins(false, false);
            if !matches!(self.cur_token(), Token::RParen(_)) {
                self.report_error(ParseError::Expected(
                    "a right parenthesis to terminate conventional loop header.".into(),
                    self.cur_token().get_source_ref(),
                    None,
                ));
            } else {
                self.advance();
            }
            self.scope.push(ParseScope::Loop);
            let body = self.parse_block(false);
            self.scope.pop();
            span = span.combine(body.get_source_ref());
            return Ins::RegLoop {
                init: Box::new(loop_var),
                loop_cond,
                update: Box::new(loop_update),
                block: Box::new(body),
                loc: span,
            };
        }
        let expr = self.parse_expr();
        let mut cur = self.cur_token();
        match (&expr, cur) {
            // iterator loop
            (_, Token::In(_)) => {
                // report error for expr if it is not an Expr::Ident {..}
                if !matches!(expr, Expr::Ident { .. }) {
                    self.report_error(ParseError::Expected(
                        "an identifier to name the loop variable.".into(),
                        expr.get_source_ref(),
                        None,
                    ));
                }
                // skip over the Token::In
                self.advance();
                // parse the iteration target
                let target = self.parse_expr();
                self.scope.push(ParseScope::Loop);
                let body = self.parse_block(false);
                self.scope.pop();
                span = span.combine(body.get_source_ref());
                Ins::ForInLoop {
                    loop_var: expr,
                    loop_target: target,
                    block: Box::new(body),
                    loc: span,
                }
            }
            // while with a post code
            (_, Token::Colon(_)) => {
                self.advance();
                cur = self.cur_token();
                self.scope.push(ParseScope::Loop);
                let post_code = if matches!(cur, Token::LParen(_)) {
                    // we are in a while with post instruction
                    self.advance();
                    let post_code = self.next_ins(true, false);
                    // we should see a )
                    if !matches!(self.cur_token(), Token::RParen(_)) {
                        self.report_error(ParseError::Expected(
                            "a right parenthesis to terminate post-instruction.".into(),
                            self.cur_token().get_source_ref(),
                            None,
                        ));
                    } else {
                        self.advance();
                    }
                    Some(Box::new(post_code))
                } else {
                    self.report_error(ParseError::Expected(
                        "a left parenthesis to begin post-instruction.".into(),
                        self.cur_token().get_source_ref(),
                        None,
                    ));
                    None
                };

                let body = self.parse_block(false);
                self.scope.pop();
                span = span.combine(body.get_source_ref());
                return Ins::WhileLoop {
                    cond: expr,
                    post_code,
                    block: Box::new(body),
                    loc: span,
                };
            }
            // parse a while
            (_, _) => {
                self.scope.push(ParseScope::Loop);
                let body = self.parse_block(false);
                self.scope.pop();
                span = span.combine(body.get_source_ref());
                Ins::WhileLoop {
                    cond: expr,
                    post_code: None,
                    block: Box::new(body),
                    loc: span,
                }
            }
        }
    }

    fn parse_defer(&mut self) -> Ins {
        let mut loc = self.cur_token().get_source_ref();
        self.advance();

        let sub_ins = self.next_ins(true, false);
        loc = loc.combine(sub_ins.get_source_ref());
        Ins::Defer {
            sub_ins: Box::new(sub_ins),
            loc,
        }
    }

    fn parse_print(&mut self) -> Ins {
        let is_println = matches!(self.cur_token(), Token::Println(_));
        let mut span = self.cur_token().get_source_ref();
        self.advance();

        let mut cur = self.cur_token();

        // look for (
        if !matches!(cur, Token::LParen(_)) {
            self.report_error(ParseError::Expected(
                format!(
                    "a left parenthesis to begin argument section for {}.",
                    if is_println { "println" } else { "print" }
                ),
                cur.get_source_ref(),
                None,
            ));
            return Ins::ErrorIns {
                loc: span.combine(cur.get_source_ref()),
            };
        }

        self.advance();

        cur = self.cur_token();
        let output = self.parse_expr();

        // look for )
        cur = self.cur_token();
        if !matches!(cur, Token::RParen(_)) {
            self.report_error(ParseError::Expected(
                format!(
                    "a right parenthesis to end argument section for {} instruction.",
                    if is_println { "println" } else { "print" }
                ),
                cur.get_source_ref(),
                None,
            ));
            return Ins::ErrorIns {
                loc: span.combine(cur.get_source_ref()),
            };
        }

        span = span.combine(cur.get_source_ref());
        self.advance();

        Ins::PrintIns {
            is_println,
            loc: span,
            output,
        }
    }

    fn parse_return(&mut self) -> Ins {
        if !self.scope.contains(&ParseScope::Function) {
            self.report_error(ParseError::ReturnInstructionOutsideFunction(
                self.cur_token().get_source_ref(),
            ));
        }
        let start_loc = self.cur_token().get_source_ref();
        self.advance();
        // check if there is semicolon, else we need a parse an expression
        let cur = self.cur_token();
        if matches!(cur, Token::Semicolon(_)) {
            let loc = start_loc.combine(cur.get_source_ref());
            Ins::Return { expr: None, loc }
        } else {
            let val = self.parse_expr();
            // TODO: determine if this triggered an error and react appropriately
            let mut return_loc = start_loc.combine(val.get_source_ref());

            Ins::Return {
                expr: Some(val),
                loc: return_loc,
            }
        }
    }

    fn parse_if_conditional(&mut self) -> Ins {
        let mut span = self.cur_token().get_source_ref();
        self.advance();

        let mut conds_and_code = vec![];
        // get the first condition and body
        let first_cond = self.parse_expr();
        let first_body = self.parse_block(true);
        conds_and_code.push((Some(first_cond), first_body));

        // check for any else-if / else branches
        let mut cur = self.cur_token();
        while !self.is_at_eof() {
            if !matches!(cur, Token::Else(_)) {
                break;
            }
            self.advance();
            // look for if token
            cur = self.cur_token();
            if matches!(cur, Token::If(_)) {
                self.advance();
                let else_if_cond = self.parse_expr();
                let else_if_code = self.parse_block(true);
                conds_and_code.push((Some(else_if_cond), else_if_code));
                cur = self.cur_token();
            } else {
                let else_body = self.parse_block(true);
                conds_and_code.push((None, else_body));
                break;
            }
        }

        span = span.combine(conds_and_code.last().unwrap().1.get_source_ref());

        Ins::IfConditional {
            conds_and_code,
            loc: span,
        }
    }

    fn parse_fn_params(&mut self) -> Vec<FnParam> {
        let mut cur = self.cur_token();

        if !matches!(cur, Token::LParen(_)) {
            self.report_error(ParseError::Expected(
                "a left parenthesis to begin the list of parameters.".to_string(),
                cur.get_source_ref(),
                None,
            ));
        } else {
            self.advance();
        }

        let mut params = vec![];
        let mut saw_rparen = false;
        while !self.is_at_eof() {
            cur = self.cur_token();
            if matches!(cur, Token::RParen(_)) {
                saw_rparen = true;
                break;
            }

            // parse parameter name and type
            let param_name = self.parse_identifier();
            let param_ty = self.parse_type();

            cur = self.cur_token();

            if matches!(cur, Token::RParen(_)) {
                saw_rparen = true;
            } else if !matches!(cur, Token::Comma(_)) {
                self.report_error(ParseError::Expected(
                    "a comma to separate parameters or a right parenthesis to terminate the list of parameters.".to_string(),
                    cur.get_source_ref(),
                    None,
                ))
            } else {
                self.advance();
            }

            let param_span = param_ty.get_loc().combine(param_name.get_source_ref());
            params.push(FnParam {
                name: param_name,
                given_ty: param_ty,
                loc: param_span,
            });

            if saw_rparen {
                break;
            }
        }

        if !saw_rparen {
            self.report_error(ParseError::Expected(
                "a right parenthesis to terminate the list of parameters.".to_string(),
                self.cur_token().get_source_ref(),
                None,
            ));
        } else {
            self.advance();
        }

        params
    }

    fn parse_fn(&mut self) -> Ins {
        let start = self.cur_token();
        self.advance();

        let fn_name = self.parse_identifier();

        let params = self.parse_fn_params();

        let ret_type = self.parse_type();
        self.scope.push(ParseScope::Function);
        let body = self.next_ins(false, false);
        self.scope.pop();
        let fn_span = start.get_source_ref().combine(body.get_source_ref());

        Ins::DeclFunc {
            name: fn_name,
            params,
            ret_type,
            body: Box::new(body),
            loc: fn_span,
        }
    }

    fn parse_struct_block(&mut self) -> (Vec<Ins>, Vec<Ins>, Rc<SourceRef>) {
        let mut block_loc = self.cur_token().get_source_ref();
        self.advance();

        let mut fields = vec![];
        let mut funcs = vec![];
        let mut struct_deps = vec![];
        let mut ood = HashMap::new();
        let mut saw_rcurly = false;
        while !self.is_at_eof() {
            let cur = self.cur_token();
            if matches!(cur, Token::RCurly(_)) {
                block_loc = block_loc.combine(cur.get_source_ref());
                saw_rcurly = true;
                break;
            }

            let ins = self.next_ins(true, false);
            block_loc = block_loc.combine(ins.get_source_ref());
            match &ins {
                Ins::DeclConst { .. } | Ins::DeclVar { .. } => {
                    if !self.cur_deps.is_empty() {
                        struct_deps.extend(self.cur_deps.clone());
                        self.cur_deps.clear();
                    }
                    fields.push(ins);
                }
                Ins::DeclFunc { name, .. } => {
                    let ins_id = ins.get_id().unwrap();
                    ood.insert(ins_id, self.cur_deps.clone());
                    self.cur_deps.clear();
                    funcs.push(ins);
                }
                Ins::DeclStruct {
                    name,
                    fields,
                    funcs,
                    loc,
                } => todo!(),
                Ins::ErrorIns { .. } => continue,
                _ => {
                    // report error for unexpected Ins at this level
                    todo!()
                }
            }
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

        // for (ins_s, deps) in ood.iter() {
        //     println!("{ins_s}");
        //     for dep in deps.iter() {
        //         println!("  - {}", dep.to);
        //     }
        //     println!()
        // }

        let mut ood = self.topological_sort(&mut ood);
        let mut ood_funcs = vec![];
        if !ood.is_empty() {
            // for ins in funcs.iter() {
            //     if !ood.contains(&ins.get_id().unwrap()) {}
            // }
            for node in ood.iter() {
                let node = node.as_str();
                let ins = funcs
                    .iter()
                    .find(|ins| ins.get_id().is_some() && ins.get_id().unwrap() == node);
                if let Some(ins) = ins {
                    ood_funcs.push(ins.clone());
                }
            }
        } else {
            ood_funcs = funcs;
        }

        self.cur_deps = struct_deps;

        (fields, ood_funcs, block_loc)
    }

    fn parse_struct(&mut self) -> Ins {
        let start = self.cur_token();
        self.advance();

        let struct_name = self.parse_identifier();

        self.scope.push(ParseScope::Struct);
        let (fields, ood_funcs, struct_span) = self.parse_struct_block();
        self.scope.pop();

        Ins::DeclStruct {
            name: struct_name,
            loc: struct_span,
            fields,
            funcs: ood_funcs,
        }
    }

    fn parse_module(&mut self) -> Ins {
        let start = self.cur_token();
        self.advance();

        let mod_name = self.parse_identifier();

        self.scope.push(ParseScope::Mod);
        let body = self.parse_block(false);
        self.scope.pop();
        let struct_span = start.get_source_ref().combine(body.get_source_ref());

        Ins::DeclModule {
            name: mod_name,
            body: Box::new(body),
            loc: struct_span,
        }
    }

    fn parse_expr_ins(&mut self) -> Ins {
        let lhs = self.parse_expr();

        let mut cur = self.cur_token();
        match (lhs, cur) {
            // Variable / Constant Declaration and Initialization:
            // Variable Declaration and Initialization => Expr::Ident : Type? = Expr?
            // Variable Declaration => Expr::Ident : Type
            // Constant => Expr::Ident : Type? : Expr
            (Expr::Ident { name, loc }, Token::Colon(_)) => {
                // skip the ':'
                self.advance();

                cur = self.cur_token();

                // get type if any is provided
                let decl_ty = if !matches!(cur, Token::Colon(_) | Token::Assign(_)) {
                    Some(self.parse_type())
                } else {
                    None
                };

                // now we have to determine if we are dealing with a constant or variable
                // declaration
                cur = self.cur_token();
                match cur {
                    // Variable Declaration and Initialization
                    Token::Assign(_) => {
                        self.advance();
                        let val = self.parse_expr();
                        let mut decl_var_span = loc.combine(val.get_source_ref());

                        cur = self.cur_token();

                        Ins::DeclVar {
                            name: Expr::Ident { name, loc },
                            ty: decl_ty,
                            init_val: Some(val),
                            loc: decl_var_span,
                        }
                    }
                    // Constant Declaration and Initialization
                    Token::Colon(_) => {
                        self.advance();
                        let val = self.parse_expr();
                        let mut decl_const_span = loc.combine(val.get_source_ref());

                        cur = self.cur_token();

                        Ins::DeclConst {
                            name: Expr::Ident { name, loc },
                            ty: decl_ty,
                            init_val: val,
                            loc: decl_const_span,
                        }
                    }
                    // Potential Variable Declaration without Initialization
                    _ => {
                        // we can guarantee there will be a provided type by this point
                        let decl_ty = decl_ty.unwrap();
                        Ins::DeclVar {
                            loc: loc.combine(decl_ty.get_loc()),
                            name: Expr::Ident { name, loc },
                            ty: Some(decl_ty),
                            init_val: None,
                        }
                    }
                }
            }
            // Assignment Instruction:
            // Expr = Expr;
            (lhs, Token::Assign(_)) => {
                // skip past the '='
                self.advance();
                let value = self.parse_expr();
                let mut assign_span = lhs.get_source_ref().combine(value.get_source_ref());

                Ins::AssignTo {
                    target: lhs,
                    value,
                    loc: assign_span,
                }
            }
            (lhs, Token::PlusAssign(_)) => {
                // skip past the +=
                self.advance();
                let value = self.parse_expr();
                let mut assign_span = lhs.get_source_ref().combine(value.get_source_ref());

                Ins::AssignTo {
                    target: lhs.clone(),
                    value: Expr::BinOp {
                        op: BinOpType::Add,
                        left: Box::new(lhs),
                        right: Box::new(value),
                        loc: assign_span.clone(),
                    },
                    loc: assign_span,
                }
            }
            (lhs, Token::MinusAssign(_)) => {
                // skip past the -=
                self.advance();
                let value = self.parse_expr();
                let mut assign_span = lhs.get_source_ref().combine(value.get_source_ref());

                Ins::AssignTo {
                    target: lhs.clone(),
                    value: Expr::BinOp {
                        op: BinOpType::Sub,
                        left: Box::new(lhs),
                        right: Box::new(value),
                        loc: assign_span.clone(),
                    },
                    loc: assign_span,
                }
            }
            (lhs, Token::StarAssign(_)) => {
                // skip past the *=
                self.advance();
                let value = self.parse_expr();
                let mut assign_span = lhs.get_source_ref().combine(value.get_source_ref());

                Ins::AssignTo {
                    target: lhs.clone(),
                    value: Expr::BinOp {
                        op: BinOpType::Mult,
                        left: Box::new(lhs),
                        right: Box::new(value),
                        loc: assign_span.clone(),
                    },
                    loc: assign_span,
                }
            }
            (lhs, Token::SlashAssign(_)) => {
                // skip past the /=
                self.advance();
                let value = self.parse_expr();
                let mut assign_span = lhs.get_source_ref().combine(value.get_source_ref());

                Ins::AssignTo {
                    target: lhs.clone(),
                    value: Expr::BinOp {
                        op: BinOpType::Div,
                        left: Box::new(lhs),
                        right: Box::new(value),
                        loc: assign_span.clone(),
                    },
                    loc: assign_span,
                }
            }
            (lhs, Token::ModuloAssign(_)) => {
                // skip past the %=
                self.advance();
                let value = self.parse_expr();
                let mut assign_span = lhs.get_source_ref().combine(value.get_source_ref());

                Ins::AssignTo {
                    target: lhs.clone(),
                    value: Expr::BinOp {
                        op: BinOpType::Mod,
                        left: Box::new(lhs),
                        right: Box::new(value),
                        loc: assign_span.clone(),
                    },
                    loc: assign_span,
                }
            }
            (lhs, _) => Ins::ExprIns {
                loc: lhs.get_source_ref(),
                expr: lhs,
            },
        }
    }

    fn parse_block(&mut self, use_new_scope: bool) -> Ins {
        let mut block_loc = self.cur_token().get_source_ref();
        self.advance();

        let mut code = vec![];

        if use_new_scope {
            self.scope.push(ParseScope::Block);
        }
        let mut saw_rcurly = false;
        while !self.is_at_eof() {
            let cur = self.cur_token();
            if matches!(cur, Token::RCurly(_)) {
                block_loc = block_loc.combine(cur.get_source_ref());
                saw_rcurly = true;
                break;
            }

            let ins = self.next_ins(true, false);
            code.push(ins);
        }
        let last_ins = code.last();
        if let Some(last_ins) = last_ins {
            block_loc = block_loc.combine(last_ins.get_source_ref());
        }

        if use_new_scope {
            self.scope.pop();
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

    fn parse_type(&mut self) -> Rc<Ty> {
        let cur = self.cur_token();
        self.advance();
        let ty = match cur {
            Token::I8(loc) => Rc::new(Ty::Signed {
                size: 8,
                is_int: false,
                loc,
            }),
            Token::I16(loc) => Rc::new(Ty::Signed {
                size: 16,
                is_int: false,
                loc,
            }),
            Token::I32(loc) => Rc::new(Ty::Signed {
                size: 32,
                is_int: false,
                loc,
            }),
            Token::I64(loc) => Rc::new(Ty::Signed {
                size: 64,
                is_int: false,
                loc,
            }),
            Token::Int(loc) => {
                // 64 bit platform
                Ty::get_int_ty(loc)
            }
            Token::U8(loc) => Rc::new(Ty::Unsigned {
                size: 8,
                is_uint: false,
                loc,
            }),
            Token::U16(loc) => Rc::new(Ty::Unsigned {
                size: 16,
                is_uint: false,
                loc,
            }),
            Token::U32(loc) => Rc::new(Ty::Unsigned {
                size: 32,
                is_uint: false,
                loc,
            }),
            Token::U64(loc) => Rc::new(Ty::Unsigned {
                size: 64,
                is_uint: false,
                loc,
            }),
            Token::UInt(loc) => {
                // 64 bit platform
                Ty::get_uint_ty(loc)
            }
            Token::Char(loc) => Rc::new(Ty::Char { loc }),
            Token::Bool(loc) => Rc::new(Ty::Bool { loc }),
            Token::Str(loc) => Rc::new(Ty::Str {
                loc,
                is_interp: false,
            }),
            Token::Identifier(name, loc) => Rc::new(Ty::NamedType { name, loc }),
            Token::Void(loc) => Rc::new(Ty::Void { loc }),
            Token::LBracket(loc) => {
                let sub_ty = self.parse_type();

                match self.cur_token() {
                    Token::Comma(_) => {
                        self.advance();
                        // check for an underscore or parse an expr for the size of the
                        // array.
                        let static_size = match self.cur_token() {
                            Token::Underscore(_) => {
                                self.advance();
                                None
                            }
                            _ => Some(self.parse_expr()),
                        };
                        let mut span = loc;

                        if !matches!(self.cur_token(), Token::RBracket(_)) {
                            self.report_error(ParseError::Expected(
                                "a right bracket (]) to terminate the static array type."
                                    .to_string(),
                                self.cur_token().get_source_ref(),
                                None,
                            ));
                        } else {
                            span = span.combine(self.cur_token().get_source_ref());
                            self.advance()
                        }
                        Rc::new(Ty::StaticArray {
                            sub_ty,
                            size: static_size,
                            loc: span,
                        })
                    }
                    Token::RBracket(rbrac_loc) => {
                        self.advance();
                        let span = loc.combine(rbrac_loc);
                        Rc::new(Ty::Slice { sub_ty, loc: span })
                    }
                    _ => {
                        self.report_error(ParseError::Expected(
                            "a right bracket (]) to terminate the slice type or a comma to separate the type and size of the static array type.".to_string(),
                            self.cur_token().get_source_ref(),
                            None,
                        ));
                        Rc::new(Ty::ErrorType {
                            loc: self.cur_token().get_source_ref(),
                        })
                    }
                }
            }
            Token::QuestionMark(loc) => {
                let sub_ty = self.parse_type();
                let span = loc.combine(sub_ty.get_loc().clone());
                Rc::new(Ty::Optional { sub_ty, loc: span })
            }
            Token::BackSlash(loc) => {
                // we will need to parse comma-separated types delimited by parenthesis
                // and a return type to make the signature
                let mut loc = loc;

                let mut cur = self.cur_token();

                if !matches!(cur, Token::LParen(_)) {
                    self.report_error(ParseError::Expected(
                        "a left parenthesis to begin the list of parameter types.".to_string(),
                        cur.get_source_ref(),
                        None,
                    ));
                } else {
                    self.advance();
                }

                let mut param_tys = vec![];
                let mut saw_rparen = false;
                while !self.is_at_eof() {
                    cur = self.cur_token();
                    if matches!(cur, Token::RParen(_)) {
                        saw_rparen = true;
                        break;
                    }

                    let param_ty = self.parse_type();

                    cur = self.cur_token();

                    if matches!(cur, Token::RParen(_)) {
                        saw_rparen = true;
                    } else if !matches!(cur, Token::Comma(_)) {
                        self.report_error(ParseError::Expected(
                            "a comma to separate parameter types or a right parenthesis to terminate the list of parameter types.".to_string(),
                            cur.get_source_ref(),
                            None,
                        ))
                    } else {
                        self.advance();
                    }

                    param_tys.push(param_ty);

                    if saw_rparen {
                        break;
                    }
                }

                if !saw_rparen {
                    self.report_error(ParseError::Expected(
                        "a right parenthesis to terminate the list of parameters.".to_string(),
                        self.cur_token().get_source_ref(),
                        None,
                    ));
                } else {
                    self.advance();
                }

                // parse expected return type
                let ret_type = self.parse_type();
                Rc::new(Ty::Func {
                    params: param_tys,
                    loc: loc.combine(ret_type.get_loc()),
                    ret: ret_type,
                })
            }
            _ => {
                // TODO: is it wise to advance the cursor here?
                self.report_error(ParseError::CannotParseAType(cur.get_source_ref()));
                Rc::new(Ty::ErrorType {
                    loc: cur.get_source_ref(),
                })
            }
        };
        ty
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_lambda_expression()
    }

    fn parse_lambda_expression(&mut self) -> Expr {
        let maybe_bckslsh = self.cur_token();
        match maybe_bckslsh {
            Token::BackSlash(loc) => {
                // lambda expressions are written in the form:
                // \([PARAMS]) ret_type {}
                self.advance();

                let params = self.parse_fn_params();

                let ret_type = self.parse_type();
                self.scope.push(ParseScope::Function);
                let body = self.next_ins(false, false);
                self.scope.pop();
                let lambda_span = loc.combine(body.get_source_ref());

                Expr::Lambda {
                    params,
                    ret_type,
                    body: Box::new(body),
                    loc: lambda_span,
                }
            }
            _ => self.parse_optional_expr(),
        }
    }

    fn parse_optional_expr(&mut self) -> Expr {
        let op = self.cur_token();
        match op {
            Token::Some(loc) => {
                self.advance();
                let val = self.parse_ternary();
                Expr::OptionalExpr {
                    loc: loc.combine(val.get_source_ref()),
                    val: Some(Box::new(val)),
                }
            }
            Token::None(loc) => {
                self.advance();
                Expr::OptionalExpr { val: None, loc }
            }
            _ => self.parse_ternary(),
        }
    }

    fn parse_ternary(&mut self) -> Expr {
        let mut cond = self.parse_or();

        // look for ?
        if matches!(self.cur_token(), Token::QuestionMark(_)) {
            self.advance();
            let then = self.parse_expr();

            // look for :
            if !matches!(self.cur_token(), Token::Colon(_)) {
                self.report_error(ParseError::Expected(
                    "a colon to separate both branches of the ternary condition expression."
                        .to_string(),
                    self.cur_token().get_source_ref(),
                    None,
                ));
            } else {
                self.advance();
            }

            let otherwise = self.parse_expr();

            cond = Expr::TernaryConditional {
                loc: cond.get_source_ref().combine(otherwise.get_source_ref()),
                cond: Box::new(cond),
                then: Box::new(then),
                otherwise: Box::new(otherwise),
            };
        }

        cond
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
        let mut lhs = self.parse_primary();
        while matches!(
            self.cur_token(),
            Token::LParen(_) | Token::LBracket(_) | Token::Dot(_)
        ) {
            let op = self.cur_token();
            self.advance();
            match op {
                // Function Calls
                Token::LParen(_) => {
                    // read arguments for the call
                    let mut args = vec![];
                    let mut saw_rparen = false;
                    while !self.is_at_eof() {
                        let mut cur = self.cur_token();

                        // if we are not already at the end of the argument list,
                        // parse an expression
                        if !matches!(cur, Token::RParen(_)) {
                            let arg = self.parse_expr();
                            args.push(arg);
                            cur = self.cur_token();
                        }

                        // check to see if we have reached the end of the argument
                        // list
                        if matches!(cur, Token::RParen(_)) {
                            saw_rparen = true;
                            break;
                        }

                        // arguments are to be comma separated
                        if !matches!(cur, Token::Comma(_)) {
                            self.report_error(ParseError::Expected("a comma to separate arguments or a right parenthesis to terminate function call.".to_string(), cur.get_source_ref(), None));
                            break;
                        }
                        self.advance();
                    }
                    let mut call_span = lhs.get_source_ref();
                    if saw_rparen {
                        call_span = call_span.combine(self.cur_token().get_source_ref());
                        self.advance();
                    } else {
                        self.report_error(ParseError::Expected(
                            "a right parenthesis to terminate function call.".to_string(),
                            self.cur_token().get_source_ref(),
                            None,
                        ));
                    }

                    if let Expr::Ident { name, loc } = &lhs {
                        self.cur_deps.push(Dep {
                            ty: DepTy::Func,
                            to: name.clone(),
                            loc: loc.clone(),
                        });
                    }

                    if let Expr::AccessMember { mem, loc, .. } = &lhs {
                        self.cur_deps.push(Dep {
                            ty: DepTy::Func,
                            to: mem.as_str(),
                            loc: loc.clone(),
                        });
                    }

                    lhs = Expr::CallFn {
                        func: Box::new(lhs),
                        args,
                        loc: call_span,
                    };
                }
                // Index Array or Make Slice
                Token::LBracket(_) => {
                    let mut span = lhs.get_source_ref();
                    if matches!(self.cur_token(), Token::Colon(_)) {
                        self.advance();
                        let end_excl = match self.cur_token() {
                            Token::RBracket(_) => None,
                            _ => Some(Box::new(self.parse_expr())),
                        };

                        if !matches!(self.cur_token(), Token::RBracket(_)) {
                            self.report_error(ParseError::Expected(
                                "a right bracket (]) to terminate the slice expression."
                                    .to_string(),
                                self.cur_token().get_source_ref(),
                                None,
                            ));
                        } else {
                            span = span.combine(self.cur_token().get_source_ref());
                            self.advance()
                        }
                        lhs = Expr::MakeSlice {
                            target: Box::new(lhs),
                            start: None,
                            end_excl,
                            loc: span,
                        };
                        continue;
                    }
                    let index_expr = self.parse_expr();
                    span = span.combine(index_expr.get_source_ref());

                    match self.cur_token() {
                        Token::RBracket(_) => {
                            span = span.combine(self.cur_token().get_source_ref());
                            self.advance();
                            lhs = Expr::IndexInto {
                                target: Box::new(lhs),
                                index: Box::new(index_expr),
                                loc: span,
                            };
                        }
                        Token::Colon(_) => {
                            self.advance();
                            let end_excl = match self.cur_token() {
                                Token::RBracket(_) => None,
                                _ => Some(Box::new(self.parse_expr())),
                            };

                            if !matches!(self.cur_token(), Token::RBracket(_)) {
                                self.report_error(ParseError::Expected(
                                    "a right bracket (]) to terminate the slice expression."
                                        .to_string(),
                                    self.cur_token().get_source_ref(),
                                    None,
                                ));
                            } else {
                                span = span.combine(self.cur_token().get_source_ref());
                                self.advance()
                            }
                            lhs = Expr::MakeSlice {
                                target: Box::new(lhs),
                                start: Some(Box::new(index_expr)),
                                end_excl,
                                loc: span,
                            }
                        }
                        _ => self.report_error(ParseError::Expected(
                            concat!(
                                "a right bracket to terminate index expression or a colon to ",
                                "separate start and exclusive end of the slice expression."
                            )
                            .to_string(),
                            self.cur_token().get_source_ref(),
                            None,
                        )),
                    }
                }
                // Struct Member Access or Initialization
                // struct.field or StructName.(key: value, key2, key3: value)
                Token::Dot(_) => {
                    let mut cur = self.cur_token();
                    match cur {
                        // Struct Member Access
                        _ => {
                            let member = self.parse_identifier();
                            let mem_acc_span =
                                lhs.get_source_ref().combine(member.get_source_ref());
                            lhs = Expr::AccessMember {
                                target: Box::new(lhs),
                                mem: Box::new(member),
                                loc: mem_acc_span,
                            };
                        }
                    }
                }
                _ => unreachable!("Parser::parse_index_expr: unexpected op: {op:?}"),
            }
        }

        lhs
    }

    fn parse_identifier(&mut self) -> Expr {
        let cur = self.cur_token();
        match cur {
            Token::Identifier(name, loc) => {
                self.advance();
                Expr::Ident { name, loc }
            }
            _ => {
                self.report_error(ParseError::Expected(
                    "an identifier.".to_string(),
                    cur.get_source_ref(),
                    None,
                ));
                Expr::ErrorExpr {
                    loc: cur.get_source_ref(),
                }
            }
        }
    }

    fn parse_primary(&mut self) -> Expr {
        let cur = self.cur_token();

        match cur {
            Token::CharLiteral(loc, chr) => {
                self.advance();
                Expr::Char { val: chr, loc }
            }
            Token::Identifier(..) => self.parse_identifier(),
            Token::NumberLiteral(num, src) => {
                self.advance();
                Expr::Number { val: num, loc: src }
            }
            Token::SingleLineStringLiteral(loc, str) => {
                self.advance();
                Expr::Str { val: str, loc }
            }
            Token::MultiLineStringFragment(..) => {
                // TODO
                unreachable!("Parse::parse_primary: proto should not have multiline string support just yet.")
            }
            Token::True(loc) => {
                self.advance();
                Expr::Bool { val: true, loc }
            }
            Token::False(loc) => {
                self.advance();
                Expr::Bool { val: false, loc }
            }
            Token::LParen(_) => {
                self.advance();
                let inner_expr = self.parse_expr();
                let mut i_expr_span = cur.get_source_ref().combine(inner_expr.get_source_ref());
                if !matches!(self.cur_token(), Token::RParen(_)) {
                    self.report_error(ParseError::Expected(
                        "a right parenthesis to terminate grouped expression.".to_string(),
                        self.cur_token().get_source_ref(),
                        None,
                    ));
                } else {
                    i_expr_span = self.cur_token().get_source_ref().combine(i_expr_span);
                    self.advance();
                }
                Expr::GroupedExpr {
                    inner: Box::new(inner_expr),
                    loc: i_expr_span,
                }
            }
            Token::LBracket(loc) => {
                self.advance();

                let mut items = vec![];
                let mut saw_rbracket = false;
                while !self.is_at_eof() {
                    let mut cur = self.cur_token();
                    // if we are not already at the end of the argument list,
                    // parse an expression
                    if !matches!(cur, Token::RBracket(_)) {
                        let item = self.parse_expr();
                        items.push(item);
                        cur = self.cur_token();
                    }

                    // check to see if we have reached the end of the argument
                    // list
                    if matches!(cur, Token::RBracket(_)) {
                        saw_rbracket = true;
                        break;
                    }

                    // arguments are to be comma separated
                    if !matches!(cur, Token::Comma(_)) {
                        self.report_error(ParseError::Expected("a comma to separate array items or a right bracket (]) to terminate the static array.".to_string(), cur.get_source_ref(), None));
                        break;
                    }
                    self.advance();
                }

                let mut span = loc;
                if saw_rbracket {
                    span = span.combine(self.cur_token().get_source_ref());
                    self.advance();
                } else {
                    self.report_error(ParseError::Expected(
                        "a right bracket (]) to terminate the static array.".to_string(),
                        self.cur_token().get_source_ref(),
                        None,
                    ));
                }

                Expr::StaticArray {
                    vals: items,
                    loc: span,
                }
            }
            Token::BackTick(loc) => {
                // an interporlated string start and end with a backtick, can contain text
                // or expression sections starting with { and ending with }
                let mut span = loc.clone();
                self.advance();

                let mut parts = vec![];
                while !self.is_at_eof() {
                    match self.cur_token() {
                        Token::BackTick(end_loc) => {
                            span = span.combine(end_loc);
                            self.advance();
                            break;
                        }
                        Token::InterpStrLiteral(str_loc, content) => {
                            self.advance();
                            let str = Expr::InterpStr { val: content, loc: str_loc };
                            parts.push(str);
                        }
                        Token::LCurly(_) => {
                            self.advance();
                            let sub_expr = self.parse_expr();
                            if !matches!(self.cur_token(), Token::RCurly(_)) {
                                self.report_error(ParseError::Expected(
                                    "a } to terminate expression section in interpolated string.".to_string(),
                                    self.cur_token().get_source_ref(),
                                    None
                                ))
                            } else {
                                self.advance();
                            }
                            parts.push(sub_expr);
                        }
                        _ => unreachable!("pparserr::parse_primary(): for interpolated strings, unexpected token: {:#?}", self.cur_token())
                    }
                }
                Expr::InterpolatedString { parts, loc: span }
            }
            _ => {
                let span = cur.get_source_ref();
                self.report_error(ParseError::CannotParseAnExpression(span.clone()));
                Expr::ErrorExpr { loc: span }
            }
        }
    }
}

#[cfg(test)]
#[derive(serde::Serialize, serde::Deserialize)]
pub struct ParserTestResult {
    code_str_repr: Vec<String>,
    lexer_errors: Vec<String>,
    parser_errors: Vec<String>,
    parse_warning: Vec<String>,
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
        let parse_warning = parser
            .parse_warns
            .iter()
            .map(|w| format!("{:?}", w))
            .collect::<Vec<String>>();

        let result = ParserTestResult {
            code_str_repr,
            lexer_errors,
            parser_errors,
            parse_warning,
        };

        insta::assert_yaml_snapshot!(result);
    });
}
