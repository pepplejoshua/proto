use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::{Expression, ProtoBinaryOp, ProtoNode, ProtoProgram, ProtoStruct, Statement, Var},
    common::{LexContext, ProtoErr},
    lexer::{lex_next, Span, Token, TokenKind},
};

pub struct ParseContext {
    pub current: Rc<Token>,
}

impl ParseContext {
    pub fn new(ctx: &mut LexContext) -> Result<Self, ProtoErr> {
        let token = lex_next(ctx)?;
        Ok(ParseContext {
            current: Rc::new(token),
        })
    }
}

fn consume(
    lex_ctx: &mut LexContext,
    ctx: &mut ParseContext,
    kind: TokenKind,
) -> Result<(), ProtoErr> {
    if ctx.current.kind == kind {
        ctx.current = Rc::new(lex_next(lex_ctx)?);
        Ok(())
    } else {
        Err(ProtoErr::General(
            format!(
                "Expected token {:?} but received {:?} :: [{}:{}]",
                kind, ctx.current.kind, ctx.current.line, ctx.current.column,
            ),
            Some(ctx.current.clone()),
        ))
    }
}

fn consume_id(lex_ctx: &mut LexContext, ctx: &mut ParseContext) -> Result<(), ProtoErr> {
    if matches!(ctx.current.kind, TokenKind::Identifier(_)) {
        ctx.current = Rc::new(lex_next(lex_ctx)?);
        Ok(())
    } else {
        Err(ProtoErr::General(
            format!(
                "Expected token {:?} but received {:?} :: [{}:{}]",
                TokenKind::Identifier("id".into()),
                ctx.current.kind,
                ctx.current.line,
                ctx.current.column,
            ),
            Some(ctx.current.clone()),
        ))
    }
}

// handles literals
fn parse_factor(lex_ctx: &mut LexContext, ctx: &mut ParseContext) -> Result<Expression, ProtoErr> {
    match ctx.current.kind {
        TokenKind::Integer(_)
        | TokenKind::Char(_)
        | TokenKind::Boolean(_)
        | TokenKind::String(_) => {
            let current = ctx.current.clone();
            consume(lex_ctx, ctx, ctx.current.kind.clone())?;
            Ok(Expression::Literal(current))
        }
        _ => {
            let span = ctx.current.span;
            let lexeme = &lex_ctx.source[span.start..span.end];
            Err(ProtoErr::General(
                format!("Unknown token in expression '{}'", lexeme),
                Some(ctx.current.clone()),
            ))
        }
    }
}

// handles highest level of precedence
// * / %
fn parse_term(lex_ctx: &mut LexContext, ctx: &mut ParseContext) -> Result<Expression, ProtoErr> {
    let mut factor = parse_factor(lex_ctx, ctx)?;

    while ctx.current.kind == TokenKind::Star
        || ctx.current.kind == TokenKind::Slash
        || ctx.current.kind == TokenKind::Modulo
    {
        let operator = ctx.current.clone();
        consume(lex_ctx, ctx, operator.kind.clone())?;
        factor = Expression::BinaryOp(ProtoBinaryOp {
            left: Rc::new(factor),
            right: Rc::new(parse_factor(lex_ctx, ctx)?),
            operator: operator.clone(),
        });
    }

    Ok(factor)
}

// handles second level of prcedence
// + -
pub fn parse_expr(lex_ctx: &mut LexContext, ctx: &mut ParseContext) -> Result<ProtoNode, ProtoErr> {
    let mut term = parse_term(lex_ctx, ctx)?;

    while ctx.current.kind == TokenKind::Plus || ctx.current.kind == TokenKind::Minus {
        let operator = ctx.current.clone();
        consume(lex_ctx, ctx, operator.kind.clone())?;
        term = Expression::BinaryOp(ProtoBinaryOp {
            left: Rc::new(term),
            right: Rc::new(parse_term(lex_ctx, ctx)?),
            operator: operator.clone(),
        });
    }

    if ctx.current.kind == TokenKind::SemiColon {
        Ok(ProtoNode::ProtoStatement(Statement::ExpressionStatement(
            term,
        )))
    } else {
        Ok(ProtoNode::ProtoExpr(term))
    }
}

pub fn parse_struct(
    lex_ctx: &mut LexContext,
    ctx: &mut ParseContext,
) -> Result<ProtoNode, ProtoErr> {
    consume(lex_ctx, ctx, TokenKind::Struct)?;

    let identifier = ctx.current.clone();
    consume_id(lex_ctx, ctx)?;

    consume(lex_ctx, ctx, TokenKind::OpenCurly)?;

    // TODO: Parse record body
    let mut members: HashMap<Span, Var> = HashMap::new();

    // TODO: Add mut and add mutability to members?
    while ctx.current.kind == TokenKind::Let {
        consume(lex_ctx, ctx, TokenKind::Let)?;

        let member_id = ctx.current.clone();
        consume_id(lex_ctx, ctx)?;

        consume(lex_ctx, ctx, TokenKind::Comma)?;

        members.insert(
            member_id.span,
            Var {
                identifier: member_id.clone(),
            },
        );
    }

    consume(lex_ctx, ctx, TokenKind::CloseCurly)?;

    Ok(ProtoNode::ProtoStatement(Statement::Struct(ProtoStruct {
        identifier,
        members,
    })))
}

// TODO
// handles the forms (with optional semicolons):
// let identifier: type = expr;
// mut identifier: type = expr;
// let identifier: type;
// mut identifier: type;

// fn parse_var_declaration(
//     lex_ctx: &mut LexContext,
//     ctx: &mut ParseContext,
// ) -> Result<ProtoNode, ProtoErr> {
//     // Ok(())
// }

fn top_level(lex_ctx: &mut LexContext, ctx: &mut ParseContext) -> Result<ProtoProgram, ProtoErr> {
    let mut code = ProtoProgram { program: vec![] };
    // top level should be dealing with ProtoNode creation which gets
    // added to a ProtoProgram's vector
    while ctx.current.kind != TokenKind::End {
        let line = match ctx.current.kind {
            TokenKind::Struct => parse_struct(lex_ctx, ctx)?, // will fix this later
            // TokenKind::Let | TokenKind::Mut => parse_var_declaration(lex_ctx, ctx)?,
            _ => parse_expr(lex_ctx, ctx)?,
        };
        code.program.push(line);
    }

    Ok(code)
}

pub fn parse(source: String) -> Result<ProtoProgram, ProtoErr> {
    let mut lex_ctx = LexContext::new(source);
    let mut ctx = ParseContext::new(&mut lex_ctx)?;

    // Bubble error to main (for now)
    let program = top_level(&mut lex_ctx, &mut ctx)?;
    Ok(program)
}
