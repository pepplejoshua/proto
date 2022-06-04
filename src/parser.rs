use std::collections::HashMap;

use crate::{common::{ProtoErr, LexContext, Environment}, lexer::{Token, Span, lex_next, TokenKind}, ast::{Record, Var}};

pub struct ParseContext {
	current: Token,
}

fn consume(lex_ctx: &mut LexContext, ctx: &mut ParseContext, kind: TokenKind) -> Result<(), ProtoErr> {
	if ctx.current.kind == kind {
		ctx.current = lex_next(lex_ctx)?;
		Ok(())
	} else {
		Err(ProtoErr::General( 
			format!("Expected token {:?} but received {:?} :: [{}:{}]",
				kind, ctx.current.kind,
				ctx.current.line, ctx.current.column,
			), 
			Some(ctx.current.clone()) 
		))
	}
}

fn consume_id(lex_ctx: &mut LexContext, ctx: &mut ParseContext) -> Result<(), ProtoErr> {
	if matches!(ctx.current.kind, TokenKind::Identifier(_)) {
		ctx.current = lex_next(lex_ctx)?;
		Ok(())
	} else {
		Err(ProtoErr { msg: 
			format!("Expected token {:?} but received {:?} :: [{}:{}]",
				TokenKind::Identifier("id".into()), ctx.current.kind,
				ctx.current.line, ctx.current.column,
			), 
			token: Some(ctx.current.clone()) 
		})
	}
}

fn record(lex_ctx: &mut LexContext, ctx: &mut ParseContext) -> Result<(), ProtoErr> {
	consume(lex_ctx, ctx, TokenKind::Record)?;

	let id = ctx.current.clone();
	consume_id(lex_ctx, ctx)?;

	consume(lex_ctx, ctx, TokenKind::OpenCurly)?;
	
	// TODO: Parse record body
	let mut members: HashMap<Span, Var> = HashMap::new();
	while ctx.current.kind == TokenKind::Var {
		consume(lex_ctx, ctx, TokenKind::Var)?;
		
		let member_id = ctx.current.clone();
		consume_id(lex_ctx, ctx)?;
		
		consume(lex_ctx, ctx, TokenKind::SemiColon)?;
		
		members.insert(
			member_id.span,
			Var { identifier: member_id },			
		);
	}

	consume(lex_ctx, ctx, TokenKind::CloseCurly)?;

	Ok(())
}

fn top_level(lex_ctx: &mut LexContext, ctx: &mut ParseContext) -> Result<(), ProtoErr> {
	while ctx.current.kind != TokenKind::End {
		match ctx.current.kind {
			TokenKind::Record => return record(lex_ctx, ctx),
			_ => return Err(ProtoErr::General(format!("Unknown statement in top-level: {:?}", ctx.current.kind), Some(ctx.current.clone()))),
		}
	}

	Ok(())
}

pub fn parse(source: String) -> Result<ParseContext, ProtoErr> {
	let mut lex_ctx = LexContext::new(source);
	let mut ctx = ParseContext { current: lex_next(&mut lex_ctx).unwrap(), env: Environment::new() };

	// Bubble error to main (for now)
	top_level(&mut lex_ctx, &mut ctx)?;

	Ok(ctx)
}