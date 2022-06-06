use std::collections::HashMap;

use crate::{
	common::{ProtoErr, LexContext},
	lexer::{Token, Span, lex_next, TokenKind},
	ast::{AstNode, Record, Var, BinOp}
};

pub struct ParseContext {
	pub current: Token,
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
		Err(ProtoErr::General( 
			format!("Expected token {:?} but received {:?} :: [{}:{}]",
				TokenKind::Identifier("id".into()), ctx.current.kind,
				ctx.current.line, ctx.current.column,
			), 
			Some(ctx.current.clone()) 
		))
	}
}

fn parse_factor(lex_ctx: &mut LexContext, ctx: &mut ParseContext) -> Result<AstNode, ProtoErr> {
	match ctx.current.kind {
		TokenKind::Integer(_) | TokenKind::Char(_) | TokenKind::Boolean(_) => {
			let current = ctx.current.clone();
			consume(lex_ctx, ctx, ctx.current.kind.clone())?;
			Ok(AstNode::Literal(current))
		}
		_ => {
			let span = ctx.current.span;
			let lexeme = &lex_ctx.source[span.start..span.end];
			Err(ProtoErr::General(format!("Unknown token in expression '{}'", lexeme), Some(ctx.current.clone())))
		}
	}
}

fn parse_term(lex_ctx: &mut LexContext, ctx: &mut ParseContext) -> Result<AstNode, ProtoErr> {
	let mut factor = parse_factor(lex_ctx, ctx)?;

	while ctx.current.kind == TokenKind::Star || ctx.current.kind == TokenKind::Slash {
		let operator = ctx.current.clone();
		consume(lex_ctx, ctx, operator.kind.clone())?;
		factor = AstNode::BinaryOp(BinOp { left: Box::new(factor), right: Box::new(parse_factor(lex_ctx, ctx)?), operator });
	}

	Ok(factor)
}

pub fn parse_expr(lex_ctx: &mut LexContext, ctx: &mut ParseContext) -> Result<AstNode, ProtoErr> {
	let mut term = parse_term(lex_ctx, ctx)?;

	while ctx.current.kind == TokenKind::Plus || ctx.current.kind == TokenKind::Minus {
		let operator = ctx.current.clone();
		consume(lex_ctx, ctx, operator.kind.clone())?;
		term = AstNode::BinaryOp(BinOp { left: Box::new(term), right: Box::new(parse_term(lex_ctx, ctx)?), operator });
	}

	Ok(term)
}

// FIXME: Rename record to struct
pub fn parse_struct(lex_ctx: &mut LexContext, ctx: &mut ParseContext) -> Result<AstNode, ProtoErr> {
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
			Var { identifier: member_id },			
		);
	}

	consume(lex_ctx, ctx, TokenKind::CloseCurly)?;

	Ok(AstNode::Struct(Record { identifier, members }))
}

fn top_level(lex_ctx: &mut LexContext, ctx: &mut ParseContext) -> Result<(), ProtoErr> {
	while ctx.current.kind != TokenKind::End {
		// FIXME: Handle AstNode and insert it into a block or some context
		let _ = match ctx.current.kind {
			TokenKind::Struct => parse_struct(lex_ctx, ctx)?,
			_ => return Err(ProtoErr::General(format!("Unknown statement in top-level: {:?}", ctx.current.kind), Some(ctx.current.clone()))),
		};
	}

	Ok(())
}

pub fn parse(source: String) -> Result<(), ProtoErr> {
	let mut lex_ctx = LexContext::new(source);
	let mut ctx = ParseContext { current: lex_next(&mut lex_ctx).unwrap() };

	// Bubble error to main (for now)
	top_level(&mut lex_ctx, &mut ctx)?;

	Ok(())
}