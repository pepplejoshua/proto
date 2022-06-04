use crate::common::{ProtoErr, LexContext};

#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum TokenKind {
	// Complex
	Identifier(String),
	String(String),
	Integer(i32),
	Float(f32),
	Boolean(bool),
	
	// Singles
	Var, Record,
	Equals, Plus, Minus, Star, Slash,
	Dot, Comma, SemiColon,
	OpenParen, CloseParen,
	OpenCurly, CloseCurly,

	End,
}

// Instead of cloning/copying the source to create
// a heaped string, we just keep track of where it is
// within the string instead (reduces overall allocations)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
	pub start: usize,
	pub end: usize,
}

#[derive(Debug, Clone)]
pub struct Token {
	pub span: Span,
	pub kind: TokenKind,
	pub column: usize,
	pub line: usize,
}

impl Token {
	pub fn new(span: Span, kind: TokenKind, column: usize, line: usize) -> Self {
		Self { span, kind, column, line }
	}
}

fn advance(ctx: &mut LexContext) {
	ctx.current_ip += 1;
	ctx.column += 1;
}

fn advance_line(ctx: &mut LexContext) {
	ctx.current_ip += 1;
	ctx.line += 1;
	ctx.column = 1;
}

fn skip_whitespace(ctx: &mut LexContext) {
	'outer: while ctx.current_ip < ctx.source.len() {
		match ctx.source.as_bytes()[ctx.current_ip] {
			b' ' | b'\t' | b'\r' => advance(ctx),
			b'\n' => advance_line(ctx),

			// Comments
			b'#' => {
				while ctx.current_ip < ctx.source.len() && ctx.source.as_bytes()[ctx.current_ip] != b'\n' {
					advance(ctx);
				}
			}
			
			_ => break 'outer,
		}
	}
}

fn find_keyword(ctx: &LexContext, span: Span) -> Option<TokenKind> {
	let lexeme = &ctx.source[span.start..span.end];

	match lexeme {
		"record" => Some(TokenKind::Record),
		"var" => Some(TokenKind::Var),

		"true" | "false" => Some(TokenKind::Boolean(lexeme.parse().unwrap())),
		_ => None,
	}
}

fn find_single(ctx: &LexContext) -> Option<TokenKind> {
	let single = ctx.source.as_bytes()[ctx.current_ip];

	match single {
		b'+' => Some(TokenKind::Plus),
		b'-' => Some(TokenKind::Minus),
		b'*' => Some(TokenKind::Star),
		b'/' => Some(TokenKind::Slash),
		b'=' => Some(TokenKind::Equals),
		
		b'.' => Some(TokenKind::Dot),
		b',' => Some(TokenKind::Comma),
		b';' => Some(TokenKind::SemiColon),
		
		b'{' => Some(TokenKind::OpenCurly),
		b'}' => Some(TokenKind::CloseCurly),
		b'(' => Some(TokenKind::OpenParen),
		b')' => Some(TokenKind::CloseParen),
		_ => None,
	}
}

fn is_identifier(current: char) -> bool {
	return char::is_alphabetic(current) || current == '_';
}

fn make_token(ctx: &LexContext, span: Span, kind: TokenKind, column: usize) -> Token {
	Token::new(span, kind, column, ctx.line)
}

fn identifier(ctx: &mut LexContext) -> Token {
	let start = ctx.current_ip;
	let start_col = ctx.column;

	while ctx.current_ip < ctx.source.len() && is_identifier(ctx.source.as_bytes()[ctx.current_ip] as char) {
		advance(ctx);
	}

	let span = Span { start, end: ctx.current_ip };
	let keyword = find_keyword(ctx, span);

	make_token(
		ctx, span,
		if let Some(word) = keyword { word } else { TokenKind::Identifier(ctx.source[start..ctx.current_ip].into()) },
		start_col
	)
}

fn number(ctx: &mut LexContext) -> Result<Token, ProtoErr> {
	let start = ctx.current_ip;
	let start_col = ctx.column;
	let mut is_float = false;

	while ctx.current_ip < ctx.source.len() && char::is_digit(ctx.source.as_bytes()[ctx.current_ip] as char, 10) {
		advance(ctx);

		if ctx.source.as_bytes()[ctx.current_ip] == b'.' {
			if is_float {
				return Err(ProtoErr { msg: String::from("Multiple decimals found in floating point"), token: None });
			}

			is_float = true;
		}
	}

	let lexeme = &ctx.source[start..ctx.current_ip];

	Ok(make_token(
		ctx, Span { start, end: ctx.current_ip },
		if is_float { TokenKind::Float(lexeme.parse().unwrap()) } else { TokenKind::Integer(lexeme.parse().unwrap()) },
		start_col
	))
}

fn string(ctx: &mut LexContext) -> Result<Token, ProtoErr> {
	advance(ctx);

	let start = ctx.current_ip;
	let start_col = ctx.column;

	while ctx.current_ip < ctx.source.len() && ctx.source.as_bytes()[ctx.current_ip] != b'"' {
		advance(ctx);
	}

	// Unterminated string
	if ctx.current_ip >= ctx.source.len() {
		return Err(ProtoErr { msg: String::from("Unterminated string"), token: None });
	}
	
	advance(ctx);

	Ok(make_token(
		ctx, Span { start, end: ctx.current_ip },
		TokenKind::String(ctx.source[start..ctx.current_ip].into()),
		start_col
	))
}

pub fn lex_next(ctx: &mut LexContext) -> Result<Token, ProtoErr> {
	skip_whitespace(ctx);

	// End of the stream
	if ctx.current_ip >= ctx.source.len() {
		return Ok(make_token(
			ctx,
			Span { start: ctx.current_ip, end: ctx.current_ip }, 
			TokenKind::End,
			ctx.column,
		));
	}

	let current = ctx.source.as_bytes()[ctx.current_ip] as char;

	if is_identifier(current) {
		return Ok(identifier(ctx));
	}

	if char::is_digit(current, 10) {
		return number(ctx);
	}

	if current == '"' {
		return string(ctx);
	}

	if let Some(single) = find_single(ctx) {
		advance(ctx);

		return Ok(make_token(
			ctx,
			Span { start: ctx.current_ip-1, end: ctx.current_ip }, 
			single,
			ctx.column-1,
		));
	}

	// Unknown item found
	Err(ProtoErr { msg: format!("Unknown character found: '{}'", ctx.source.as_bytes()[ctx.current_ip]), token: None})
}