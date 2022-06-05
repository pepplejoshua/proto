use std::fs;
use proto::{common::{ProtoErr, LexContext}, lexer::{TokenKind, lex_next}};

#[test]
fn test_lexer_tokens() -> Result<(), ProtoErr> {
	let mut ctx = LexContext::new(fs::read_to_string("./examples/proto_tokens.pr").unwrap());

    loop {
		let next = lex_next(&mut ctx)?;

		if next.kind == TokenKind::End {
			break;
		}
    }

	Ok(())
}