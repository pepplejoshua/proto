use std::fs;
use proto::{common::{ProtoErr, LexContext}, lexer::{TokenKind, lex_next}};

#[test]
fn test_lexer_tokens() -> Result<(), ProtoErr> {
	let mut ctx = LexContext::new(fs::read_to_string("./examples/proto_tokens.pr").unwrap());

    loop {
		if lex_next(&mut ctx)?.kind == TokenKind::End {
			break;
		}
    }

	Ok(())
}