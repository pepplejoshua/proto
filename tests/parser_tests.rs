use proto::{
    common::{LexContext, ProtoErr},
    lexer::{lex_next, TokenKind, Token},
	parser::{ParseContext, parse_struct}, ast::AstNode,
};
use std::fs;

#[test]
fn test_parser_struct_id_and_members() -> Result<(), ProtoErr> {
	let path = "./samples/test_sources/parser/valid/struct_id_and_members.pr";
	let src = fs::read_to_string(path).unwrap();
    
	let mut lexer_ctx = LexContext::new(src.clone());
	let mut parser_ctx = ParseContext { current: lex_next(&mut lexer_ctx).unwrap() };

	if let AstNode::Struct(rec) = parse_struct(&mut lexer_ctx, &mut parser_ctx)? {
		let id_span = rec.identifier.span;
		let identifier: &str = src[id_span.start..id_span.end].as_ref();
		assert_eq!(identifier, "Person");

		let expected_members = [
			"name", "age",
		];

		assert_eq!(expected_members.len(), rec.members.len());

		for (span, _) in rec.members.iter() {
			let identifier = &src[span.start..span.end];
			// Note: We must use contains, as a hashmap converting to an iter is
			// 		 not 100% reliable with the order, so we can't directly compare.
			assert!(expected_members.contains(&identifier));
		}
	} else {
		// For some reason record returns a new AST, or is updated and the test was not
		return Err(ProtoErr::General("Unknown AST node returned from record".into(), None));
	}

	Ok(())
}