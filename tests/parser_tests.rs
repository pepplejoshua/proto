use proto::{
    common::{LexContext, ProtoErr},
    lexer::{lex_next, TokenKind, Token},
	parser::{ParseContext, parse_struct, parse_expr},
	ast::{AstNode, BinOp},
};
use std::fs;


// Helper functions
fn is_operator(token: &Token, expected: TokenKind) -> Result<(), ProtoErr> {
	if token.kind == expected {
		Ok(())
	} else {
		Err(ProtoErr::General(format!("Operators expected {:?} but received {:?}", expected, token.kind), None))
	}
}

fn is_numeric_literal(ast: AstNode, expected: i64) -> Result<(), ProtoErr> {
	if let AstNode::Literal(literal) = ast {
		if literal.kind == TokenKind::Integer(expected) {
			Ok(())
		} else {
			Err(ProtoErr::General(format!("Literal expected {expected} but received {:?}", literal.kind), None))
		}
	} else {
		Err(ProtoErr::General("Node is not a literal".into(), None))
	}
}


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

#[test]
fn test_parser_binop_simple() -> Result<(), ProtoErr> {
	let path = "./samples/test_sources/parser/valid/binary_operation.pr";
	let src = fs::read_to_string(path).unwrap();

	let mut lexer_ctx = LexContext::new(src.clone());
	let mut parser_ctx = ParseContext { current: lex_next(&mut lexer_ctx).unwrap() };

	if let AstNode::BinaryOp(binary_op) = parse_expr(&mut lexer_ctx, &mut parser_ctx)? {
		is_numeric_literal(*binary_op.left, 1)?;
		is_numeric_literal(*binary_op.right, 2)?;
		is_operator(&binary_op.operator, TokenKind::Plus)?;
	} else {
		// For some reason binop returns a new AST
		return Err(ProtoErr::General("Unknown AST node returned from expression".into(), None));
	}
	
	Ok(())
}