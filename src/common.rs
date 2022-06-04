use crate::{lexer::Token, ast::Record};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct ProtoErr {
	pub msg: String,
	pub token: Option<Token>,
}

// Since there is not lexer to hold state,
// the context exists for that purpose
pub struct LexContext {
	pub source: String,
	pub current_ip: usize,
	pub column: usize,
	pub line: usize,
}

impl LexContext {
	pub fn new(source: String) -> Self {
		Self { source , current_ip: 0, column: 1, line: 1 }
	}
}

pub struct Environment {
	// Hold functions etc with AST
	pub records: HashMap<String, Record>,
}

impl Environment {
	pub fn new() -> Self {
		Self { records: HashMap::new() }
	}
}