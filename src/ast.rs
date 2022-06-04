use crate::lexer::{Span, Token};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Var {
    pub identifier: Token,
}

#[derive(Debug, Clone)]
pub struct Record {
    pub identifier: Token,
    pub members: HashMap<Span, Var>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub identifier: Token,
    pub parameters: Vec<Span>,
}

#[derive(Debug)]
pub enum AstNode {
    Variable(Var),
    Struct(Record),
    Block(Vec<Box<AstNode>>),
    FunctionDef(Function),
}
