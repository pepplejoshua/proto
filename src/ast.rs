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

#[derive(Debug, Clone)]
pub struct BinOp {
    // Note: Box required for recursive type
    pub left: Box<AstNode>,
    pub right: Box<AstNode>,
    pub operator: Token,
}

#[derive(Debug, Clone)]
pub struct UnaryOp {
    // Note: Box required for recursive type
    pub operator: Token,
    pub right: Box<AstNode>,
}

#[derive(Debug, Clone)]
pub enum AstNode {
    BinaryOp(BinOp),
    UnaryOp(UnaryOp),
    Literal(Token),
    Variable(Var),
    Struct(Record),
    Block(Vec<Box<AstNode>>),
    FunctionDef(Function),
}
