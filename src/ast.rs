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

#[derive(Debug, Clone)]
pub enum Statement {
    ExpressionStatement(Expression),
    Struct(Record),
    FunctionDef(Function),
    VariableDecl(Var, Expression),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Block(Vec<Box<ProtoNode>>),
    BinaryOp(BinOp),
    UnaryOp(UnaryOp),
    Literal(Token),
    Variable(Var),
}

#[derive(Debug, Clone)]
pub enum ProtoNode {
    ProtoExpr(Expression),
    ProtoStatement(Statement),
}

#[derive(Debug, Clone)]
pub struct ProtoProgram {
    #[allow(unused)]
    program: Vec<ProtoNode>,
}
