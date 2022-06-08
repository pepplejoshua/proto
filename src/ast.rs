use crate::lexer::{Span, Token};
use std::{collections::HashMap, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub enum ProtoType {
    I64,
    String,
    Char,
    Bool,
    ArrayOf(Rc<ProtoType>),
    TupleOf(Vec<Rc<ProtoType>>),
}

#[derive(Debug, Clone)]
pub struct Var {
    pub identifier: Rc<Token>,
    pub var_type: Option<Rc<ProtoType>>,
}

#[derive(Debug, Clone)]
pub struct ProtoStruct {
    pub identifier: Rc<Token>,
    // why do we use Span as the key?
    pub members: HashMap<Span, Var>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub identifier: Rc<Token>,
    pub parameters: Vec<Var>,
}

#[derive(Debug, Clone)]
pub struct ProtoBinaryOp {
    pub left: Rc<Expression>,
    pub right: Rc<Expression>,
    pub operator: Rc<Token>,
}

#[derive(Debug, Clone)]
pub struct ProtoUnaryOp {
    pub right: Rc<Expression>,
    pub operator: Rc<Token>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    ExpressionStatement(Rc<Expression>),
    Struct(Rc<ProtoStruct>),
    FunctionDef(Rc<Function>),
    VariableDecl {
        variable: Rc<Var>,
        annotated_type: Option<Rc<ProtoType>>,
        right: Option<Rc<Expression>>,
        is_mutable: bool,
    },
}

#[derive(Debug, Clone)]
pub enum Expression {
    Block(Vec<Rc<ProtoNode>>),
    BinaryOp(ProtoBinaryOp),
    UnaryOp(ProtoUnaryOp),
    Literal(Rc<Token>),
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
    pub program: Vec<ProtoNode>,
}
