use super::{source::SourceRef, token::Token, types::Type};

#[derive(Debug, Clone)]
pub enum Expr {
    Id(Token, Option<Type>),
    Number(Token, Option<Type>),
    Binary(Token, Box<Expr>, Box<Expr>, Option<Type>),
    Comparison(Token, Box<Expr>, Box<Expr>, Option<Type>),
    Boolean(Token, Option<Type>),
    Unary(Token, Box<Expr>, Option<Type>),
    FnCall {
        func: Box<Expr>,
        args: Vec<Expr>,
        rparen: Token,
        fn_type: Option<Type>,
    },
}

impl Expr {
    pub fn source_ref(&self) -> SourceRef {
        match &self {
            Expr::Id(t, _) => t.get_source_ref().clone(),
            Expr::Number(t, _) => t.get_source_ref().clone(),
            Expr::Binary(_, lhs, rhs, _) => {
                let lhs_ref = lhs.source_ref();
                let rhs_ref = rhs.source_ref();
                lhs_ref.combine(&rhs_ref)
            }
            Expr::Boolean(t, _) => t.get_source_ref().clone(),
            Expr::Unary(operator, operand, _) => {
                let operator_ref = operator.get_source_ref().clone();
                let operand_ref = operand.source_ref();
                operator_ref.combine(&operand_ref)
            }
            Expr::Comparison(_, lhs, rhs, _) => {
                let lhs_ref = lhs.source_ref();
                let rhs_ref = rhs.source_ref();
                lhs_ref.combine(&rhs_ref)
            }
            Expr::FnCall {
                func,
                args: _,
                rparen,
                fn_type: _,
            } => func.source_ref().combine(rparen.get_source_ref()),
        }
    }

    pub fn type_info(&self) -> Option<Type> {
        match &self {
            Expr::Id(_, t) => t.clone(),
            Expr::Number(_, t) => t.clone(),
            Expr::Binary(_, _, _, t) => t.clone(),
            Expr::Boolean(_, t) => t.clone(),
            Expr::Unary(_, _, t) => t.clone(),
            Expr::Comparison(_, _, _, t) => t.clone(),
            Expr::FnCall {
                func: _,
                args: _,
                rparen: _,
                fn_type,
            } => fn_type.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    ConstantDecl(Token, Option<Type>, Expr),
    VariableDecl(Token, Option<Type>, Option<Expr>),
    ExpressionIns(Expr),
}

#[derive(Debug, Clone)]
pub struct Module {
    instructions: Vec<Instruction>,
}

impl Module {
    pub fn new() -> Module {
        Module {
            instructions: vec![],
        }
    }

    pub fn add_instruction(&mut self, ins: Instruction) {
        self.instructions.push(ins);
    }
}
