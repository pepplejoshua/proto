use super::{source::SourceRef, token::Token};

pub enum Expr {
    Id(Token),
    Number(Token),
    Binary(Token, Box<Expr>, Box<Expr>),
    Boolean(Token),
    Unary(Token, Box<Expr>),
}

impl Expr {
    pub fn source_ref(&self) -> SourceRef {
        match &self {
            Expr::Id(t) => t.get_source_ref().clone(),
            Expr::Number(t) => t.get_source_ref().clone(),
            Expr::Binary(_, lhs, rhs) => {
                let lhs_ref = lhs.source_ref();
                let rhs_ref = rhs.source_ref();
                lhs_ref.combine(&rhs_ref)
            }
            Expr::Boolean(t) => t.get_source_ref().clone(),
            Expr::Unary(operator, operand) => {
                let operator_ref = operator.get_source_ref().clone();
                let operand_ref = operand.source_ref();
                operator_ref.combine(&operand_ref)
            }
        }
    }
}

pub enum Instruction {
    VariableDecl(Token, Expr),
    ExpressionIns(Expr),
}
