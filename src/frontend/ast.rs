use super::{source::SourceRef, token::Token, types::Type};

#[derive(Debug, Clone)]
pub enum Expr {
    Id(Token, Option<Type>),
    Number(Token, Option<Type>),
    Binary(Token, Box<Expr>, Box<Expr>, Option<Type>),
    Comparison(Token, Box<Expr>, Box<Expr>, Option<Type>),
    Boolean(Token, Option<Type>),
    Unary(Token, Box<Expr>, Option<Type>),
    Grouped(Box<Expr>, Option<Type>, SourceRef),
    FnCall {
        func: Box<Expr>,
        args: Vec<Expr>,
        rparen: Token,
        fn_type: Option<Type>,
    },
}

#[allow(dead_code)]
impl Expr {
    pub fn source_ref(&self) -> SourceRef {
        match &self {
            Expr::Id(t, _) => t.get_source_ref(),
            Expr::Number(t, _) => t.get_source_ref(),
            Expr::Binary(_, lhs, rhs, _) => {
                let lhs_ref = lhs.source_ref();
                let rhs_ref = rhs.source_ref();
                lhs_ref.combine(rhs_ref)
            }
            Expr::Boolean(t, _) => t.get_source_ref(),
            Expr::Unary(operator, operand, _) => {
                let operator_ref = operator.get_source_ref();
                let operand_ref = operand.source_ref();
                operator_ref.combine(operand_ref)
            }
            Expr::Comparison(_, lhs, rhs, _) => {
                let lhs_ref = lhs.source_ref();
                let rhs_ref = rhs.source_ref();
                lhs_ref.combine(rhs_ref)
            }
            Expr::FnCall {
                func,
                args: _,
                rparen,
                fn_type: _,
            } => func.source_ref().combine(rparen.get_source_ref()),
            Expr::Grouped(_, _, src) => src.clone(),
        }
    }

    pub fn as_str(&self) -> String {
        match self {
            Expr::Id(tok, _) => tok.as_str(),
            Expr::Number(num, _) => num.as_str(),
            Expr::Binary(op, lhs, rhs, _) => {
                format!("{} {} {}", op.as_str(), lhs.as_str(), rhs.as_str())
            }
            Expr::Comparison(op, lhs, rhs, _) => {
                format!("{} {} {}", op.as_str(), lhs.as_str(), rhs.as_str())
            }
            Expr::Boolean(val, _) => val.as_str(),
            Expr::Unary(op, operand, _) => format!("{} {}", op.as_str(), operand.as_str()),
            Expr::FnCall {
                func,
                args,
                rparen: _,
                fn_type: _,
            } => {
                let mut fn_str = func.as_str();
                fn_str.push('(');

                let mut fn_args = vec![];
                for arg in args {
                    fn_args.push(arg.as_str());
                }

                fn_str + &fn_args.join(", ")
            }
            Expr::Grouped(e, _, _) => {
                let s = format!("({})", e.as_str());
                s
            }
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
            Expr::Grouped(_, t, _) => t.clone(),
        }
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Instruction {
    ConstantDecl(Token, Option<Type>, Expr, SourceRef),
    VariableDecl(Token, Option<Type>, Option<Expr>, SourceRef),
    AssignmentIns(Expr, Expr),
    ExpressionIns(Expr, Token),
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
