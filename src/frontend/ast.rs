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
                format!("{} {} {}", lhs.as_str(), op.as_str(), rhs.as_str())
            }
            Expr::Comparison(op, lhs, rhs, _) => {
                format!("{} {} {}", lhs.as_str(), op.as_str(), rhs.as_str())
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

                fn_str + &fn_args.join(", ") + ")"
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
    ConstantDecl {
        const_name: Token,
        const_type: Option<Type>,
        init_expr: Expr,
        src_ref: SourceRef,
        public: bool,
    },
    VariableDecl(Token, Option<Type>, Option<Expr>, SourceRef),
    AssignmentIns(Expr, Expr),
    ExpressionIns(Expr, Token),
    FunctionDef {
        name: Token,
        params: Vec<Expr>,
        return_type: Type,
        body: Box<Instruction>,
        is_public: bool,
    },
}

#[allow(dead_code)]
impl Instruction {
    pub fn as_str(&self) -> String {
        match self {
            Instruction::ConstantDecl {
                const_name,
                const_type: t,
                init_expr,
                src_ref: _,
                public: _,
            } => match t {
                Some(c_type) => {
                    format!(
                        "let {} {} = {};",
                        const_name.as_str(),
                        c_type.as_str(),
                        init_expr.as_str()
                    )
                }
                None => {
                    format!("let {} = {};", const_name.as_str(), init_expr.as_str())
                }
            },
            Instruction::VariableDecl(name, t, init, _) => match (t, init) {
                (None, None) => format!("mut {};", name.as_str()),
                (None, Some(init)) => format!("mut {} = {};", name.as_str(), init.as_str()),
                (Some(c_type), None) => {
                    format!("mut {} {};", name.as_str(), c_type.as_str())
                }
                (Some(c_type), Some(init)) => format!(
                    "mut {} {} = {};",
                    name.as_str(),
                    c_type.as_str(),
                    init.as_str()
                ),
            },
            Instruction::AssignmentIns(target, value) => {
                format!("{} = {};", target.as_str(), value.as_str())
            }
            Instruction::ExpressionIns(expr, _) => format!("{};", expr.as_str()),
            Instruction::FunctionDef {
                name,
                params,
                return_type,
                body,
                is_public,
            } => {
                // collect param strings
                let mut param_strs = String::new();
                for (i, param) in params.iter().enumerate() {
                    param_strs.push_str(&param.as_str());
                    if i + 1 < params.len() {
                        param_strs.push_str(", ");
                    }
                }
                let str_rep = format!("fn {} ({param_strs}) {}", name.as_str(), body.as_str());
                if *is_public {
                    ("pub ".to_string() + &str_rep).into()
                } else {
                    str_rep
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub instructions: Vec<Instruction>,
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
