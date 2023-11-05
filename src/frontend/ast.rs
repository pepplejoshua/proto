use super::{source::SourceRef, token::Token, types::TypeSignature};

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Expr {
    TypedId(Token, TypeSignature),
    UntypedId(Token),
    Number(Token),
    StringLiteral(Token),
    CharacterLiteral(Token),
    Binary(Token, Box<Expr>, Box<Expr>),
    Comparison(Token, Box<Expr>, Box<Expr>),
    Boolean(Token, bool),
    Unary(Token, Box<Expr>),
    Grouped(Box<Expr>, SourceRef),
    FnCall {
        func: Box<Expr>,
        args: Vec<Expr>,
        span: SourceRef,
    },
    ScopeInto {
        module: Box<Expr>,
        target: Box<Expr>,
        src: SourceRef,
    },
    DirectiveExpr {
        directive: Box<Expr>,
        expr: Option<Box<Expr>>,
        src: SourceRef,
    },
    NamedStructLiteral {
        // using StructName { a bool, b char } or StructName { a = true, b = 'a' }
        name: Box<Expr>,
        fields: KeyValueBindings,
        src: SourceRef,
    },
    Integer(String, SourceRef),
}

#[allow(dead_code)]
impl Expr {
    pub fn source_ref(&self) -> SourceRef {
        match &self {
            Expr::Number(t) => t.get_source_ref(),
            Expr::Binary(_, lhs, rhs) => {
                let lhs_ref = lhs.source_ref();
                let rhs_ref = rhs.source_ref();
                lhs_ref.combine(rhs_ref)
            }
            Expr::Boolean(t, _) => t.get_source_ref(),
            Expr::Unary(operator, operand) => {
                let operator_ref = operator.get_source_ref();
                let operand_ref = operand.source_ref();
                operator_ref.combine(operand_ref)
            }
            Expr::Comparison(_, lhs, rhs) => {
                let lhs_ref = lhs.source_ref();
                let rhs_ref = rhs.source_ref();
                lhs_ref.combine(rhs_ref)
            }
            Expr::FnCall {
                func: _,
                args: _,
                span,
            } => span.clone(),
            Expr::Grouped(_, src) => src.clone(),
            Expr::ScopeInto {
                module: _,
                target: _,
                src,
            } => src.clone(),
            Expr::DirectiveExpr {
                directive: _,
                expr: _,
                src,
            } => src.clone(),
            Expr::StringLiteral(t) => t.get_source_ref(),
            Expr::CharacterLiteral(t) => t.get_source_ref(),
            Expr::NamedStructInit {
                name: _,
                fields: _,
                src,
            } => src.clone(),
            Expr::TypedId(t, _) => t.get_source_ref(),
            Expr::UntypedId(t) => t.get_source_ref(),
        }
    }

    pub fn as_str(&self) -> String {
        match self {
            Expr::Number(num) => num.as_str(),
            Expr::Binary(op, lhs, rhs) => {
                format!("{} {} {}", lhs.as_str(), op.as_str(), rhs.as_str())
            }
            Expr::Comparison(op, lhs, rhs) => {
                format!("{} {} {}", lhs.as_str(), op.as_str(), rhs.as_str())
            }
            Expr::Boolean(val, _) => val.as_str(),
            Expr::Unary(op, operand) => format!("{} {}", op.as_str(), operand.as_str()),
            Expr::FnCall {
                func,
                args,
                span: _,
            } => {
                let mut fn_str = func.as_str();
                fn_str.push('(');

                let mut fn_args = vec![];
                for arg in args {
                    fn_args.push(arg.as_str());
                }

                fn_str + &fn_args.join(", ") + ")"
            }
            Expr::Grouped(e, _) => {
                let s = format!("({})", e.as_str());
                s
            }
            Expr::ScopeInto {
                module,
                target,
                src: _,
            } => format!("{}.{}", module.as_str(), target.as_str()),
            Expr::DirectiveExpr {
                directive,
                expr,
                src: _,
            } => {
                if let Some(expr) = expr {
                    format!("@{} {}", directive.as_str(), expr.as_str())
                } else {
                    format!("@{}", directive.as_str())
                }
            }
            Expr::StringLiteral(literal) => literal.as_str(),
            Expr::CharacterLiteral(literal) => literal.as_str(),
            Expr::NamedStructInit {
                name,
                fields,
                src: _,
            } => {
                let mut s = format!(":{} ", name.as_str());
                s.push_str(&fields.as_str());
                s
            }
            Expr::TypedId(id, id_type) => format!("{} {}", id.as_str(), id_type.as_str()),
            Expr::UntypedId(id) => id.as_str(),
        }
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct KeyValueBindings {
    pub pairs: Vec<(Expr, Option<Expr>)>,
    pub span: SourceRef,
}

#[allow(dead_code)]
impl KeyValueBindings {
    pub fn as_str(&self) -> String {
        let mut s = String::from("{ ");
        for (index, (k, v)) in self.pairs.iter().enumerate() {
            s.push_str(&k.as_str());
            if let Some(v) = v {
                s.push_str(" = ");
                s.push_str(&v.as_str());
            }

            if index < self.pairs.len() - 1 {
                s.push_str(", ");
            }
        }
        s.push_str(" }");
        s
    }

    pub fn source_ref(&self) -> SourceRef {
        self.span.clone()
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Instruction {
    ConstantDecl {
        const_name: Token,
        const_type: Option<TypeSignature>,
        init_expr: Expr,
        src_ref: SourceRef,
        is_public: bool,
    },
    VariableDecl(Token, Option<TypeSignature>, Option<Expr>, SourceRef),
    AssignmentIns(Expr, Expr),
    ExpressionIns(Expr, Token),
    FunctionDef {
        name: Token,
        params: Vec<Expr>,
        return_type: TypeSignature,
        body: Box<Instruction>,
        is_public: bool,
        src: SourceRef,
    },
    InfiniteLoop {
        src: SourceRef,
        body: Box<Instruction>,
    },
    WhileLoop {
        src: SourceRef,
        condition: Expr,
        body: Box<Instruction>,
    },
    CodeBlock {
        src: SourceRef,
        instructions: Vec<Instruction>,
    },
    Module {
        name: Expr,
        body: Box<Instruction>,
        src: SourceRef,
        is_public: bool,
    },
    Return {
        src: SourceRef,
        value: Option<Expr>,
    },
    Break(SourceRef),
    Continue(SourceRef),
    DirectiveInstruction {
        directive: Expr,
        block: Box<Instruction>,
        src: SourceRef,
    },
    ConditionalBranchIns {
        pairs: Vec<(Option<Expr>, Box<Instruction>)>,
        src: SourceRef,
    },
    SingleLineComment {
        comment: String,
        src: SourceRef,
    },
}

#[allow(dead_code)]
impl Instruction {
    pub fn as_str(&self) -> String {
        match self {
            Instruction::SingleLineComment { comment, src: _ } => comment.clone(),
            Instruction::ConstantDecl {
                const_name,
                const_type: t,
                init_expr,
                src_ref: _,
                is_public: _,
            } => match (t, init_expr) {
                (None, None) => unreachable!("constant decl with no type or init expr"),
                (None, Some(init)) => format!("let {} = {};", const_name.as_str(), init.as_str()),
                (Some(c_type), None) => {
                    format!("let {} {};", const_name.as_str(), c_type.as_str())
                }
                (Some(c_type), Some(init)) => format!(
                    "let {} {} = {};",
                    const_name.as_str(),
                    c_type.as_str(),
                    init.as_str()
                ),
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
            Instruction::AssignmentIns(target, value, _) => {
                format!("{} = {};", target.as_str(), value.as_str())
            }
            Instruction::ExpressionIns(expr, _) => format!("{};", expr.as_str()),
            Instruction::FunctionPrototype {
                name,
                params,
                return_type,
                is_public,
                src: _,
            } => {
                // collect param strings
                let mut param_strs = String::new();
                for (i, param) in params.iter().enumerate() {
                    param_strs.push_str(&param.as_str());
                    if i + 1 < params.len() {
                        param_strs.push_str(", ");
                    }
                }
                let str_rep = format!(
                    "fn {} ({param_strs}) {};",
                    name.as_str(),
                    return_type.as_str()
                );
                if *is_public {
                    "pub ".to_string() + &str_rep
                } else {
                    str_rep
                }
            }
            Instruction::FunctionDef {
                name,
                params,
                return_type,
                body,
                is_public,
                src: _,
            } => {
                // collect param strings
                let mut param_strs = String::new();
                for (i, param) in params.iter().enumerate() {
                    param_strs.push_str(&param.as_str());
                    if i + 1 < params.len() {
                        param_strs.push_str(", ");
                    }
                }
                let str_rep = format!(
                    "fn {} ({param_strs}) {} {}",
                    name.as_str(),
                    return_type.as_str(),
                    body.as_str()
                );
                if *is_public {
                    "pub ".to_string() + &str_rep
                } else {
                    str_rep
                }
            }
            Instruction::CodeBlock {
                src: _,
                instructions,
            } => {
                let mut ins_str = "{ ".to_string();

                for (id, ins) in instructions.iter().enumerate() {
                    ins_str.push_str(&ins.as_str());

                    if id + 1 < instructions.len() {
                        ins_str.push(' ');
                    }
                }
                ins_str + " }"
            }
            Instruction::Module {
                name,
                body,
                src: _,
                is_public,
            } => {
                let mod_str = format!("mod {} {}", name.as_str(), body.as_str());
                if *is_public {
                    mod_str
                } else {
                    "pub ".to_string() + &mod_str
                }
            }
            Instruction::Return { src: _, value } => {
                if let Some(value) = value {
                    format!("return {};", value.as_str())
                } else {
                    "return;".to_string()
                }
            }
            Instruction::InfiniteLoop { src: _, body } => {
                format!("loop {}", body.as_str())
            }
            Instruction::WhileLoop {
                src: _,
                condition,
                body,
            } => {
                format!("while {} {}", condition.as_str(), body.as_str())
            }
            Instruction::Break(_) => "break;".to_string(),
            Instruction::Continue(_) => "continue;".to_string(),
            Instruction::DirectiveInstruction {
                directive,
                block,
                src: _,
            } => {
                format!("@{} {}", directive.as_str(), block.as_str())
            }
            Instruction::ConditionalBranchIns { pairs, src: _ } => {
                let mut str_rep = String::new();
                for i in 0..pairs.len() {
                    let (cond, ins) = &pairs[i];
                    if i == 0 {
                        // for the first pair, we do if <cond> <ins>
                        str_rep.push_str(&format!(
                            "if {} {}",
                            cond.clone().unwrap().as_str(),
                            ins.as_str()
                        ));
                    } else if i < pairs.len() - 1 {
                        // for the rest, we do else if <cond> <ins>
                        str_rep.push_str(&format!(
                            " else if {} {}",
                            cond.clone().unwrap().as_str(),
                            ins.as_str()
                        ));
                    } else {
                        // for the last, we do else <ins>
                        str_rep.push_str(&format!(" else {}", ins.as_str()));
                    }
                }
                str_rep
            }
        }
    }

    pub fn source_ref(&self) -> SourceRef {
        match self {
            Instruction::SingleLineComment { comment: _, src } => src.clone(),
            Instruction::ConstantDecl {
                const_name: _,
                const_type: _,
                init_expr: _,
                src_ref,
                is_public: _,
            } => src_ref.clone(),
            Instruction::VariableDecl(_, _, _, src) => src.clone(),
            Instruction::AssignmentIns(target, value, _) => {
                target.source_ref().combine(value.source_ref())
            }
            Instruction::ExpressionIns(_, src) => src.clone(),
            Instruction::FunctionPrototype {
                name: _,
                params: _,
                return_type: _,
                is_public: _,
                src,
            } => src.clone(),
            Instruction::FunctionDef {
                name: _,
                params: _,
                return_type: _,
                body: _,
                is_public: _,
                src,
            } => src.clone(),
            Instruction::CodeBlock {
                src,
                instructions: _,
            } => src.clone(),
            Instruction::Module {
                name: _,
                body: _,
                src,
                is_public: _,
            } => src.clone(),
            Instruction::Return { src, value: _ } => src.clone(),
            Instruction::InfiniteLoop { src, body: _ } => src.clone(),
            Instruction::WhileLoop {
                src,
                condition: _,
                body: _,
            } => src.clone(),
            Instruction::Break(src) => src.clone(),
            Instruction::Continue(src) => src.clone(),
            Instruction::DirectiveInstruction {
                directive: _,
                block: _,
                src,
            } => src.clone(),
            Instruction::ConditionalBranchIns { pairs: _, src } => src.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompilationModule {
    pub instructions: Vec<Instruction>,
}

impl CompilationModule {
    pub fn new() -> CompilationModule {
        CompilationModule {
            instructions: vec![],
        }
    }

    pub fn add_instruction(&mut self, ins: Instruction) {
        self.instructions.push(ins);
    }
}
