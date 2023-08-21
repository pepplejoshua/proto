use super::{source::SourceRef, token::Token};

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum TypeReference {
    IdentifierType(String, Option<SourceRef>),
}

#[allow(dead_code)]
impl TypeReference {
    pub fn as_str(&self) -> String {
        match self {
            TypeReference::IdentifierType(s, _) => s.clone(),
        }
    }

    pub fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TypeReference::IdentifierType(s1, _), TypeReference::IdentifierType(s2, _)) => {
                s1 == s2
            }
        }
    }

    pub fn type_from(tag: &str) -> Self {
        TypeReference::IdentifierType(tag.to_string(), None)
    }

    pub fn identifier_type_with_loc(tag: &str, at: SourceRef) -> Self {
        TypeReference::IdentifierType(tag.to_string(), Some(at))
    }

    pub fn get_source_ref(&self) -> Option<SourceRef> {
        match self {
            TypeReference::IdentifierType(_, s) => s.clone(),
        }
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Expr {
    Id(Token, Option<TypeReference>, SourceRef),
    Number(Token, SourceRef),
    SingleLineStringLiteral(Token, SourceRef),
    MultiLineStringLiteral(Vec<Token>, SourceRef),
    CharacterLiteral(Token, SourceRef),
    Binary(Token, Box<Expr>, Box<Expr>, SourceRef),
    Comparison(Token, Box<Expr>, Box<Expr>, SourceRef),
    Boolean(Token, SourceRef),
    Unary(Token, Box<Expr>, SourceRef),
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
    AnonStructLiteral {
        // using .{ a bool, b char }
        fields: KeyValueBindings,
        src: SourceRef,
    },
    StructDecl {
        src: SourceRef,
        contents: Box<Instruction>,
    },
    Integer(String, SourceRef),
}

#[allow(dead_code)]
impl Expr {
    pub fn source_ref(&self) -> SourceRef {
        match &self {
            Expr::Id(_, _, s) => s.clone(),
            Expr::Number(_, s) => s.clone(),
            Expr::Binary(_, _, _, s) => s.clone(),
            Expr::Boolean(_, s) => s.clone(),
            Expr::Unary(_, _, s) => s.clone(),
            Expr::Comparison(_, _, _, s) => s.clone(),
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
            Expr::SingleLineStringLiteral(_, s) => s.clone(),
            Expr::CharacterLiteral(_, s) => s.clone(),
            Expr::MultiLineStringLiteral(_, s) => s.clone(),
            Expr::Integer(_, s) => s.clone(),
            Expr::NamedStructLiteral {
                name: _,
                fields: _,
                src,
            } => src.clone(),
            Expr::AnonStructLiteral { fields: _, src } => src.clone(),
            Expr::StructDecl { src, contents: _ } => src.clone(),
        }
    }

    pub fn as_str(&self) -> String {
        match self {
            Expr::Id(tok, maybe_type, _) => {
                let mut s = tok.as_str().to_string();
                if let Some(t) = maybe_type {
                    s.push_str(&format!(" {}", t.as_str()));
                }
                s
            }
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
            Expr::SingleLineStringLiteral(literal, _) => literal.as_str(),
            Expr::CharacterLiteral(literal, _) => literal.as_str(),
            Expr::MultiLineStringLiteral(literals, _) => {
                let mut s = String::new();
                // start each line with ||
                for (index, line) in literals.iter().enumerate() {
                    s.push_str("||");
                    s.push_str(&line.as_str());
                    if index < literals.len() - 1 {
                        s.push('\n');
                    }
                }
                s
            }
            Expr::Integer(num, _) => num.clone(),
            Expr::NamedStructLiteral { name, fields, .. } => {
                format!("{} {}", name.as_str(), fields.as_str())
            }
            Expr::AnonStructLiteral { fields, .. } => format!(".{}", fields.as_str()),
            Expr::StructDecl { src: _, contents } => format!("struct {}", contents.as_str()),
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
    NamedStructDecl {
        name: Expr,
        fields: KeyValueBindings,
        src: SourceRef,
    },
    ConstantDecl {
        const_name: Token,
        const_type: Option<TypeReference>,
        init_expr: Expr,
        src_ref: SourceRef,
        is_public: bool,
    },
    VariableDecl(Token, Option<TypeReference>, Option<Expr>, SourceRef),
    AssignmentIns(Expr, Expr, SourceRef),
    ExpressionIns(Expr, SourceRef),
    FunctionPrototype {
        name: Token,
        params: Vec<Expr>,
        return_type: TypeReference,
        is_public: bool,
        src: SourceRef,
    },
    FunctionDef {
        name: Token,
        params: Vec<Expr>,
        return_type: TypeReference,
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
        block: Option<Box<Instruction>>,
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
            Instruction::NamedStructDecl {
                name,
                fields,
                src: _,
            } => {
                format!(":{} {}", name.as_str(), fields.as_str())
            }
            Instruction::ConstantDecl {
                const_name,
                const_type: t,
                init_expr,
                src_ref: _,
                is_public: _,
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
                if let Some(block) = block {
                    format!("@{} {}", directive.as_str(), block.as_str())
                } else {
                    format!("@{}", directive.as_str())
                }
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
            Instruction::NamedStructDecl {
                name: _,
                fields: _,
                src,
            } => src.clone(),
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
