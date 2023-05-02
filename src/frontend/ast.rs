use super::{source::SourceRef, token::Token, types::Type};

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Expr {
    Id(Token, Option<Type>),
    Number(Token, Option<Type>),
    StringLiteral(Token, Option<Type>),
    CharacterLiteral(Token, Option<Type>),
    Binary(Token, Box<Expr>, Box<Expr>, Option<Type>),
    Comparison(Token, Box<Expr>, Box<Expr>, Option<Type>),
    Boolean(Token, Option<Type>),
    Unary(Token, Box<Expr>, Option<Type>),
    Grouped(Box<Expr>, Option<Type>, SourceRef),
    FnCall {
        func: Box<Expr>,
        args: Vec<Expr>,
        span: SourceRef,
        fn_type: Option<Type>,
    },
    ScopeInto {
        module: Box<Expr>,
        target: Box<Expr>,
        src: SourceRef,
        resolved_type: Option<Type>,
    },
    DirectiveExpr {
        directive: Box<Expr>,
        expr: Option<Box<Expr>>,
        resolved_type: Option<Type>,
        src: SourceRef,
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
                func: _,
                args: _,
                span,
                fn_type: _,
            } => span.clone(),
            Expr::Grouped(_, _, src) => src.clone(),
            Expr::ScopeInto {
                module: _,
                target: _,
                src,
                resolved_type: _,
            } => src.clone(),
            Expr::DirectiveExpr {
                directive: _,
                expr: _,
                resolved_type: _,
                src,
            } => src.clone(),
            Expr::StringLiteral(t, _) => t.get_source_ref(),
            Expr::CharacterLiteral(t, _) => t.get_source_ref(),
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
                span: _,
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
            Expr::ScopeInto {
                module,
                target,
                src: _,
                resolved_type: _,
            } => format!("{}::{}", module.as_str(), target.as_str()),
            Expr::DirectiveExpr {
                directive,
                expr,
                resolved_type: _,
                src: _,
            } => {
                if let Some(expr) = expr {
                    format!("@{} {}", directive.as_str(), expr.as_str())
                } else {
                    format!("@{}", directive.as_str())
                }
            }
            Expr::StringLiteral(tok, _) => format!("\"{}\"", tok.as_str()),
            Expr::CharacterLiteral(tok, _) => format!("'{}'", tok.as_str()),
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
                span: _,
                fn_type,
            } => fn_type.clone(),
            Expr::Grouped(_, t, _) => t.clone(),
            Expr::ScopeInto {
                module: _,
                target: _,
                src: _,
                resolved_type,
            } => resolved_type.clone(),
            Expr::DirectiveExpr {
                directive: _,
                expr: _,
                resolved_type,
                src: _,
            } => resolved_type.clone(),
            Expr::StringLiteral(_, t) => t.clone(),
            Expr::CharacterLiteral(_, t) => t.clone(),
        }
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum PathAction {
    ToParentDir(SourceRef),
    ImportAll(SourceRef),
    SearchFor(Expr),
    SearchCoreModulesFor(Expr),
    SearchProjectRootFor(Expr),
    SearchLastModuleFor(Expr),
    SearchCurrentFileFor(Expr),
    NameLastItemAs(Expr),
}

#[allow(dead_code)]
impl PathAction {
    pub fn is_search_action(&self) -> bool {
        match self {
            PathAction::ToParentDir(_) => false,
            PathAction::NameLastItemAs(_) => false,
            PathAction::SearchFor(_) => true,
            PathAction::SearchCoreModulesFor(_) => true,
            PathAction::SearchProjectRootFor(_) => true,
            PathAction::SearchLastModuleFor(_) => true,
            PathAction::SearchCurrentFileFor(_) => true,
            PathAction::ImportAll(_) => false,
        }
    }

    pub fn is_terminating_action(&self) -> bool {
        match self {
            PathAction::ToParentDir(_) => false,
            PathAction::SearchFor(_) => true,
            PathAction::NameLastItemAs(_) => true,
            PathAction::SearchCoreModulesFor(_) => true,
            PathAction::SearchProjectRootFor(_) => true,
            PathAction::SearchLastModuleFor(_) => true,
            PathAction::SearchCurrentFileFor(_) => false,
            PathAction::ImportAll(_) => true,
        }
    }

    pub fn allows_naming_last(&self) -> bool {
        match self {
            PathAction::ToParentDir(_) => false,
            PathAction::SearchFor(_) => true,
            PathAction::NameLastItemAs(_) => false,
            PathAction::SearchCoreModulesFor(_) => true,
            PathAction::SearchProjectRootFor(_) => true,
            PathAction::SearchLastModuleFor(_) => true,
            PathAction::SearchCurrentFileFor(_) => false,
            PathAction::ImportAll(_) => false,
        }
    }

    pub fn as_str(&self) -> String {
        match self {
            PathAction::ToParentDir(_) => "^".to_string(),
            PathAction::SearchFor(e) => e.as_str(),
            PathAction::NameLastItemAs(alias) => format!(" as {}", alias.as_str()),
            PathAction::SearchCoreModulesFor(e) => format!("@{}", e.as_str()),
            PathAction::SearchProjectRootFor(m) => format!("${}", m.as_str()),
            PathAction::SearchLastModuleFor(n) => n.as_str(),
            PathAction::SearchCurrentFileFor(inner_m) => format!("!{}", inner_m.as_str()),
            PathAction::ImportAll(_) => "*".to_string(),
        }
    }

    pub fn source_ref(&self) -> SourceRef {
        match self {
            PathAction::ToParentDir(src) => src.clone(),
            PathAction::SearchFor(e) => e.source_ref(),
            PathAction::SearchCoreModulesFor(e) => e.source_ref(),
            PathAction::SearchProjectRootFor(e) => e.source_ref(),
            PathAction::SearchLastModuleFor(e) => e.source_ref(),
            PathAction::SearchCurrentFileFor(e) => e.source_ref(),
            PathAction::NameLastItemAs(e) => e.source_ref(),
            PathAction::ImportAll(src) => src.clone(),
        }
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct DependencyPath {
    pub actions: Vec<PathAction>,
}

#[allow(dead_code)]
impl DependencyPath {
    pub fn combine(&self, sub_path: &DependencyPath) -> DependencyPath {
        let mut actions = self.actions.clone();
        let mut sub_path_actions = sub_path.actions.clone();
        actions.append(&mut sub_path_actions);
        DependencyPath { actions }
    }

    pub fn source_ref(&self) -> SourceRef {
        // get first action
        let first_action = self.actions.first().unwrap();
        // get last action
        let last_action = self.actions.last().unwrap();
        first_action.source_ref().combine(last_action.source_ref())
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
        is_public: bool,
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
        value: Expr,
    },
    Break(SourceRef),
    Continue(SourceRef),
    UseDependency {
        paths: Vec<DependencyPath>,
        src: SourceRef,
    },
    DirectiveInstruction {
        directive: Expr,
        block: Option<Box<Instruction>>,
        src: SourceRef,
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
                format!("return {}", value.as_str())
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
            Instruction::UseDependency { paths, src: _ } => {
                let mut path_str = String::new();
                for (i, path) in paths.iter().enumerate() {
                    for (j, part) in path.actions.iter().enumerate() {
                        path_str.push_str(&part.as_str());
                        if j + 1 < path.actions.len() {
                            path_str.push_str("::");
                        }
                    }
                    if i + 1 < paths.len() {
                        path_str.push_str(", ");
                    }
                }
                format!("use {};", path_str)
            }
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
        }
    }

    pub fn source_ref(&self) -> SourceRef {
        match self {
            Instruction::ConstantDecl {
                const_name: _,
                const_type: _,
                init_expr: _,
                src_ref,
                is_public: _,
            } => src_ref.clone(),
            Instruction::VariableDecl(_, _, _, src) => src.clone(),
            Instruction::AssignmentIns(target, value) => {
                target.source_ref().combine(value.source_ref())
            }
            Instruction::ExpressionIns(expr, terminator) => {
                expr.source_ref().combine(terminator.get_source_ref())
            }
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
            Instruction::UseDependency { paths: _, src } => src.clone(),
            Instruction::DirectiveInstruction {
                directive: _,
                block: _,
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
