use crate::frontend::{
    ast::{
        CompilationModule, DependencyPath, Expr, Instruction, KeyValueBindings as TreeKVBindings,
    },
    source::SourceRef,
    token::Token,
    types::Type,
};

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct KeyValueBindings {
    pub pairs: Vec<(ExprRef, Option<ExprRef>)>,
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
pub enum PIRExpr {
    Id(Token, Option<Type>),
    Number(Token, Option<Type>),
    StringLiteral(Token, Option<Type>),
    CharacterLiteral(Token, Option<Type>),
    Binary(Token, ExprRef, ExprRef, Option<Type>),
    Comparison(Token, ExprRef, ExprRef, Option<Type>),
    Boolean(Token, Option<Type>),
    Unary(Token, ExprRef, Option<Type>),
    Grouped(ExprRef, Option<Type>, SourceRef),
    FnCall {
        func: ExprRef,
        args: Vec<ExprRef>,
        span: SourceRef,
        fn_type: Option<Type>,
    },
    ScopeInto {
        module: ExprRef,
        target: ExprRef,
        src: SourceRef,
        resolved_type: Option<Type>,
    },
    DirectiveExpr {
        directive: ExprRef,
        expr: Option<ExprRef>,
        resolved_type: Option<Type>,
        src: SourceRef,
    },
    NamedStructInit {
        name: ExprRef,
        fields: KeyValueBindings,
        src: SourceRef,
        resolved_type: Option<Type>,
    },
}

#[allow(dead_code)]
impl PIRExpr {
    pub fn type_info(&self) -> Option<Type> {
        match &self {
            PIRExpr::Id(_, t) => t.clone(),
            PIRExpr::Number(_, t) => t.clone(),
            PIRExpr::Binary(_, _, _, t) => t.clone(),
            PIRExpr::Boolean(_, t) => t.clone(),
            PIRExpr::Unary(_, _, t) => t.clone(),
            PIRExpr::Comparison(_, _, _, t) => t.clone(),
            PIRExpr::FnCall {
                func: _,
                args: _,
                span: _,
                fn_type,
            } => fn_type.clone(),
            PIRExpr::Grouped(_, t, _) => t.clone(),
            PIRExpr::ScopeInto {
                module: _,
                target: _,
                src: _,
                resolved_type,
            } => resolved_type.clone(),
            PIRExpr::DirectiveExpr {
                directive: _,
                expr: _,
                resolved_type,
                src: _,
            } => resolved_type.clone(),
            PIRExpr::StringLiteral(_, t) => t.clone(),
            PIRExpr::CharacterLiteral(_, t) => t.clone(),
            PIRExpr::NamedStructInit {
                name: _,
                fields: _,
                src: _,
                resolved_type,
            } => resolved_type.clone(),
        }
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct ExprRef {
    pub loc: usize,
}

impl ExprRef {
    pub fn as_str(&self) -> String {
        format!("ExprRef({})", self.loc)
    }
}

#[derive(Debug, Clone)]
pub struct ExprPool {
    pub pool: Vec<PIRExpr>,
}

#[allow(dead_code)]
impl ExprPool {
    pub fn new() -> Self {
        ExprPool { pool: Vec::new() }
    }

    fn add(&mut self, expr: PIRExpr) -> ExprRef {
        self.pool.push(expr);
        ExprRef {
            loc: self.pool.len() - 1,
        }
    }

    pub fn lowir(&mut self, expr: Expr) -> ExprRef {
        match expr {
            Expr::Id(id, t) => self.add(PIRExpr::Id(id, t)),
            Expr::Number(n, t) => self.add(PIRExpr::Number(n, t)),
            Expr::StringLiteral(s, t) => self.add(PIRExpr::StringLiteral(s, t)),
            Expr::CharacterLiteral(c, t) => self.add(PIRExpr::CharacterLiteral(c, t)),
            Expr::Binary(op, lhs, rhs, t) => {
                let lhs = self.lowir(*lhs);
                let rhs = self.lowir(*rhs);
                self.add(PIRExpr::Binary(op, lhs, rhs, t))
            }
            Expr::Comparison(op, lhs, rhs, t) => {
                let lhs = self.lowir(*lhs);
                let rhs = self.lowir(*rhs);
                self.add(PIRExpr::Comparison(op, lhs, rhs, t))
            }
            Expr::Boolean(b, t) => self.add(PIRExpr::Boolean(b, t)),
            Expr::Unary(op, expr, t) => {
                let expr = self.lowir(*expr);
                self.add(PIRExpr::Unary(op, expr, t))
            }
            Expr::Grouped(expr, t, src) => {
                let expr = self.lowir(*expr);
                self.add(PIRExpr::Grouped(expr, t, src))
            }
            Expr::FnCall {
                func,
                args,
                span,
                fn_type,
            } => {
                let func = self.lowir(*func);
                let args = args
                    .into_iter()
                    .map(|arg| self.lowir(arg))
                    .collect::<Vec<ExprRef>>();
                self.add(PIRExpr::FnCall {
                    func,
                    args,
                    span,
                    fn_type,
                })
            }
            Expr::ScopeInto {
                module,
                target,
                src,
                resolved_type,
            } => {
                let module = self.lowir(*module);
                let target = self.lowir(*target);
                self.add(PIRExpr::ScopeInto {
                    module,
                    target,
                    src,
                    resolved_type,
                })
            }
            Expr::DirectiveExpr {
                directive,
                expr,
                resolved_type,
                src,
            } => {
                let directive = self.lowir(*directive);
                let expr = expr.map(|expr| self.lowir(*expr));
                self.add(PIRExpr::DirectiveExpr {
                    directive,
                    expr,
                    resolved_type,
                    src,
                })
            }
            Expr::NamedStructInit {
                name,
                fields,
                src,
                resolved_type,
            } => {
                let name = self.lowir(*name);
                let fields = self.pir_bindings(fields);
                self.add(PIRExpr::NamedStructInit {
                    name,
                    fields,
                    src,
                    resolved_type,
                })
            }
        }
    }

    fn pir_bindings(&mut self, bindings: TreeKVBindings) -> KeyValueBindings {
        let nbindings = bindings
            .pairs
            .iter()
            .map(|pair| {
                let key = self.lowir(pair.0.clone());
                if let Some(value) = pair.1.clone() {
                    let value = self.lowir(value);
                    (key, Some(value))
                } else {
                    (key, None)
                }
            })
            .collect::<Vec<(ExprRef, Option<ExprRef>)>>();
        KeyValueBindings {
            pairs: nbindings,
            span: bindings.span,
        }
    }

    pub fn get(&self, expr_ref: &ExprRef) -> PIRExpr {
        self.pool[expr_ref.loc].clone()
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum PIRIns {
    SingleLineComment {
        comment: String,
        src: SourceRef,
    },
    NamedStructDecl {
        name: ExprRef,
        fields: KeyValueBindings,
        src: SourceRef,
    },
    ConstantDecl {
        const_name: Token,
        const_type: Option<Type>,
        init_expr: ExprRef,
        src_ref: SourceRef,
        is_public: bool,
    },
    VariableDecl(Token, Option<Type>, Option<ExprRef>, SourceRef),
    AssignmentIns(ExprRef, ExprRef),
    ExpressionIns(ExprRef, Token),
    FunctionDef {
        name: Token,
        params: Vec<ExprRef>,
        return_type: Type,
        body: InsRef,
        is_public: bool,
        src: SourceRef,
    },
    InfiniteLoop {
        src: SourceRef,
        body: InsRef,
    },
    WhileLoop {
        src: SourceRef,
        condition: ExprRef,
        body: InsRef,
    },
    CodeBlock {
        src: SourceRef,
        instructions: Vec<InsRef>,
    },
    Module {
        name: ExprRef,
        body: InsRef,
        src: SourceRef,
        is_public: bool,
    },
    Return {
        src: SourceRef,
        value: Option<ExprRef>,
    },
    Break(SourceRef),
    Continue(SourceRef),
    UseDependency {
        paths: Vec<DependencyPath>,
        src: SourceRef,
    },
    DirectiveInstruction {
        directive: ExprRef,
        block: Option<InsRef>,
        src: SourceRef,
    },
    ConditionalBranchIns {
        pairs: Vec<(Option<ExprRef>, InsRef)>,
        src: SourceRef,
    },
}

impl PIRIns {
    pub fn as_str(&self) -> String {
        match self {
            PIRIns::SingleLineComment { comment, src: _ } => comment.clone(),
            PIRIns::NamedStructDecl {
                name,
                fields,
                src: _,
            } => {
                format!(":{} {}", name.as_str(), fields.as_str())
            }
            PIRIns::ConstantDecl {
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
            PIRIns::VariableDecl(name, t, init, _) => match (t, init) {
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
            PIRIns::AssignmentIns(target, value) => {
                format!("{} = {};", target.as_str(), value.as_str())
            }
            PIRIns::ExpressionIns(expr, _) => format!("{};", expr.as_str()),
            PIRIns::FunctionDef {
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
            PIRIns::CodeBlock {
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
            PIRIns::Module {
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
            PIRIns::Return { src: _, value } => match value {
                Some(v) => format!("return {};", v.as_str()),
                None => "return;".to_string(),
            },
            PIRIns::InfiniteLoop { src: _, body } => {
                format!("loop {}", body.as_str())
            }
            PIRIns::WhileLoop {
                src: _,
                condition,
                body,
            } => {
                format!("while {} {}", condition.as_str(), body.as_str())
            }
            PIRIns::Break(_) => "break;".to_string(),
            PIRIns::Continue(_) => "continue;".to_string(),
            PIRIns::UseDependency { paths, src: _ } => {
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
            PIRIns::DirectiveInstruction {
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
            PIRIns::ConditionalBranchIns { pairs, src: _ } => {
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
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct InsRef {
    pub loc: usize,
}

impl InsRef {
    pub fn as_str(&self) -> String {
        format!("InsRef({})", self.loc)
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct InsPool {
    pub pool: Vec<PIRIns>,
}

#[allow(dead_code)]
impl InsPool {
    pub fn new() -> InsPool {
        InsPool { pool: Vec::new() }
    }

    fn exists(&self, ins: &PIRIns) -> Option<InsRef> {
        for (i, ins2) in self.pool.iter().enumerate() {
            if ins.as_str() == ins2.as_str() {
                return Some(InsRef { loc: i });
            }
        }
        None
    }

    fn add(&mut self, ins: PIRIns) -> InsRef {
        self.pool.push(ins);
        InsRef {
            loc: self.pool.len() - 1,
        }
    }

    pub fn to_pir(&mut self, epool: &mut ExprPool, ins: Instruction) -> InsRef {
        match ins {
            Instruction::SingleLineComment { comment, src } => {
                let ins = PIRIns::SingleLineComment { comment, src };
                self.add(ins)
            }
            Instruction::NamedStructDecl { name, fields, src } => {
                let name_ref = epool.lowir(name);
                let fields = epool.pir_bindings(fields);
                let ins = PIRIns::NamedStructDecl {
                    name: name_ref,
                    fields,
                    src,
                };
                self.add(ins)
            }
            Instruction::ConstantDecl {
                const_name,
                const_type,
                init_expr,
                src_ref,
                is_public,
            } => {
                let init_expr = epool.lowir(init_expr);
                let ins = PIRIns::ConstantDecl {
                    const_name,
                    const_type,
                    init_expr,
                    src_ref,
                    is_public,
                };
                self.add(ins)
            }
            Instruction::VariableDecl(name, var_type, init_expr, span) => {
                let init_expr = init_expr.map(|e| epool.lowir(e));
                let ins = PIRIns::VariableDecl(name, var_type, init_expr, span);
                self.add(ins)
            }
            Instruction::AssignmentIns(dest, target) => {
                let dest = epool.lowir(dest);
                let target = epool.lowir(target);
                let ins = PIRIns::AssignmentIns(dest, target);
                self.add(ins)
            }
            Instruction::ExpressionIns(expr, semi) => {
                let expr = epool.lowir(expr);
                let ins = PIRIns::ExpressionIns(expr, semi);
                self.add(ins)
            }
            Instruction::FunctionDef {
                name,
                params,
                return_type,
                body,
                is_public,
                src,
            } => {
                let params = params.into_iter().map(|p| epool.lowir(p)).collect();
                let body = self.to_pir(epool, *body);
                let ins = PIRIns::FunctionDef {
                    name,
                    params,
                    return_type,
                    body,
                    is_public,
                    src,
                };
                self.add(ins)
            }
            Instruction::InfiniteLoop { src, body } => {
                let body = self.to_pir(epool, *body);
                let ins = PIRIns::InfiniteLoop { src, body };
                self.add(ins)
            }
            Instruction::WhileLoop {
                src,
                condition,
                body,
            } => {
                let condition = epool.lowir(condition);
                let body = self.to_pir(epool, *body);
                let ins = PIRIns::WhileLoop {
                    src,
                    condition,
                    body,
                };
                self.add(ins)
            }
            Instruction::CodeBlock { src, instructions } => {
                let instructions = instructions
                    .into_iter()
                    .map(|i| self.to_pir(epool, i))
                    .collect();
                let ins = PIRIns::CodeBlock { src, instructions };
                self.add(ins)
            }
            Instruction::Module {
                name,
                body,
                src,
                is_public,
            } => {
                let name = epool.lowir(name);
                let body = self.to_pir(epool, *body);
                let ins = PIRIns::Module {
                    name,
                    body,
                    src,
                    is_public,
                };
                self.add(ins)
            }
            Instruction::Return { src, value } => {
                let value = value.map(|v| epool.lowir(v));
                let ins = PIRIns::Return { src, value };
                self.add(ins)
            }
            Instruction::Break(src) => {
                let ins = PIRIns::Break(src);
                self.add(ins)
            }
            Instruction::Continue(src) => {
                let ins = PIRIns::Continue(src);
                self.add(ins)
            }
            Instruction::UseDependency { paths, src } => {
                let ins = PIRIns::UseDependency { paths, src };
                self.add(ins)
            }
            Instruction::DirectiveInstruction {
                directive,
                block,
                src,
            } => {
                let directive = epool.lowir(directive);
                let block = block.map(|b| self.to_pir(epool, *b));
                let ins = PIRIns::DirectiveInstruction {
                    directive,
                    block,
                    src,
                };
                self.add(ins)
            }
            Instruction::ConditionalBranchIns { pairs, src } => {
                let pairs = pairs
                    .into_iter()
                    .map(|(cond, body)| {
                        if let Some(cond) = cond {
                            let cond = epool.lowir(cond);
                            let body = self.to_pir(epool, *body);
                            (Some(cond), body)
                        } else {
                            let body = self.to_pir(epool, *body);
                            (None, body)
                        }
                    })
                    .collect();
                let ins = PIRIns::ConditionalBranchIns { pairs, src };
                self.add(ins)
            }
        }
    }

    pub fn get(&self, ins_ref: &InsRef) -> PIRIns {
        self.pool[ins_ref.loc].clone()
    }
}

#[derive(Debug, Clone)]
pub struct PIRModule {
    pub ins_pool: InsPool,
    pub expr_pool: ExprPool,
    pub top_level: Vec<InsRef>,
    pub path: String,
}

#[allow(dead_code)]
impl PIRModule {
    pub fn new(cm: CompilationModule, path: String) -> PIRModule {
        let mut pirmod = PIRModule {
            ins_pool: InsPool::new(),
            expr_pool: ExprPool::new(),
            top_level: Vec::new(),
            path,
        };
        for ins in cm.instructions {
            let id = pirmod.ins_pool.to_pir(&mut pirmod.expr_pool, ins);
            pirmod.top_level.push(id);
        }
        pirmod
    }
}

#[allow(dead_code)]
pub trait PIRModulePass<'a, InsRes, ExprRes, KVRes, ModRes, Error> {
    fn process_ins(&mut self, ins: &InsRef) -> Result<InsRes, Error>;
    fn process_expr(&mut self, expr: &ExprRef) -> Result<ExprRes, Error>;
    fn process_pairs(&mut self, kv: &KeyValueBindings) -> Result<KVRes, Error>;
    fn process_module(&mut self) -> Result<Vec<InsRes>, Error> {
        let mut res = vec![];

        let module = self.get_module();
        for ins_ref in module.top_level.iter() {
            let ins_res = self.process_ins(&ins_ref)?;
            res.push(ins_res);
        }

        Ok(res)
    }
    fn process(&mut self) -> Result<ModRes, Error>;
    fn new(module: &'a PIRModule) -> Self;
    fn get_module(&mut self) -> &'a PIRModule;
}
