use crate::frontend::{
    ast::{
        CompilationModule, DependencyPath, Expr, Instruction, KeyValueBindings as TreeKVBindings,
        SemanticType,
    },
    source::SourceRef,
    token::Token,
};

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct KeyValueBindings {
    pub pairs: Vec<(usize, Option<usize>)>,
    pub span: SourceRef,
}

#[allow(dead_code)]
impl KeyValueBindings {
    pub fn source_ref(&self) -> SourceRef {
        self.span.clone()
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum PIRExpr {
    Id(Token, Option<SemanticType>),
    Number(Token, Option<SemanticType>),
    StringLiteral(Token, Option<SemanticType>),
    CharacterLiteral(Token, Option<SemanticType>),
    Binary(Token, usize, usize, Option<SemanticType>),
    Comparison(Token, usize, usize, Option<SemanticType>),
    Boolean(Token, Option<SemanticType>),
    Unary(Token, usize, Option<SemanticType>),
    Grouped(usize, Option<SemanticType>, SourceRef),
    FnCall {
        func: usize,
        args: Vec<usize>,
        span: SourceRef,
        fn_type: Option<SemanticType>,
    },
    ScopeInto {
        module: usize,
        target: usize,
        src: SourceRef,
        resolved_type: Option<SemanticType>,
    },
    DirectiveExpr {
        directive: usize,
        expr: Option<usize>,
        resolved_type: Option<SemanticType>,
        src: SourceRef,
    },
    NamedStructInit {
        name: usize,
        fields: KeyValueBindings,
        src: SourceRef,
        resolved_type: Option<SemanticType>,
    },
}

#[allow(dead_code)]
impl PIRExpr {
    pub fn type_info(&self) -> Option<SemanticType> {
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
pub struct ExprPool {
    pub pool: Vec<PIRExpr>,
}

#[allow(dead_code)]
impl ExprPool {
    pub fn new() -> Self {
        ExprPool { pool: Vec::new() }
    }

    fn add(&mut self, expr: PIRExpr) -> usize {
        self.pool.push(expr);
        self.pool.len() - 1
    }

    pub fn to_pir(&mut self, expr: Expr) -> usize {
        match expr {
            Expr::Id(id, t) => self.add(PIRExpr::Id(id, t)),
            Expr::Number(n, t) => self.add(PIRExpr::Number(n, t)),
            Expr::StringLiteral(s, t) => self.add(PIRExpr::StringLiteral(s, t)),
            Expr::CharacterLiteral(c, t) => self.add(PIRExpr::CharacterLiteral(c, t)),
            Expr::Binary(op, lhs, rhs, t) => {
                let lhs = self.to_pir(*lhs);
                let rhs = self.to_pir(*rhs);
                self.add(PIRExpr::Binary(op, lhs, rhs, t))
            }
            Expr::Comparison(op, lhs, rhs, t) => {
                let lhs = self.to_pir(*lhs);
                let rhs = self.to_pir(*rhs);
                self.add(PIRExpr::Comparison(op, lhs, rhs, t))
            }
            Expr::Boolean(b, t) => self.add(PIRExpr::Boolean(b, t)),
            Expr::Unary(op, expr, t) => {
                let expr = self.to_pir(*expr);
                self.add(PIRExpr::Unary(op, expr, t))
            }
            Expr::Grouped(expr, t, src) => {
                let expr = self.to_pir(*expr);
                self.add(PIRExpr::Grouped(expr, t, src))
            }
            Expr::FnCall {
                func,
                args,
                span,
                fn_type,
            } => {
                let func = self.to_pir(*func);
                let args = args
                    .into_iter()
                    .map(|arg| self.to_pir(arg))
                    .collect::<Vec<usize>>();
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
                let module = self.to_pir(*module);
                let target = self.to_pir(*target);
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
                let directive = self.to_pir(*directive);
                let expr = expr.map(|expr| self.to_pir(*expr));
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
                let name = self.to_pir(*name);
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
                let key = self.to_pir(pair.0.clone());
                if let Some(value) = pair.1.clone() {
                    let value = self.to_pir(value);
                    (key, Some(value))
                } else {
                    (key, None)
                }
            })
            .collect::<Vec<(usize, Option<usize>)>>();
        KeyValueBindings {
            pairs: nbindings,
            span: bindings.span,
        }
    }

    pub fn get(&self, expr_ref: &usize) -> PIRExpr {
        self.pool[*expr_ref].clone()
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
        name: usize,
        fields: KeyValueBindings,
        src: SourceRef,
    },
    ConstantDecl {
        const_name: Token,
        const_type: Option<SemanticType>,
        init_expr: usize,
        src_ref: SourceRef,
        is_public: bool,
    },
    VariableDecl(Token, Option<SemanticType>, Option<usize>, SourceRef),
    AssignmentIns(usize, usize),
    ExpressionIns(usize, Token),
    FunctionPrototype {
        name: Token,
        params: Vec<usize>,
        return_type: SemanticType,
        is_public: bool,
        src: SourceRef,
    },
    FunctionDef {
        name: Token,
        params: Vec<usize>,
        return_type: SemanticType,
        body: usize,
        is_public: bool,
        src: SourceRef,
    },
    InfiniteLoop {
        src: SourceRef,
        body: usize,
    },
    WhileLoop {
        src: SourceRef,
        condition: usize,
        body: usize,
    },
    CodeBlock {
        src: SourceRef,
        instructions: Vec<usize>,
    },
    Module {
        name: usize,
        body: usize,
        src: SourceRef,
        is_public: bool,
    },
    Return {
        src: SourceRef,
        value: Option<usize>,
    },
    Break(SourceRef),
    Continue(SourceRef),
    UseDependency {
        paths: Vec<DependencyPath>,
        src: SourceRef,
    },
    DirectiveInstruction {
        directive: usize,
        block: Option<usize>,
        src: SourceRef,
    },
    ConditionalBranchIns {
        pairs: Vec<(Option<usize>, usize)>,
        src: SourceRef,
    },
    TypeExtension {
        target_type: SemanticType,
        extensions: usize,
        src: SourceRef,
    },
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

    fn add(&mut self, ins: PIRIns) -> usize {
        self.pool.push(ins);
        self.pool.len() - 1
    }

    pub fn to_pir(&mut self, epool: &mut ExprPool, ins: Instruction) -> usize {
        match ins {
            Instruction::SingleLineComment { comment, src } => {
                let ins = PIRIns::SingleLineComment { comment, src };
                self.add(ins)
            }
            Instruction::NamedStructDecl { name, fields, src } => {
                let name_ref = epool.to_pir(name);
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
                let init_expr = epool.to_pir(init_expr);
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
                let init_expr = init_expr.map(|e| epool.to_pir(e));
                let ins = PIRIns::VariableDecl(name, var_type, init_expr, span);
                self.add(ins)
            }
            Instruction::AssignmentIns(dest, target) => {
                let dest = epool.to_pir(dest);
                let target = epool.to_pir(target);
                let ins = PIRIns::AssignmentIns(dest, target);
                self.add(ins)
            }
            Instruction::ExpressionIns(expr, semi) => {
                let expr = epool.to_pir(expr);
                let ins = PIRIns::ExpressionIns(expr, semi);
                self.add(ins)
            }
            Instruction::FunctionPrototype {
                name,
                params,
                return_type,
                is_public,
                src,
            } => {
                let params = params.into_iter().map(|p| epool.to_pir(p)).collect();
                let ins = PIRIns::FunctionPrototype {
                    name,
                    params,
                    return_type,
                    is_public,
                    src,
                };
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
                let params = params.into_iter().map(|p| epool.to_pir(p)).collect();
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
                let condition = epool.to_pir(condition);
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
                let name = epool.to_pir(name);
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
                let value = value.map(|v| epool.to_pir(v));
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
                let directive = epool.to_pir(directive);
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
                            let cond = epool.to_pir(cond);
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
            Instruction::TypeExtension {
                target_type,
                extensions,
                src,
            } => {
                let extensions_ins = self.to_pir(epool, *extensions);
                let ins = PIRIns::TypeExtension {
                    target_type,
                    extensions: extensions_ins,
                    src,
                };
                self.add(ins)
            }
        }
    }

    pub fn get(&self, ins_ref: &usize) -> PIRIns {
        self.pool[*ins_ref].clone()
    }
}

#[derive(Debug, Clone)]
pub struct PIRModule {
    pub ins_pool: InsPool,
    pub expr_pool: ExprPool,
    pub top_level: Vec<usize>,
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
    fn process_ins(&mut self, ins: &usize) -> Result<InsRes, Error>;
    fn process_expr(&mut self, expr: &usize) -> Result<ExprRes, Error>;
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
