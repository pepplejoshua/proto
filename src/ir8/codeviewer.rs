use super::lowir::{ExprPool, ExprRef, InsPool, InsRef, KeyValueBindings, LowIRExpr, LowIRIns};
use super::visitir::{VisitIRError, VisitsLowIR};

#[derive(Clone)]
#[allow(dead_code)]
enum CodeBlockType {
    Regular,
    InstructionPart,
}

#[allow(dead_code)]
pub struct CodeViewer {
    ins_pool: InsPool,
    expr_pool: ExprPool,
    left_padding: u16,
    block_type: CodeBlockType,
}

#[allow(dead_code)]
impl CodeViewer {
    pub fn new(ins_pool: InsPool, expr_pool: ExprPool) -> Self {
        Self {
            left_padding: 0,
            ins_pool,
            expr_pool,
            block_type: CodeBlockType::Regular,
        }
    }

    pub fn unwrap(&mut self, res: Vec<Result<String, VisitIRError>>) -> Vec<String> {
        let mut result = vec![];
        for r in res {
            match r {
                Ok(s) => result.push(s.clone()),
                Err(_) => {}
            }
        }
        result
    }

    pub fn increase_padding(&mut self) {
        self.left_padding += 2;
    }

    pub fn decrease_padding(&mut self) {
        self.left_padding -= 2;
    }

    pub fn pad_text(&self, text: String) -> String {
        let padding = " ".repeat(self.left_padding as usize);
        format!("{}{}", padding, text)
    }

    pub fn get_ins_pool(self) -> InsPool {
        self.ins_pool
    }

    pub fn get_expr_pool(self) -> ExprPool {
        self.expr_pool
    }
}

impl VisitsLowIR<String, String, String> for CodeViewer {
    fn visit_pairs(&mut self, kv: &KeyValueBindings) -> Result<String, VisitIRError> {
        let mut view = String::from("{");
        let kv_pair_len = kv.pairs.len();
        if kv_pair_len > 0 {
            self.increase_padding();
        } else {
            view.push_str(" }");
            return Ok(view);
        }

        let mut count = 0;
        for (key, value) in &kv.pairs {
            let key = self.visit_expr(&key)?;
            if let Some(value) = value {
                let value = self.visit_expr(&value)?;
                view.push('\n');
                view.push_str(&self.pad_text(format!("{} = {}", key, value)));
            } else {
                view.push('\n');
                view.push_str(&self.pad_text(format!("{}", key)));
            }
            count += 1;
            if count < kv_pair_len {
                view.push(',');
            }
        }
        self.decrease_padding();
        view.push('\n');
        view.push_str(&self.pad_text("}".to_string()));
        Ok(view)
    }

    fn visit_ins(&mut self, ins: &InsRef) -> Result<String, VisitIRError> {
        // get instruction from pool
        let ins_node = self.ins_pool.get(&ins);

        match ins_node {
            LowIRIns::NamedStructDecl {
                name,
                fields,
                src: _,
            } => {
                let name = self.visit_expr(&name)?;
                let mut view = String::new();
                view.push_str(&self.pad_text(format!(":{} ", name)));
                if fields.pairs.len() > 0 {
                    view.push_str(&self.visit_pairs(&fields)?);
                } else {
                    view.push_str("{ }");
                }
                Ok(view)
            }
            LowIRIns::ConstantDecl {
                const_name,
                const_type: _,
                init_expr,
                src_ref: _,
                is_public,
            } => {
                let const_name = const_name.as_str();
                let init_expr = self.visit_expr(&init_expr)?;
                let mut view = format!("let {} = {}", const_name, init_expr);
                if is_public {
                    view = format!("pub {}", view);
                }
                Ok(self.pad_text(view))
            }
            LowIRIns::VariableDecl(var_name, _, init_expr, _) => {
                let var_name = var_name.as_str();
                if let Some(init_expr) = init_expr {
                    let init_expr = self.visit_expr(&init_expr)?;
                    Ok(self.pad_text(format!("mut {} = {}", var_name, init_expr)))
                } else {
                    Ok(self.pad_text(format!("mut {}", var_name)))
                }
            }
            LowIRIns::AssignmentIns(target, val) => {
                let target = self.visit_expr(&target)?;
                let val = self.visit_expr(&val)?;
                Ok(self.pad_text(format!("{} = {};", target, val)))
            }
            LowIRIns::ExpressionIns(expr, _) => {
                let expr = self.visit_expr(&expr)?;
                Ok(self.pad_text(format!("{};", expr)))
            }
            LowIRIns::FunctionDef {
                name,
                params,
                return_type,
                body,
                is_public,
                src: _,
            } => {
                let name = name.as_str();
                let mut param_strs = vec![];
                for param in params {
                    param_strs.push(self.visit_expr(&param)?);
                }
                let params = param_strs.join(", ");
                let return_type = return_type.as_str();
                let block_type = self.block_type.clone();
                self.block_type = CodeBlockType::InstructionPart;
                let body = self.visit_ins(&body)?;
                self.block_type = block_type;
                let mut view = format!("fn {name}({params}) {return_type} {body}");
                if is_public {
                    view = format!("pub {}", view);
                }
                Ok(self.pad_text(view))
            }
            LowIRIns::InfiniteLoop { src: _, body } => {
                let block_type = self.block_type.clone();
                self.block_type = CodeBlockType::InstructionPart;
                let body = self.visit_ins(&body)?;
                self.block_type = block_type;
                Ok(self.pad_text(format!("loop {}", body)))
            }
            LowIRIns::WhileLoop {
                src: _,
                condition,
                body,
            } => {
                let condition = self.visit_expr(&condition)?;
                let block_type = self.block_type.clone();
                self.block_type = CodeBlockType::InstructionPart;
                let body = self.visit_ins(&body)?;
                self.block_type = block_type;
                Ok(self.pad_text(format!("while {} {}", condition, body)))
            }
            LowIRIns::CodeBlock {
                src: _,
                instructions,
            } => {
                let mut view = String::new();
                match self.block_type {
                    CodeBlockType::InstructionPart => {
                        view.push_str("{");
                    }
                    CodeBlockType::Regular => {
                        view.push_str(&self.pad_text("{\n".to_string()));
                    }
                }

                if instructions.len() == 0 {
                    view.push_str(" }");
                } else {
                    view.push('\n');
                    self.increase_padding();
                    let mut ins_strs = vec![];
                    for ins in instructions {
                        ins_strs.push(self.visit_ins(&ins)?);
                    }
                    self.decrease_padding();
                    view.push_str(&ins_strs.join("\n\n"));
                    view.push_str(&("\n".to_string() + &self.pad_text("}".to_string())));
                }
                Ok(view)
            }
            LowIRIns::Module {
                name,
                body,
                src: _,
                is_public,
            } => {
                let name = self.visit_expr(&name)?;
                let block_type = self.block_type.clone();
                self.block_type = CodeBlockType::InstructionPart;
                let body = self.visit_ins(&body)?;
                self.block_type = block_type;
                let mut view = format!("mod {} {}", name, body);
                if is_public {
                    view = format!("pub {}", view);
                }
                Ok(self.pad_text(view))
            }
            LowIRIns::Return { src: _, value } => {
                if let Some(value) = value {
                    let value = self.visit_expr(&value)?;
                    Ok(self.pad_text(format!("return {};", value)))
                } else {
                    Ok(self.pad_text("return;".to_string()))
                }
            }
            LowIRIns::Break(_) => Ok(self.pad_text("break;".to_string())),
            LowIRIns::Continue(_) => Ok(self.pad_text("continue;".to_string())),
            LowIRIns::UseDependency { paths, src: _ } => {
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
                Ok(format!("use {};", path_str))
            }
            LowIRIns::DirectiveInstruction {
                directive,
                block,
                src: _,
            } => {
                let directive = self.visit_expr(&directive)?;
                if let Some(block) = block {
                    let block_type = self.block_type.clone();
                    self.block_type = CodeBlockType::InstructionPart;
                    let block = self.visit_ins(&block)?;
                    self.block_type = block_type;
                    Ok(self.pad_text(format!("@{} {}", directive, block)))
                } else {
                    Ok(self.pad_text(format!("@{};", directive)))
                }
            }
            LowIRIns::ConditionalBranchIns { pairs, src: _ } => {
                let mut view = String::new();
                let block_type = self.block_type.clone();
                self.block_type = CodeBlockType::InstructionPart;
                for i in 0..pairs.len() {
                    let (cond, ins) = &pairs[i];
                    if i == 0 {
                        let cond = self.visit_expr(&cond.clone().unwrap())?;
                        let ins = self.visit_ins(&ins)?;
                        // self.increase_padding();
                        // for the first pair, we do if <cond> <ins>
                        view.push_str(&self.pad_text(format!("if {} {}", cond, ins)));
                        // self.decrease_padding();
                    } else if i < pairs.len() - 1 {
                        let cond = self.visit_expr(&cond.clone().unwrap())?;
                        let ins = self.visit_ins(&ins)?;
                        self.increase_padding();
                        // for the rest, we do else if <cond> <ins>
                        view.push_str(&format!(" else if {} {}", cond, ins));
                        self.decrease_padding();
                    } else {
                        // for the last, we do else <ins>
                        let ins = self.visit_ins(&ins)?;
                        self.increase_padding();
                        view.push_str(&format!(" else {}", ins));
                        self.decrease_padding();
                    }
                }
                self.block_type = block_type;
                Ok(view)
            }
        }
    }

    fn visit_expr(&mut self, expr: &ExprRef) -> Result<String, VisitIRError> {
        // get expr from pool
        let expr_node = self.expr_pool.get(&expr);

        match expr_node {
            LowIRExpr::Id(token, id_type) => {
                if let Some(id_type) = id_type {
                    Ok(format!("{} {}", token.as_str(), id_type.as_str()))
                } else {
                    Ok(token.as_str())
                }
            }
            LowIRExpr::Number(num, _) => Ok(num.as_str()),
            LowIRExpr::StringLiteral(lit, _) => Ok(lit.as_str()),
            LowIRExpr::CharacterLiteral(lit, _) => Ok(lit.as_str()),
            LowIRExpr::Binary(operator, lhs, rhs, _) => {
                let lhs = self.visit_expr(&lhs)?;
                let rhs = self.visit_expr(&rhs)?;
                Ok(format!("{} {} {}", lhs, operator.as_str(), rhs))
            }
            LowIRExpr::Comparison(operator, lhs, rhs, _) => {
                let lhs = self.visit_expr(&lhs)?;
                let rhs = self.visit_expr(&rhs)?;
                Ok(format!("{} {} {}", lhs, operator.as_str(), rhs))
            }
            LowIRExpr::Boolean(lit, _) => Ok(lit.as_str()),
            LowIRExpr::Unary(operator, expr, _) => {
                let expr = self.visit_expr(&expr)?;
                Ok(format!("{}{}", operator.as_str(), expr))
            }
            LowIRExpr::Grouped(inner_expr, _, _) => {
                let inner_expr = self.visit_expr(&inner_expr)?;
                Ok(format!("({})", inner_expr))
            }
            LowIRExpr::FnCall {
                func,
                args,
                span: _,
                fn_type: _,
            } => {
                let func = self.visit_expr(&func)?;
                let args_str = args
                    .iter()
                    .map(|arg| self.visit_expr(&arg))
                    .collect::<Result<Vec<String>, VisitIRError>>()?;
                let args_str = args_str.join(", ");
                Ok(format!("{}({})", func, args_str))
            }
            LowIRExpr::ScopeInto {
                module,
                target,
                src: _,
                resolved_type: _,
            } => {
                let module = self.visit_expr(&module)?;
                let target = self.visit_expr(&target)?;
                Ok(format!("{}::{}", module, target))
            }
            LowIRExpr::DirectiveExpr {
                directive,
                expr,
                resolved_type: _,
                src: _,
            } => {
                let directive = self.visit_expr(&directive)?;
                if let Some(expr) = expr {
                    let expr = self.visit_expr(&expr)?;
                    Ok(format!("@{} {}", directive, expr))
                } else {
                    Ok(format!("@{}", directive))
                }
            }
            LowIRExpr::NamedStructInit {
                name,
                fields,
                src: _,
                resolved_type: _,
            } => {
                let name = self.visit_expr(&name)?;
                let block_type = self.block_type.clone();
                self.block_type = CodeBlockType::InstructionPart;
                let fields = self.visit_pairs(&fields)?;
                self.block_type = block_type;
                Ok(format!(":{} {}", name, fields))
            }
        }
    }
}
