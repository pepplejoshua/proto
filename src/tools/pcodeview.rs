use crate::pir::ir::{KeyValueBindings, PIRExpr, PIRIns, PIRModule, PIRModulePass};

#[derive(Clone)]
#[allow(dead_code)]
enum CodeBlockType {
    Regular,
    InstructionPart,
}

#[allow(dead_code)]
pub struct PCodeView<'a> {
    module: &'a PIRModule,
    left_padding: u16,
    block_type: CodeBlockType,
}

#[allow(dead_code)]
impl<'a> PCodeView<'a> {
    pub fn increase_padding(&mut self) {
        self.left_padding += 4;
    }

    pub fn decrease_padding(&mut self) {
        self.left_padding -= 4;
    }

    pub fn pad_text(&self, text: String) -> String {
        let padding = " ".repeat(self.left_padding as usize);
        format!("{}{}", padding, text)
    }
}

#[allow(dead_code)]
impl<'a> PIRModulePass<'a, String, String, String, String, ()> for PCodeView<'a> {
    fn process_ins(&mut self, ins: &usize) -> Result<String, ()> {
        // get instruction from pool
        let module = self.module;
        let ins_node = module.ins_pool.get(&ins);

        match ins_node {
            PIRIns::SingleLineComment { comment, src: _ } => {
                let comment = comment.clone();
                Ok(self.pad_text(comment))
            }
            PIRIns::NamedStructDecl {
                name,
                fields,
                src: _,
            } => {
                let name = self.process_expr(&name)?;
                let mut view = String::new();
                view.push_str(&self.pad_text(format!(":{} ", name)));
                if fields.pairs.len() > 0 {
                    view.push_str(&self.process_pairs(&fields)?);
                } else {
                    view.push_str("{ }");
                }
                Ok(view)
            }
            PIRIns::ConstantDecl {
                const_name,
                const_type,
                init_expr,
                src_ref: _,
                is_public,
            } => {
                let const_name = const_name.as_str();
                let init_expr = self.process_expr(&init_expr)?;
                let mut const_type_ref = "".to_string();
                if let Some(type_ref) = const_type {
                    const_type_ref = type_ref.as_str();
                }

                let mut view = format!("let {const_name} {const_type_ref} = {init_expr};");
                if is_public {
                    view = format!("pub {}", view);
                }
                Ok(self.pad_text(view))
            }
            PIRIns::VariableDecl(var_name, type_ref, init_expr, _) => {
                let var_name = var_name.as_str();
                let mut init_str = "".to_string();
                if let Some(init) = init_expr {
                    init_str = self.process_expr(&init)?;
                }

                let mut var_type = "".to_string();
                if let Some(type_ref) = type_ref {
                    var_type = type_ref.as_str();
                }

                let mut output = format!("mut {var_name}");
                if !var_type.is_empty() {
                    output.push_str(&format!(" {}", var_type));
                }

                if !init_str.is_empty() {
                    output.push_str(&format!(" = {}", init_str));
                }

                output.push_str(";");

                Ok(self.pad_text(output))
            }
            PIRIns::AssignmentIns(target, val, _) => {
                let target = self.process_expr(&target)?;
                let val = self.process_expr(&val)?;
                Ok(self.pad_text(format!("{} = {};", target, val)))
            }
            PIRIns::ExpressionIns(expr, _) => {
                let expr = self.process_expr(&expr)?;
                Ok(self.pad_text(format!("{};", expr)))
            }
            PIRIns::FunctionPrototype {
                name,
                params,
                return_type,
                is_public,
                defined_as_primitive: _,
                src: _,
            } => {
                let name = name.as_str();
                let mut param_strs = vec![];
                for param in params {
                    param_strs.push(self.process_expr(&param)?);
                }
                let params = param_strs.join(", ");
                let return_type = return_type.as_str();
                let mut view = format!(
                    "fn {name}({params}) {return_type};",
                    name = name,
                    params = params,
                    return_type = return_type
                );
                if is_public {
                    view = format!("pub {}", view);
                }
                Ok(self.pad_text(view))
            }
            PIRIns::FunctionDef {
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
                    param_strs.push(self.process_expr(&param)?);
                }
                let params = param_strs.join(", ");
                let return_type = return_type.as_str();
                let block_type = self.block_type.clone();
                self.block_type = CodeBlockType::InstructionPart;
                let body = self.process_ins(&body)?;
                self.block_type = block_type;
                let mut view = format!("fn {name}({params}) {return_type} {body}");
                if is_public {
                    view = format!("pub {}", view);
                }
                Ok(self.pad_text(view))
            }
            PIRIns::InfiniteLoop { src: _, body } => {
                let block_type = self.block_type.clone();
                self.block_type = CodeBlockType::InstructionPart;
                let body = self.process_ins(&body)?;
                self.block_type = block_type;
                Ok(self.pad_text(format!("loop {}", body)))
            }
            PIRIns::WhileLoop {
                src: _,
                condition,
                body,
            } => {
                let condition = self.process_expr(&condition)?;
                let block_type = self.block_type.clone();
                self.block_type = CodeBlockType::InstructionPart;
                let body = self.process_ins(&body)?;
                self.block_type = block_type;
                Ok(self.pad_text(format!("while {} {}", condition, body)))
            }
            PIRIns::CodeBlock {
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
                        ins_strs.push(self.process_ins(&ins)?);
                    }
                    self.decrease_padding();
                    view.push_str(&ins_strs.join("\n"));
                    view.push_str(&("\n".to_string() + &self.pad_text("}".to_string())));
                }
                Ok(view)
            }
            PIRIns::Module {
                name,
                body,
                src: _,
                is_public,
            } => {
                let name = self.process_expr(&name)?;
                let block_type = self.block_type.clone();
                self.block_type = CodeBlockType::InstructionPart;
                let body = self.process_ins(&body)?;
                self.block_type = block_type;
                let mut view = format!("mod {} {}", name, body);
                if is_public {
                    view = format!("pub {}", view);
                }
                Ok(self.pad_text(view))
            }
            PIRIns::Return { src: _, value } => {
                if let Some(value) = value {
                    let value = self.process_expr(&value)?;
                    Ok(self.pad_text(format!("return {};", value)))
                } else {
                    Ok(self.pad_text("return;".to_string()))
                }
            }
            PIRIns::Break(_) => Ok(self.pad_text("break;".to_string())),
            PIRIns::Continue(_) => Ok(self.pad_text("continue;".to_string())),
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
                Ok(self.pad_text(format!("use {};", path_str)))
            }
            PIRIns::DirectiveInstruction {
                directive,
                block,
                src: _,
            } => {
                let directive = self.process_expr(&directive)?;
                if let Some(block) = block {
                    let block_type = self.block_type.clone();
                    self.block_type = CodeBlockType::InstructionPart;
                    let block = self.process_ins(&block)?;
                    self.block_type = block_type;
                    Ok(self.pad_text(format!("@{} {}", directive, block)))
                } else {
                    Ok(self.pad_text(format!("@{};", directive)))
                }
            }
            PIRIns::ConditionalBranchIns { pairs, src: _ } => {
                let mut view = String::new();
                let block_type = self.block_type.clone();
                self.block_type = CodeBlockType::InstructionPart;
                for i in 0..pairs.len() {
                    let (cond, ins) = &pairs[i];
                    if i == 0 {
                        let cond = self.process_expr(&cond.clone().unwrap())?;
                        let ins = self.process_ins(&ins)?;
                        // self.increase_padding();
                        // for the first pair, we do if <cond> <ins>
                        view.push_str(&self.pad_text(format!("if {} {}", cond, ins)));
                        // self.decrease_padding();
                    } else if i < pairs.len() - 1 {
                        let cond = self.process_expr(&cond.clone().unwrap())?;
                        let ins = self.process_ins(&ins)?;
                        self.increase_padding();
                        // for the rest, we do else if <cond> <ins>
                        view.push_str(&format!(" else if {} {}", cond, ins));
                        self.decrease_padding();
                    } else {
                        // for the last, we do else <ins>
                        let ins = self.process_ins(&ins)?;
                        self.increase_padding();
                        view.push_str(&format!(" else {}", ins));
                        self.decrease_padding();
                    }
                }
                self.block_type = block_type;
                Ok(view)
            }
            PIRIns::TypeExtension {
                target_type,
                extensions,
                src: _,
            } => {
                let target_type = target_type.as_str();
                let block_type = self.block_type.clone();
                self.block_type = CodeBlockType::InstructionPart;
                let ext_str = self.process_ins(&extensions)?;
                self.block_type = block_type;
                let view = format!("extend {target_type} {ext_str}");
                Ok(view)
            }
        }
    }

    fn process_expr(&mut self, expr: &usize) -> Result<String, ()> {
        // get expr from pool
        let module = self.module;
        let expr_node = module.expr_pool.get(&expr);

        match expr_node {
            PIRExpr::Id(token, id_type, _) => {
                if let Some(id_type) = id_type {
                    Ok(format!("{} {}", token.as_str(), id_type.as_str()))
                } else {
                    Ok(token.as_str())
                }
            }
            PIRExpr::Number(num, _, _) => Ok(num.as_str()),
            PIRExpr::StringLiteral(lit, _, _) => Ok(lit.as_str()),
            PIRExpr::CharacterLiteral(lit, _, _) => Ok(lit.as_str()),
            PIRExpr::Binary(operator, lhs, rhs, _, _) => {
                let lhs = self.process_expr(&lhs)?;
                let rhs = self.process_expr(&rhs)?;
                Ok(format!("{} {} {}", lhs, operator.as_str(), rhs))
            }
            PIRExpr::Comparison(operator, lhs, rhs, _, _) => {
                let lhs = self.process_expr(&lhs)?;
                let rhs = self.process_expr(&rhs)?;
                Ok(format!("{} {} {}", lhs, operator.as_str(), rhs))
            }
            PIRExpr::Boolean(lit, _, _) => Ok(lit.as_str()),
            PIRExpr::Unary(operator, expr, _, _) => {
                let expr = self.process_expr(&expr)?;
                Ok(format!("{}{}", operator.as_str(), expr))
            }
            PIRExpr::Grouped(inner_expr, _, _) => {
                let inner_expr = self.process_expr(&inner_expr)?;
                Ok(format!("({})", inner_expr))
            }
            PIRExpr::FnCall {
                func,
                args,
                span: _,
                fn_type: _,
            } => {
                let func = self.process_expr(&func)?;
                let args_str = args
                    .iter()
                    .map(|arg| self.process_expr(&arg))
                    .collect::<Result<Vec<String>, ()>>()?;
                let args_str = args_str.join(", ");
                Ok(format!("{}({})", func, args_str))
            }
            PIRExpr::ScopeInto {
                module,
                target,
                src: _,
                resolved_type: _,
            } => {
                let module = self.process_expr(&module)?;
                let target = self.process_expr(&target)?;
                Ok(format!("{}::{}", module, target))
            }
            PIRExpr::DirectiveExpr {
                directive,
                expr,
                resolved_type: _,
                src: _,
            } => {
                let directive = self.process_expr(&directive)?;
                if let Some(expr) = expr {
                    let expr = self.process_expr(&expr)?;
                    Ok(format!("@{} {}", directive, expr))
                } else {
                    Ok(format!("@{}", directive))
                }
            }
            PIRExpr::NamedStructInit {
                name,
                fields,
                src: _,
                resolved_type: _,
            } => {
                let name = self.process_expr(&name)?;
                let block_type = self.block_type.clone();
                self.block_type = CodeBlockType::InstructionPart;
                let fields = self.process_pairs(&fields)?;
                self.block_type = block_type;
                Ok(format!(":{} {}", name, fields))
            }
        }
    }

    fn process_pairs(&mut self, kv: &KeyValueBindings) -> Result<String, ()> {
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
            let key = self.process_expr(&key)?;
            if let Some(value) = value {
                let value = self.process_expr(&value)?;
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

    fn process(&mut self) -> Result<String, ()> {
        let content = self.process_module()?;
        Ok(content.join("\n"))
    }

    fn new(module: &'a PIRModule) -> Self {
        Self {
            left_padding: 0,
            module,
            block_type: CodeBlockType::Regular,
        }
    }

    fn get_module(&mut self) -> &'a PIRModule {
        self.module
    }
}
