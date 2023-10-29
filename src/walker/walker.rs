#![allow(dead_code, unused_variables)]
use crate::frontend::ast::{CompilationModule, Expr, Instruction, TypeReference};

struct Sym {
    name: String,
    ty_ref: TypeReference,
}

struct Env {
    names: Vec<String>,
    types: Vec<TypeReference>,
    module: CompilationModule,
}

impl Env {
    pub fn new(module: CompilationModule) -> Self {
        Self {
            names: Vec::new(),
            types: Vec::new(),
            module,
        }
    }

    pub fn run(&mut self) {
        for ins in &self.module.instructions {
            run_ins(ins, self);
        }
    }
}

fn run_ins(ins: &Instruction, env: &Env) {
    match ins {
        Instruction::FunctionDef {
            name,
            params,
            return_type,
            body,
            is_public,
            src,
        } => {
            // add function to the environment
            // run the function
        }
        _ => {}
    }
}
fn run_expr(expr: &Expr, env: &Env) {}
