#![allow(unused)]

use std::collections::HashMap;

use crate::{
    parser::ast::{Expr, Ins},
    types::signature::{Sig, Type},
};

#[derive(Debug, Clone)]
struct TypeEnv {
    vars: HashMap<String, Type>,
}

impl TypeEnv {
    fn new() -> Self {
        TypeEnv {
            vars: HashMap::new(),
        }
    }

    fn extend(&self, var: String, ty: Type) -> Self {
        let mut new_env = self.clone();
        new_env.vars.insert(var, ty);
        new_env
    }

    fn lookup(&self, var: &str) -> Option<&Type> {
        self.vars.get(var)
    }
}

#[derive(Debug, Clone)]
pub struct Constraint {
    left: Type,
    right: Type,
}

impl Constraint {
    pub fn as_str(&self) -> String {
        format!("{} = {}", self.left.as_str(), self.right.as_str())
    }
}

#[derive(Debug, Clone)]
pub struct State {
    constraints: Vec<Constraint>,
    env: TypeEnv,
}

impl State {
    pub fn new() -> Self {
        State {
            constraints: vec![],
            env: TypeEnv::new(),
        }
    }
}

pub fn collect_constraints_from_ins(ins: &Ins, state: &mut State) -> Result<(), String> {
    todo!()
}

pub fn collect_constraints_from_expr(expr: &Expr, state: &mut State) -> Result<Type, String> {
    todo!()
}
