#![allow(unused)]

use crate::types::signature::{Sig, Type};

use super::tast::{TyExpr, TyFileModule, TyIns};

pub struct State {
    in_count: usize,
    gen_typedefs_for: Vec<Sig>,
}

impl State {
    pub fn new() -> Self {
        State {
            in_count: 0,
            gen_typedefs_for: vec![],
        }
    }

    pub fn indent(&mut self) {
        self.in_count += 4;
    }

    pub fn dedent(&mut self) {
        self.in_count -= 4;
    }

    pub fn get_pad(&self) -> String {
        " ".repeat(self.in_count)
    }
}

pub fn cpp_gen_ty(ty: &Type, state: &mut State) -> String {
    match ty.tag {
        Sig::Identifier => todo!(),
        Sig::Bool | Sig::Char | Sig::Void => ty.as_str(),
        Sig::Str => todo!(),
        Sig::I8 => todo!(),
        Sig::I16 => todo!(),
        Sig::I32 => todo!(),
        Sig::I64 => todo!(),
        Sig::Int => todo!(),
        Sig::U8 => todo!(),
        Sig::U16 => todo!(),
        Sig::U32 => todo!(),
        Sig::U64 => todo!(),
        Sig::UInt => todo!(),
        Sig::Function => todo!(),
        Sig::ErrorType => todo!(),
    }
}

pub fn cpp_gen_expr(expr: &TyExpr, state: &mut State) -> String {
    match expr {
        TyExpr::Integer { variant, val } => todo!(),
        TyExpr::Str { val } => todo!(),
        TyExpr::Char { val } => todo!(),
        TyExpr::Bool { val } => todo!(),
        TyExpr::Ident { name } => todo!(),
        TyExpr::BinOp { op, lhs, rhs } => todo!(),
        TyExpr::UnaryOp { op, expr } => todo!(),
        TyExpr::CallFn { func, args } => todo!(),
    }
}

pub fn cpp_gen_ins(ins: &TyIns, state: &mut State) -> String {
    let mut buf = String::new();
    match ins {
        TyIns::Constant { name, ty, init } => {
            buf = format!(
                "{}const {} {name} = {};",
                state.get_pad(),
                ty.as_str(),
                cpp_gen_expr(init, state)
            );
        }
        TyIns::Var { name, ty, init } => {
            buf = if let Some(expr) = init {
                format!(
                    "{}{} {name} = {};",
                    state.get_pad(),
                    ty.as_str(),
                    cpp_gen_expr(expr, state)
                )
            } else {
                format!("{}{} {name};", state.get_pad(), ty.as_str())
            };
        }
        TyIns::Func {
            name,
            params,
            ret_ty,
            body,
        } => {
            todo!();
        }
        TyIns::Block { code } => {
            buf = format!("{}{{", state.get_pad());
            state.indent();
            for ins in code {
                let mut ins_str = cpp_gen_ins(ins, state);
                ins_str.push('\n');
                buf.push_str(&ins_str);
            }
            state.dedent();
            buf.push_str(&format!("{}}}", state.get_pad()));
        }
        TyIns::ExprIns { expr } => {
            buf = format!("{}{};", state.get_pad(), cpp_gen_expr(expr, state));
        }
        TyIns::Return { expr } => {
            buf = if let Some(expr) = expr {
                format!("{}return {};", state.get_pad(), cpp_gen_expr(expr, state))
            } else {
                format!("{}return;", state.get_pad())
            };
        }
    }
    buf
}

pub fn cpp_gen_top_level(file_mod: &TyFileModule) {
    let mut state = State::new();
    let mut cpp_top_level_code = vec![];
    for tl_ins in file_mod.top_level.iter() {
        let ins_code = cpp_gen_ins(tl_ins, &mut state);
        cpp_top_level_code.push(ins_code);
        cpp_top_level_code.push("\n".to_string());
    }

    let cpp_code = cpp_top_level_code.join("");
    println!("{cpp_code}")
}
