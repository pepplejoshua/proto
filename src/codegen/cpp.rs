#![allow(unused)]

use std::{collections::HashSet, path::Path, process::Command};

use crate::{
    source::source::SourceReporter,
    types::signature::{Sig, Type},
};

use super::tast::{TyExpr, TyFileModule, TyIns};

pub struct State {
    in_count: usize,
    gen_typedefs_for: HashSet<Sig>,
}

impl State {
    pub fn new() -> Self {
        State {
            in_count: 0,
            gen_typedefs_for: HashSet::new(),
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

pub fn cpp_gen_typedefs(state: &mut State) -> String {
    let mut buf = String::new();
    let mut includes: HashSet<String> = HashSet::new();
    for sig in state.gen_typedefs_for.iter() {
        match sig {
            Sig::I8 => {
                buf.push_str("\ntypedef int8_t i8;");
                includes.insert("#include <cstdint>".to_string());
            }
            Sig::I16 => {
                buf.push_str("\ntypedef int16_t i16;");
                includes.insert("#include <cstdint>".to_string());
            }
            Sig::I32 => {
                buf.push_str("\ntypedef int32_t i32;");
                includes.insert("#include <cstdint>".to_string());
            }
            Sig::I64 => {
                buf.push_str("\ntypedef int64_t i64;");
                includes.insert("#include <cstdint>".to_string());
            }
            Sig::U8 => {
                buf.push_str("\ntypedef uint8_t u8;");
                includes.insert("#include <cstdint>".to_string());
            }
            Sig::U16 => {
                buf.push_str("\ntypedef uint16_t u16;");
                includes.insert("#include <cstdint>".to_string());
            }
            Sig::U32 => {
                buf.push_str("\ntypedef uint32_t u32;");
                includes.insert("#include <cstdint>".to_string());
            }
            Sig::U64 => {
                buf.push_str("\ntypedef uint64_t u64;");
                includes.insert("#include <cstdint>".to_string());
            }
            Sig::UInt => {
                buf.push_str("\ntypedef uint32_t uint;");
                includes.insert("#include <cstdint>".to_string());
            }
            Sig::Str => {
                buf.push_str("\ntypedef std::string str;");
                includes.insert("#include <string>".to_string());
            }
            _ => unreachable!(),
        };
    }
    let mut header = String::new();
    if !includes.is_empty() {
        header = includes.into_iter().collect::<Vec<String>>().join("\n");
        header.push('\n');
        header.push_str(&buf);
        header.push('\n');
    }
    header
}

pub fn cpp_gen_ty(ty: &Type, state: &mut State) -> String {
    match ty.tag {
        Sig::Identifier => todo!(),
        Sig::Bool | Sig::Char | Sig::Void | Sig::Int => ty.as_str(),
        Sig::Str
        | Sig::I8
        | Sig::I16
        | Sig::I32
        | Sig::I64
        | Sig::U8
        | Sig::U16
        | Sig::U32
        | Sig::U64
        | Sig::UInt => {
            state.gen_typedefs_for.insert(ty.tag);
            ty.as_str()
        }
        Sig::StaticArray => todo!(),
        Sig::Function | Sig::ErrorType => {
            unreachable!(
                "cpp::cpp_gen_ty(): ran into a {:?} which should not occur.",
                ty.tag
            )
        }
    }
}

pub fn cpp_gen_expr(expr: &TyExpr, state: &mut State) -> String {
    match expr {
        TyExpr::Integer { .. }
        | TyExpr::Str { .. }
        | TyExpr::Char { .. }
        | TyExpr::Bool { .. }
        | TyExpr::Ident { .. }
        | TyExpr::UnaryOp { .. } => expr.as_str(),
        TyExpr::BinOp { op, lhs, rhs } => {
            format!(
                "{} {} {}",
                cpp_gen_expr(lhs, state),
                op.as_str(),
                cpp_gen_expr(rhs, state)
            )
        }
        TyExpr::CallFn { func, args } => {
            let args_str = args
                .iter()
                .map(|arg| cpp_gen_expr(arg, state))
                .collect::<Vec<String>>();
            let args_str = args_str.join(", ");
            format!("{}({args_str})", cpp_gen_expr(func, state))
        }
    }
}

pub fn cpp_gen_ins(ins: &TyIns, state: &mut State) -> String {
    let mut buf = String::new();
    match ins {
        TyIns::Constant { name, ty, init } => {
            buf = format!(
                "{}const {} {name} = {};",
                state.get_pad(),
                cpp_gen_ty(ty, state),
                cpp_gen_expr(init, state)
            );
        }
        TyIns::Var { name, ty, init } => {
            buf = if let Some(expr) = init {
                format!(
                    "{}{} {name} = {};",
                    state.get_pad(),
                    cpp_gen_ty(ty, state),
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
            let ret_ty_s = cpp_gen_ty(ret_ty, state);
            let params_s = params
                .iter()
                .map(|param| {
                    format!(
                        "{} {}",
                        cpp_gen_ty(&param.given_ty, state),
                        param.name.as_str()
                    )
                })
                .collect::<Vec<String>>();
            let params_s = params_s.join(", ");
            let body_s = cpp_gen_ins(body, state);
            buf = format!("{ret_ty_s} {name}({params_s})\n{body_s}\n");
        }
        TyIns::Block { code } => {
            buf = format!("{}{{\n", state.get_pad());
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

pub fn cpp_gen_top_level(file_mod: &TyFileModule) -> Result<String, String> {
    let mut state = State::new();
    let mut cpp_top_level_code = vec![];
    for tl_ins in file_mod.top_level.iter() {
        let ins_code = cpp_gen_ins(tl_ins, &mut state);
        cpp_top_level_code.push(ins_code);
        cpp_top_level_code.push("\n".to_string());
    }

    let header = cpp_gen_typedefs(&mut state);
    let cpp_code = cpp_top_level_code.join("");
    if !header.is_empty() {
        let cpp_code = format!("{header}\n{cpp_code}");
    }
    // println!("{cpp_code}");

    let og_file_path = Path::new(&file_mod.src_file);
    // println!("{og_file_path:?}");
    let file_path = og_file_path.with_extension("cpp");
    // println!("{file_path:?}");
    let exe_path = og_file_path.with_extension("");
    // println!("{exe_path:?}");
    std::fs::write(file_path.clone(), cpp_code).expect("Unable to write file");

    let mut clang_compile = Command::new("clang++")
        .args([
            file_path.to_str().unwrap(),
            "-o",
            exe_path.to_str().unwrap(),
            "-std=c++11",
        ])
        .output();
    if let Err(err) = clang_compile {
        return Err(err.to_string());
    }

    // let mut rm_cpp_file = Command::new("rm").arg(file_path.to_str().unwrap()).output();
    // if let Err(err) = rm_cpp_file {
    //     return Err(err.to_string());
    // }
    Ok(exe_path.to_str().unwrap().to_string())
}
