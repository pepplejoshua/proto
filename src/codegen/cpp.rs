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
    uses_str_interpolation: bool,
    uses_print_ins: bool,
}

impl State {
    pub fn new() -> Self {
        State {
            in_count: 0,
            gen_typedefs_for: HashSet::new(),
            uses_str_interpolation: false,
            uses_print_ins: false,
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

const PANIC_FUNCTION: &str = include_str!("../std/panic.cppr");
const OPTION_CODE: &str = include_str!("../std/option.cppr");
const SLICE_AND_ARRAY_CODE: &str = include_str!("../std/slice_and_array.cppr");
const PROTO_STRINGIFY_CODE: &str = include_str!("../std/to_string.cppr");
const PROTO_PRINT_CODE: &str = include_str!("../std/print.cppr");

pub fn cpp_gen_call_stack_tracker(state: &mut State) -> String {
    todo!()
}

pub fn cpp_gen_typedefs(state: &mut State) -> String {
    let mut buf = String::new();
    let mut typedefs = String::new();
    let mut includes: HashSet<String> = HashSet::new();
    let mut has_option_class = false;
    let mut has_panic_fn = false;
    let mut has_array_class = false;

    if state.uses_str_interpolation {
        buf.push_str(PROTO_STRINGIFY_CODE);
    }

    for sig in state.gen_typedefs_for.iter() {
        match sig {
            Sig::I8 => {
                typedefs.push_str("\ntypedef int8_t i8;");
                includes.insert("#include <cstdint>".to_string());
            }
            Sig::I16 => {
                typedefs.push_str("\ntypedef int16_t i16;");
                includes.insert("#include <cstdint>".to_string());
            }
            Sig::I32 => {
                typedefs.push_str("\ntypedef int32_t i32;");
                includes.insert("#include <cstdint>".to_string());
            }
            Sig::I64 => {
                typedefs.push_str("\ntypedef int64_t i64;");
                includes.insert("#include <cstdint>".to_string());
            }
            Sig::U8 => {
                typedefs.push_str("\ntypedef uint8_t u8;");
                includes.insert("#include <cstdint>".to_string());
            }
            Sig::U16 => {
                typedefs.push_str("\ntypedef uint16_t u16;");
                includes.insert("#include <cstdint>".to_string());
            }
            Sig::U32 => {
                typedefs.push_str("\ntypedef uint32_t u32;");
                includes.insert("#include <cstdint>".to_string());
            }
            Sig::U64 => {
                typedefs.push_str("\ntypedef uint64_t u64;");
                includes.insert("#include <cstdint>".to_string());
            }
            Sig::UInt => {
                typedefs.push_str("\ntypedef uint64_t uint_pr;");
                includes.insert("#include <cstdint>".to_string());
            }
            Sig::Str => {
                typedefs.push_str("\ntypedef std::string str;");
                includes.insert("#include <string>".to_string());
            }
            Sig::StaticArray | Sig::Slice => {
                if !has_panic_fn {
                    if !state.gen_typedefs_for.contains(&Sig::Str) {
                        typedefs.push_str("\ntypedef std::string str;");
                        includes.insert("#include <string>".to_string());
                        // for exit and EXIT_FAILURE
                        includes.insert("#include <cstdlib>".to_string());
                        // for cout and endl
                        includes.insert("#include <iostream>".to_string());
                    }
                    buf.push_str(PANIC_FUNCTION.trim_end());
                    has_panic_fn = true;
                }

                if !has_option_class {
                    buf.push_str(OPTION_CODE.trim_end());
                    has_option_class = true;
                }

                if !has_array_class {
                    if !state.gen_typedefs_for.contains(&Sig::UInt) {
                        typedefs.push_str("\ntypedef uint64_t uint_pr;");
                        includes.insert("#include <cstdint>".to_string());
                    }
                    buf.push_str(SLICE_AND_ARRAY_CODE.trim_end());
                    has_array_class = true;
                }
            }
            Sig::Optional => {
                if !has_panic_fn {
                    if !state.gen_typedefs_for.contains(&Sig::Str) {
                        typedefs.push_str("\ntypedef std::string str;");
                        includes.insert("#include <string>".to_string());
                        // for exit and EXIT_FAILURE
                        includes.insert("#include <cstdlib>".to_string());
                        // for cout and endl
                        includes.insert("#include <iostream>".to_string());
                    }
                    buf.push_str(PANIC_FUNCTION);
                    has_panic_fn = true;
                }

                if !has_option_class {
                    buf.push_str(OPTION_CODE.trim_end());
                    has_option_class = true;
                }
            }
            _ => unreachable!(),
        };
    }

    if state.uses_print_ins {
        // provides cout, endl
        includes.insert("#include <iostream>".to_string());
        buf.push_str(PROTO_PRINT_CODE.trim_end());
    }

    let mut header = String::new();
    if !includes.is_empty() {
        header = includes.into_iter().collect::<Vec<String>>().join("\n");
        header.push('\n');
        header.push_str(&typedefs);
        if !buf.is_empty() {
            header.push('\n');
            header.push_str(&buf);
        }
        header.push('\n');
    }
    header
}

pub fn cpp_gen_ty(ty: &Type, state: &mut State) -> String {
    match ty.tag {
        Sig::UserDefinedType => todo!(),
        Sig::Bool | Sig::Char | Sig::Void | Sig::Int => ty.as_str(),
        Sig::Str
        | Sig::I8
        | Sig::I16
        | Sig::I32
        | Sig::I64
        | Sig::U8
        | Sig::U16
        | Sig::U32
        | Sig::U64 => {
            state.gen_typedefs_for.insert(ty.tag);
            ty.as_str()
        }
        Sig::UInt => {
            state.gen_typedefs_for.insert(ty.tag);
            "uint_pr".to_string()
        }
        Sig::StaticArray => {
            state.gen_typedefs_for.insert(ty.tag);
            let arr_ty = ty.aux_type.clone().unwrap();
            let arr_ty = cpp_gen_ty(&arr_ty, state);
            let arr_size = ty.sub_expr.clone().unwrap();
            format!("Array<{arr_ty}, {}>", arr_size.as_str())
        }
        Sig::Slice => {
            state.gen_typedefs_for.insert(ty.tag);
            let slice_ty = ty.aux_type.clone().unwrap();
            let slice_ty = cpp_gen_ty(&slice_ty, state);
            format!("Slice<{slice_ty}>")
        }
        Sig::Optional => {
            state.gen_typedefs_for.insert(ty.tag);
            let opt_ty = ty.aux_type.clone().unwrap();
            let opt_ty = cpp_gen_ty(&opt_ty, state);
            format!("Option<{opt_ty}>")
        }
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
        TyExpr::StaticArray { vals } => {
            let mut item_expr_strs = vec![];
            for item in vals.iter() {
                item_expr_strs.push(cpp_gen_expr(item, state));
            }
            format!(
                "{{{item_expr_strs}}}",
                item_expr_strs = item_expr_strs.join(", ")
            )
        }
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
        TyExpr::GroupedExpr { inner } => format!("({})", cpp_gen_expr(inner, state)),
        TyExpr::TernaryConditional {
            cond,
            then,
            otherwise,
        } => {
            format!(
                "{} ? {} : {}",
                cpp_gen_expr(cond, state),
                cpp_gen_expr(then, state),
                cpp_gen_expr(otherwise, state)
            )
        }
        TyExpr::InterpolatedString { parts } => {
            // allows us use #include <string> and std::to_string()
            state.gen_typedefs_for.insert(Sig::Str);
            state.uses_str_interpolation = true;
            let mut buf = parts
                .iter()
                .map(|p| cpp_gen_expr(p, state))
                .collect::<Vec<String>>()
                .join(" + ");
            buf
        }
        TyExpr::MakeSliceFrom { target, start } => {
            format!(
                "{}.make_slice_from({})",
                cpp_gen_expr(target, state),
                cpp_gen_expr(start, state)
            )
        }
        TyExpr::MakeSliceWithEnd {
            target,
            start,
            end_excl,
        } => {
            format!(
                "{}.make_slice({}, {})",
                cpp_gen_expr(target, state),
                cpp_gen_expr(start, state),
                cpp_gen_expr(end_excl, state)
            )
        }
        TyExpr::MultiExpr { exprs } => {
            format!(
                "({})",
                exprs
                    .iter()
                    .map(|e| { cpp_gen_expr(e, state) })
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
        TyExpr::IndexInto { target, index } => {
            format!(
                "{}[{}]",
                cpp_gen_expr(target, state),
                cpp_gen_expr(index, state)
            )
        }
        TyExpr::AccessMember { target, mem } => {
            format!(
                "{}.{}",
                cpp_gen_expr(target, state),
                cpp_gen_expr(mem, state)
            )
        }
        TyExpr::OptionalExpr { val } => {
            if let Some(v) = val {
                format!("{{{}}}", cpp_gen_expr(v, state))
            } else {
                format!("{{}}")
            }
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
            let body_s = if !matches!(**body, TyIns::Block { code: _ }) {
                format!(" {{ {} }}", cpp_gen_ins(body, state))
            } else {
                format!("\n{}", cpp_gen_ins(body, state))
            };
            buf = format!("{ret_ty_s} {name}({params_s}){body_s}\n");
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
        TyIns::IfConditional { comb } => {
            for (i, (cond, code)) in comb.iter().enumerate() {
                if i == 0 {
                    buf.push_str(&format!(
                        "{}if ({})\n{}",
                        state.get_pad(),
                        cpp_gen_expr(cond.as_ref().unwrap(), state),
                        cpp_gen_ins(code, state)
                    ));
                    continue;
                }

                match cond {
                    Some(else_if_cond) => {
                        buf.push_str(&format!(
                            " else if ({})\n{}",
                            cpp_gen_expr(else_if_cond, state),
                            cpp_gen_ins(code, state)
                        ));
                    }
                    None => {
                        buf.push_str(&format!(" else\n{}", cpp_gen_ins(code, state)));
                    }
                }
            }
        }
        TyIns::PrintIns { is_println, output } => {
            state.uses_print_ins = true;
            buf = format!(
                "{}{}({});",
                state.get_pad(),
                if *is_println {
                    "proto_println"
                } else {
                    "proto_print"
                },
                cpp_gen_expr(output, state)
            )
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
    let mut cpp_code = cpp_top_level_code.join("");
    if !header.is_empty() {
        cpp_code = format!("{header}\n{cpp_code}");
    }
    // println!("{cpp_code}");

    let og_file_path = Path::new(&file_mod.src_file);
    // println!("{og_file_path:?}");
    let file_path = og_file_path.with_extension("cpp");
    // println!("{file_path:?}");
    let exe_path = og_file_path.with_extension("");
    // println!("{exe_path:?}");
    std::fs::write(file_path.clone(), cpp_code.trim()).expect("Unable to write file");

    let mut clang_compile = Command::new("clang++")
        .args([
            file_path.to_str().unwrap(),
            "-o",
            exe_path.to_str().unwrap(),
            "-std=c++11",
        ])
        .output();

    match clang_compile {
        Ok(clang_out) => {
            if !clang_out.stderr.is_empty() {
                println!(
                    "clang++: {err}",
                    err = String::from_utf8(clang_out.stderr).unwrap(),
                )
            }
        }
        Err(_) => return Err("fatal error trying to compile {og_file_path}".to_owned()),
    }
    // let mut rm_cpp_file = Command::new("rm").arg(file_path.to_str().unwrap()).output();
    // if let Err(err) = rm_cpp_file {
    //     return Err(err.to_string());
    // }
    Ok(exe_path.to_str().unwrap().to_string())
}
