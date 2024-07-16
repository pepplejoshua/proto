#![allow(unused)]
use std::{collections::HashSet, path::Path, process::Command};

use crate::types::signature::Ty;

use super::tast::{TyExpr, TyFileModule, TyIns};

pub struct State {
    in_count: usize,
    gen_typedefs_for: HashSet<String>,
    uses_str_interpolation: bool,
    uses_print_ins: bool,
    uses_defer: bool,
}

impl State {
    pub fn new() -> Self {
        State {
            in_count: 0,
            gen_typedefs_for: HashSet::new(),
            uses_str_interpolation: false,
            uses_print_ins: false,
            uses_defer: false,
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
const HMAP: &str = include_str!("../std/hmap.cppr");
const PROTO_STRINGIFY_CODE: &str = include_str!("../std/to_string.cppr");
const PROTO_PRINT_CODE: &str = include_str!("../std/print.cppr");
const PROTO_DEFER_CODE: &str = include_str!("../std/defer.cppr");

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

    if state.uses_defer {
        buf.push_str(PROTO_DEFER_CODE);
    }

    if state.uses_print_ins {
        state.gen_typedefs_for.insert("str".into());
    }

    for sig in state.gen_typedefs_for.iter() {
        match sig.as_str() {
            "func" => {
                typedefs.push_str("\nusing std::function;");
                includes.insert("#include <functional>".to_string());
            }
            "i8" => {
                typedefs.push_str("\ntypedef int8_t i8;");
                includes.insert("#include <cstdint>".to_string());
            }
            "i16" => {
                typedefs.push_str("\ntypedef int16_t i16;");
                includes.insert("#include <cstdint>".to_string());
            }
            "i32" => {
                typedefs.push_str("\ntypedef int32_t i32;");
                includes.insert("#include <cstdint>".to_string());
            }
            "i64" => {
                typedefs.push_str("\ntypedef int64_t i64;");
                includes.insert("#include <cstdint>".to_string());
            }
            "u8" => {
                typedefs.push_str("\ntypedef uint8_t u8;");
                includes.insert("#include <cstdint>".to_string());
            }
            "u16" => {
                typedefs.push_str("\ntypedef uint16_t u16;");
                includes.insert("#include <cstdint>".to_string());
            }
            "u32" => {
                typedefs.push_str("\ntypedef uint32_t u32;");
                includes.insert("#include <cstdint>".to_string());
            }
            "u64" => {
                typedefs.push_str("\ntypedef uint64_t u64;");
                includes.insert("#include <cstdint>".to_string());
            }
            "uint" => {
                let plat_size = Ty::get_platform_size();
                includes.insert("#include <cstdint>".to_string());
                if plat_size == 64 {
                    typedefs.push_str("\ntypedef uint64_t uint_pr;");
                } else {
                    typedefs.push_str("\ntypedef uint32_t uint_pr;");
                }
            }
            "str" => {
                typedefs.push_str("\ntypedef std::string str;");
                includes.insert("#include <string>".to_string());
            }
            "hmap" => {
                if !has_panic_fn {
                    if !state.gen_typedefs_for.contains("str") {
                        typedefs.push_str("\ntypedef std::string str;");
                        includes.insert("#include <string>".to_string());
                    }
                    // for exit and EXIT_FAILURE
                    includes.insert("#include <cstdlib>".to_string());
                    // for cout and endl
                    includes.insert("#include <iostream>".to_string());
                    buf.push_str(PANIC_FUNCTION.trim_end());
                    has_panic_fn = true;
                }

                if !has_option_class {
                    buf.push_str(OPTION_CODE.trim_end());
                    has_option_class = true;
                }

                if !state.gen_typedefs_for.contains("uint") {
                    let plat_size = Ty::get_platform_size();
                    includes.insert("#include <cstdint>".to_string());
                    if plat_size == 64 {
                        typedefs.push_str("\ntypedef uint64_t uint_pr;");
                    } else {
                        typedefs.push_str("\ntypedef uint32_t uint_pr;");
                    }
                }
                includes.insert("#include <map>".to_string());
                buf.push_str(HMAP.trim_end());
            }
            "array" | "slice" => {
                if !has_panic_fn {
                    if !state.gen_typedefs_for.contains("str") {
                        typedefs.push_str("\ntypedef std::string str;");
                        includes.insert("#include <string>".to_string());
                    }
                    // for exit and EXIT_FAILURE
                    includes.insert("#include <cstdlib>".to_string());
                    // for cout and endl
                    includes.insert("#include <iostream>".to_string());
                    buf.push_str(PANIC_FUNCTION.trim_end());
                    has_panic_fn = true;
                }

                if !has_option_class {
                    buf.push_str(OPTION_CODE.trim_end());
                    has_option_class = true;
                }

                if !has_array_class {
                    if !state.gen_typedefs_for.contains("uint") {
                        let plat_size = Ty::get_platform_size();
                        includes.insert("#include <cstdint>".to_string());
                        if plat_size == 64 {
                            typedefs.push_str("\ntypedef uint64_t uint_pr;");
                        } else {
                            typedefs.push_str("\ntypedef uint32_t uint_pr;");
                        }
                    }
                    buf.push_str(SLICE_AND_ARRAY_CODE.trim_end());
                    has_array_class = true;
                }
            }
            "opt" => {
                if !has_panic_fn {
                    if !state.gen_typedefs_for.contains("str") {
                        typedefs.push_str("\ntypedef std::string str;");
                        includes.insert("#include <string>".to_string());
                    }
                    // for exit and EXIT_FAILURE
                    includes.insert("#include <cstdlib>".to_string());
                    // for cout and endl
                    includes.insert("#include <iostream>".to_string());
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
        if !typedefs.is_empty() {
            header.push('\n');
            header.push_str(&typedefs);
        }
        if !buf.is_empty() {
            header.push('\n');
            header.push_str(&buf);
        }
        header.push('\n');
    }
    header
}

pub fn cpp_gen_ty(ty: &Ty, state: &mut State) -> String {
    match ty {
        Ty::Char { .. } | Ty::Bool { .. } | Ty::Void { .. } => ty.as_str(),
        Ty::Str { .. } => {
            state.gen_typedefs_for.insert("str".to_string());
            ty.as_str()
        }
        Ty::Signed { is_int, .. } => {
            if *is_int {
                return "int".into();
            }
            state.gen_typedefs_for.insert(ty.as_str());
            ty.as_str()
        }
        Ty::Unsigned { is_uint, .. } => {
            if *is_uint {
                return "uint_pr".into();
            }
            state.gen_typedefs_for.insert(ty.as_str());
            ty.as_str()
        }
        Ty::StaticArray { sub_ty, size, .. } => {
            state.gen_typedefs_for.insert("array".into());
            let sub_ty = cpp_gen_ty(sub_ty, state);
            let size = size.as_ref().unwrap().as_str();
            format!("Array<{sub_ty}, {size}>")
        }
        Ty::Slice { sub_ty, .. } => {
            state.gen_typedefs_for.insert("slice".into());
            let sub_ty = cpp_gen_ty(sub_ty, state);
            format!("Slice<{sub_ty}>")
        }
        Ty::Optional { sub_ty, .. } => {
            state.gen_typedefs_for.insert("slice".into());
            let sub_ty = cpp_gen_ty(sub_ty, state);
            format!("Option<{sub_ty}>")
        }
        Ty::NamedType { name, .. } | Ty::Struct { name, .. } => name.clone(),
        Ty::Func { params, ret, .. } => {
            state.gen_typedefs_for.insert("func".into());
            let param_ty_s = params
                .iter()
                .map(|pty| cpp_gen_ty(pty, state))
                .collect::<Vec<String>>()
                .join(", ");
            format!("function<{}({param_ty_s})>", cpp_gen_ty(ret, state))
        }
        Ty::Pointer { sub_ty, .. } => {
            format!("{}*", cpp_gen_ty(sub_ty, state))
        }
        Ty::HashMap { key_ty, val_ty, .. } => {
            state.gen_typedefs_for.insert("hmap".into());
            state.uses_defer = true;
            format!(
                "HashMap<{}, {}>",
                cpp_gen_ty(key_ty, state),
                cpp_gen_ty(val_ty, state)
            )
        }
        _ => {
            unreachable!(
                "cpp::cpp_gen_ty(): ran into {} at {:?}, which should not occur.",
                ty.as_str(),
                ty.get_loc()
            )
        }
    }
}

pub fn cpp_gen_expr(expr: &TyExpr, state: &mut State) -> String {
    match expr {
        TyExpr::Integer { .. }
        | TyExpr::Char { .. }
        | TyExpr::Bool { .. }
        | TyExpr::Ident { .. } => expr.as_str(),
        TyExpr::UnaryOp { op, expr, .. } => format!("{}{}", op.as_str(), cpp_gen_expr(expr, state)),
        TyExpr::Str { .. } => {
            format!("str({})", expr.as_str())
        }
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
            state.gen_typedefs_for.insert("str".into());
            state.uses_str_interpolation = true;
            let buf = parts
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
        TyExpr::Lambda {
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
            format!("[&]({params_s}) -> {ret_ty_s}{body_s}")
        }
        TyExpr::DerefPtr { target } => format!("*{}", cpp_gen_expr(target, state)),
        TyExpr::MakePtrFromAddrOf { target } => format!("&{}", cpp_gen_expr(target, state)),
        TyExpr::NewAlloc { ty, args } => {
            format!(
                "new {}({})",
                cpp_gen_ty(ty, state),
                args.iter()
                    .map(|arg| { cpp_gen_expr(arg, state) })
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
        TyExpr::SliceDefaultAlloc { ty } => format!("{}()", cpp_gen_ty(ty, state)),
        TyExpr::SliceSizedAlloc { ty, cap } => {
            format!("{}({})", cpp_gen_ty(ty, state), cpp_gen_expr(cap, state))
        }
        TyExpr::NewArrayAlloc { ty, init } => {
            format!(
                "{}({{{}}})",
                cpp_gen_ty(ty, state),
                init.iter()
                    .map(|expr| { cpp_gen_expr(expr, state) })
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
        TyExpr::HashMap { pairs } => {
            format!(
                "{{ {} }}",
                pairs
                    .iter()
                    .map(|pair| {
                        format!(
                            "{{ {}, {} }}",
                            cpp_gen_expr(&pair.key, state),
                            cpp_gen_expr(&pair.val, state)
                        )
                    })
                    .collect::<Vec<String>>()
                    .join(", ")
            )
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
                format!("{}{} {name};", state.get_pad(), cpp_gen_ty(ty, state))
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
            buf = format!("{}{ret_ty_s} {name}({params_s}){body_s}\n", state.get_pad());
        }
        TyIns::Struct {
            name,
            fields,
            funcs,
        } => {
            buf = format!("{}struct {name} {{\n", state.get_pad());
            state.indent();
            // write code for instance variable, self
            // buf.push_str(&format!("{}{name}& self = *this;\n", state.get_pad()));

            // write all fields
            for f in fields {
                let mut ins_str = cpp_gen_ins(f, state);
                ins_str.push('\n');
                buf.push_str(&ins_str);
            }

            // write initializer
            if !funcs.is_empty() {
                // write all functions
                for func in funcs {
                    if let TyIns::Func {
                        name: fn_name,
                        params,
                        body,
                        ..
                    } = func
                    {
                        if fn_name == "init" {
                            buf.push_str(&format!(
                                "\n{}{}({})",
                                state.get_pad(),
                                name,
                                params
                                    .iter()
                                    .map(|param| {
                                        format!(
                                            "{} {}",
                                            cpp_gen_ty(&param.given_ty, state),
                                            param.name.as_str()
                                        )
                                    })
                                    .collect::<Vec<String>>()
                                    .join(", ")
                            ));

                            buf.push_str(&format!("\n{}", cpp_gen_ins(body, state)));
                            continue;
                        }
                    }
                    buf.push('\n');
                    let ins_str = cpp_gen_ins(func, state);
                    buf.push_str(&ins_str);
                }
            }
            state.dedent();
            buf.push_str(&format!("\n{}}};", state.get_pad()));
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
        TyIns::AssignTo { target, val } => {
            buf = format!(
                "{}{} = {};",
                state.get_pad(),
                cpp_gen_expr(target, state),
                cpp_gen_expr(val, state)
            );
        }
        TyIns::Defer { sub_ins } => {
            state.uses_defer = true;
            buf = format!("{}defer(\n", state.get_pad());
            state.indent();
            buf.push_str(&cpp_gen_ins(sub_ins, state));
            buf.push('\n');
            state.dedent();
            buf.push_str(&format!("{});", state.get_pad()));
        }
        TyIns::Break => {
            buf = format!("{}break;", state.get_pad());
        }
        TyIns::Continue => {
            buf = format!("{}continue;", state.get_pad());
        }
        TyIns::ForInLoop { var, target, block } => {
            buf = format!(
                "{}for (const auto {var} : {})\n{}",
                state.get_pad(),
                cpp_gen_expr(target, state),
                cpp_gen_ins(block, state)
            );
        }
        TyIns::WhileLoop { cond, block } => {
            buf = format!(
                "{}while ({})\n{}",
                state.get_pad(),
                cpp_gen_expr(cond, state),
                cpp_gen_ins(block, state)
            )
        }
        TyIns::RegLoop {
            init,
            cond,
            update,
            block,
        } => {
            let mut update_s = cpp_gen_ins(update, state);
            if update_s.ends_with(";") {
                update_s.pop();
            }
            buf = format!(
                "{}for ({} {}; {update_s})\n{}",
                state.get_pad(),
                cpp_gen_ins(init, state),
                cpp_gen_expr(cond, state),
                cpp_gen_ins(block, state)
            );
        }
        TyIns::Free { target } => {
            buf = format!("{}delete {};", state.get_pad(), cpp_gen_expr(target, state));
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

    let og_file_path = Path::new(file_mod.src_file.as_str());
    // println!("{og_file_path:?}");
    let file_path = og_file_path.with_extension("cpp");
    // println!("{file_path:?}");
    let exe_path = og_file_path.with_extension("");
    // println!("{exe_path:?}");
    std::fs::write(file_path.clone(), cpp_code.trim()).expect("Unable to write file");

    let clang_compile = Command::new("clang++")
        .args([
            file_path.to_str().unwrap(),
            "-o",
            exe_path.to_str().unwrap(),
            "-std=c++17",
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
    Ok(format!(
        "compiled to {}",
        exe_path.to_str().unwrap().to_string()
    ))
}
