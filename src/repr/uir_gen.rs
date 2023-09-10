#![allow(dead_code, unused_variables)]
use crate::frontend::ast::{CompilationModule, Expr, Instruction, TypeReference};

use super::uir::{Bundle, Index, Tag, UIRIns};

pub fn gen_uir(cm: &CompilationModule, bundle: &mut Bundle) {
    for ins in cm.instructions.iter() {
        let loc = gen_ins(ins, bundle);
        if let Some(loc) = loc {
            bundle.top_level_uir.push(loc);
        }
    }
}

pub fn gen_ins(ins: &Instruction, bundle: &mut Bundle) -> Option<Index> {
    match ins {
        Instruction::ConstantDecl {
            const_name,
            const_type,
            init_expr,
            src_ref,
            is_public,
        } => {
            // if there is an init_expr, generate the expr
            let (expr_i, const_type_i) = match (init_expr, const_type) {
                (None, None) => unreachable!("const decl with no expr or type"),
                (None, Some(ty_ref)) => {
                    // no expr, but some type
                    let expr_i = bundle.intern_uir(UIRIns {
                        tag: Tag::UninitializedValue,
                        aux_data: 0,
                        src: Some(src_ref.clone()),
                        indices: Vec::new(),
                    });
                    let const_type_i = bundle.intern_type(ty_ref.clone());
                    (expr_i, const_type_i)
                }
                (Some(i_expr), None) => {
                    // expr, but no type
                    let expr_i = gen_expr(i_expr, bundle);
                    let const_type_i = bundle.intern_type(TypeReference::Infer(src_ref.clone()));
                    (expr_i, const_type_i)
                }
                (Some(_), Some(_)) => {
                    // expr and type
                    let expr_i = gen_expr(init_expr.as_ref().unwrap(), bundle);
                    let const_type_i = bundle.intern_type(const_type.as_ref().unwrap().clone());
                    (expr_i, const_type_i)
                }
            };

            let const_name_i = bundle.intern_string(const_name.as_str());

            let ins = UIRIns {
                tag: Tag::Const,
                aux_data: 0,
                src: Some(src_ref.clone()),
                indices: vec![const_name_i, const_type_i, expr_i],
            };
            Some(bundle.intern_uir(ins))
        }
        Instruction::SingleLineComment { comment, src } => None,
        _ => {
            todo!()
        }
    }
}

pub fn gen_expr(e: &Expr, bundle: &mut Bundle) -> Index {
    match e {
        Expr::FnCall { func, args, span } => {
            let fn_name_i = if let Expr::Id(fn_name, _, _) = &**func {
                bundle.intern_string(fn_name.as_str())
            } else {
                unreachable!("fn call with non-id name")
            };
            let mut data = vec![fn_name_i];
            let args_i: Vec<Index> = args.iter().map(|a| gen_expr(a, bundle)).collect();
            data.extend(args_i);
            let ins = UIRIns {
                tag: Tag::Call,
                aux_data: args.len(),
                src: Some(span.clone()),
                indices: data,
            };
            bundle.intern_uir(ins)
        }
        Expr::Id(name, _, src) => {
            let name_i = bundle.intern_string(name.as_str());
            let ins = UIRIns {
                tag: Tag::NameRef,
                aux_data: 0,
                src: Some(src.clone()),
                indices: vec![name_i],
            };
            bundle.intern_uir(ins)
        }
        Expr::TypeExpr(ty_ref, src) => {
            let ty_ref_i = bundle.intern_type(ty_ref.clone());
            let ins = UIRIns {
                tag: Tag::TyRef,
                aux_data: 0,
                src: Some(src.clone()),
                indices: vec![ty_ref_i],
            };
            bundle.intern_uir(ins)
        }
        Expr::Integer(num, src) => {
            let num_i = bundle.intern_string(num.clone());
            let ins = UIRIns {
                tag: Tag::Int,
                aux_data: 0,
                src: Some(src.clone()),
                indices: vec![num_i],
            };
            bundle.intern_uir(ins)
        }
        _ => {
            todo!()
        }
    }
}
