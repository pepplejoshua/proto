#![allow(unused)]

use std::collections::HashMap;

use crate::{
    parser::ast::{BinOpType, Expr},
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
struct Substitution {
    subs: HashMap<String, Type>,
}

impl Substitution {
    fn new() -> Self {
        Substitution {
            subs: HashMap::new(),
        }
    }

    // apply the current substitution to a type
    fn apply(&self, ty: &Type) -> Type {
        match &ty.tag {
            Sig::Infer => {
                // if the type is to be inferred and there is a substitution for it,
                // apply it
                if let Some(name) = &ty.name {
                    if let Some(sub_ty) = self.subs.get(name) {
                        return sub_ty.clone();
                    }
                }
                ty.clone()
            }
            _ => ty.clone(),
        }
    }

    // extend the current substitution with a new mapping
    fn extend(&self, var: String, ty: Type) -> Self {
        let mut new_subs = self.subs.clone();
        new_subs.insert(var, self.apply(&ty));
        Substitution { subs: new_subs }
    }

    // compose 2 substitutions
    fn compose(&self, other: &Substitution) -> Substitution {
        let mut new_subs = self.subs.clone();
        for (var, ty) in &other.subs {
            new_subs.insert(var.clone(), self.apply(ty));
        }
        Substitution { subs: new_subs }
    }
}

fn unify(t1: &Type, t2: &Type, subst: &Substitution) -> Result<Substitution, String> {
    let t1 = subst.apply(t1);
    let t2 = subst.apply(t2);

    match (&t1.tag, &t2.tag) {
        (Sig::Infer, _) => unify_var(&t1, &t2, subst),
        (_, Sig::Infer) => unify_var(&t1, &t2, subst),
        (Sig::Identifier, Sig::Identifier) => {
            if t1.name == t2.name {
                Ok(subst.clone())
            } else {
                Err(format!(
                    "Mismatching identifier types: {:?} vs {:?}",
                    t1.name, t2.name
                ))
            }
        }
        _ => {
            if t1.tag == t2.tag {
                Ok(subst.clone())
            } else {
                Err(format!("Type mismatch: {} vs {}", t1.as_str(), t2.as_str()))
            }
        }
    }
}

fn unify_var(var: &Type, ty: &Type, subst: &Substitution) -> Result<Substitution, String> {
    if var == ty {
        return Ok(subst.clone());
    }

    if let Sig::Infer = var.tag {
        if let Some(name) = &var.name {
            if occurs_check(name, ty, subst) {
                return Err(format!("Occurs check failed for {:?} in {:?}", var, ty));
            }
            let new_sub = subst.extend(name.clone(), ty.clone());
            return Ok(new_sub);
        }
    }

    Err(format!(
        "Cannot unify variable {:?} with type {:?}",
        var, ty
    ))
}

fn occurs_check(var_name: &String, ty: &Type, subst: &Substitution) -> bool {
    match &ty.tag {
        Sig::Infer => {
            if let Some(name) = &ty.name {
                if name == var_name {
                    return true;
                }
                if let Some(sub_ty) = subst.subs.get(name) {
                    return occurs_check(var_name, sub_ty, subst);
                }
            }
            false
        }
        _ => false,
    }
}

fn infer_expr(
    expr: &Expr,
    env: &TypeEnv,
    subst: &Substitution,
) -> Result<(Type, Substitution), String> {
    match expr {
        Expr::Number { val, loc } => Ok((
            Type {
                tag: Sig::Int,
                name: None,
                sub_types: vec![],
                aux_type: None,
                loc: loc.clone(),
            },
            subst.clone(),
        )),
        Expr::Str { val, loc } => Ok((
            Type {
                tag: Sig::Str,
                name: None,
                sub_types: vec![],
                aux_type: None,
                loc: loc.clone(),
            },
            subst.clone(),
        )),
        Expr::Char { val, loc } => Ok((
            Type {
                tag: Sig::Char,
                name: None,
                sub_types: vec![],
                aux_type: None,
                loc: loc.clone(),
            },
            subst.clone(),
        )),
        Expr::Bool { val, loc } => Ok((
            Type {
                tag: Sig::Bool,
                name: None,
                sub_types: vec![],
                aux_type: None,
                loc: loc.clone(),
            },
            subst.clone(),
        )),
        Expr::Void { loc } => Ok((
            Type {
                tag: Sig::Void,
                name: None,
                sub_types: vec![],
                aux_type: None,
                loc: loc.clone(),
            },
            subst.clone(),
        )),
        Expr::Ident { name, loc } => {
            if let Some(var_type) = env.lookup(name) {
                Ok((var_type.clone(), subst.clone()))
            } else {
                Err(format!("Unbound variable: {}", name))
            }
        }
        Expr::BinOp {
            op,
            left,
            right,
            loc,
        } => {
            let (left_ty, subst1) = infer_expr(left, env, subst)?;
            let res = match op {
                BinOpType::Add => todo!(),
                BinOpType::Sub => todo!(),
                BinOpType::Mult => todo!(),
                BinOpType::Div => todo!(),
                BinOpType::Mod => todo!(),
                BinOpType::And => todo!(),
                BinOpType::Or => todo!(),
                BinOpType::Eq => todo!(),
                BinOpType::Neq => todo!(),
                BinOpType::Gt => todo!(),
                BinOpType::Lt => todo!(),
                BinOpType::GtEq => todo!(),
                BinOpType::LtEq => todo!(),
                BinOpType::AccessMember => todo!(),
                BinOpType::IndexArray => todo!(),
            };
            Ok(res)
        }
        Expr::InitStruct {
            struct_name,
            fields,
            loc,
        } => todo!(),
        Expr::CallFn { func, args, loc } => todo!(),
        Expr::UnaryOp { op, expr, loc } => todo!(),
        Expr::ErrorExpr { msg, loc } => todo!(),
    }
}
