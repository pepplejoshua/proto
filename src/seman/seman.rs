#![allow(unused)]

use std::collections::HashMap;

use crate::types::signature::{Sig, Type};

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
            Sig::Function => {
                // apply substitution to function types recursively
                let sub_types = ty
                    .sub_types
                    .iter()
                    .map(|t| self.apply(t))
                    .collect::<Vec<Type>>();
                let aux_type = ty.aux_type.as_ref().map(|t| Box::new(self.apply(t)));
                Type {
                    sub_types,
                    aux_type,
                    ..ty.clone()
                }
            }
            _ => ty.clone(),
        }
    }

    // extend the current substitution with a new mapping
    fn extend(&self, var: String, ty: Type) -> Self {
        let mut new_subs = self.subs.clone();
        new_subs.insert(var, ty);
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
    todo!()
}

fn occurs_check(var_name: &String, ty: &Type, subst: &Substitution) -> bool {
    match &ty.tag {
        Sig::Infer => {
            if let Some(name) = &ty.name {
                if name == var_name {
                    return true;
                }
                if let Some(sub_ty) = subst.get(name) {
                    return occurs_check(var_name, sub_ty, subst);
                }
            }
            false
        }
        _ => false,
    }
}
