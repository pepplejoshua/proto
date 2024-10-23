#![allow(unused)]
use std::{
    collections::{BTreeMap, HashMap},
    rc::Rc,
};

use crate::{parser::type_signature::Ty, source::source::SourceRef};

// the id for a type
pub type TypeId = usize;

// this is the concrete type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeDef {
    TypeAlias {
        actual_type: TypeId,
    },
    Signed {
        size: u8,
        is_int: bool,
    },
    Unsigned {
        size: u8,
        is_uint: bool,
    },
    Float {
        size: u8,
    },
    Str,
    Char,
    Void,
    Bool,
    Func {
        params: Vec<TypeId>,
        ret: TypeId,
    },
    Method {
        params: Vec<TypeId>,
        ret: TypeId,
        is_const: bool,
    },
    StaticArray {
        sub_ty: TypeId,
        size: usize,
    },
    Slice {
        sub_ty: TypeId,
    },
    Optional {
        sub_ty: TypeId,
    },
    Struct {
        fields: BTreeMap<String, TypeId>,
        funcs: BTreeMap<String, TypeId>,
    },
    Pointer {
        sub_ty: TypeId,
    },
    ErrorType,
}

// an instance of a type definition
#[derive(Debug, Clone)]
pub struct TypeInst {
    pub id: TypeId,
    pub loc: Rc<SourceRef>,
}

pub fn make_inst(id: TypeId, loc: Rc<SourceRef>) -> TypeInst {
    TypeInst { id, loc }
}

pub struct TypeTable {
    types: HashMap<TypeDef, TypeId>,
    definitions: Vec<(TypeDef, Option<Rc<SourceRef>>)>,
    next_type_id: usize,
}

impl TypeTable {
    pub fn new() -> TypeTable {
        return TypeTable {
            types: HashMap::from([(TypeDef::ErrorType, 0)]),
            definitions: vec![(TypeDef::ErrorType, None)],
            next_type_id: 1,
        };
    }

    pub fn intern_type(&mut self, ty: Rc<Ty>) -> TypeInst {
        match ty.as_ref() {
            Ty::Signed { size, is_int, loc } => {
                let ty_def = TypeDef::Signed {
                    size: *size,
                    is_int: *is_int,
                };
                let ty_id = self.add_new_type_def(ty_def, None);
                TypeInst {
                    id: ty_id,
                    loc: loc.clone(),
                }
            }
            Ty::Unsigned { size, is_uint, loc } => {
                let ty_def = TypeDef::Unsigned {
                    size: *size,
                    is_uint: *is_uint,
                };
                let ty_id = self.add_new_type_def(ty_def, None);
                TypeInst {
                    id: ty_id,
                    loc: loc.clone(),
                }
            }
            Ty::Float { size, loc } => {
                let ty_def = TypeDef::Float { size: *size };
                let ty_id = self.add_new_type_def(ty_def, None);
                TypeInst {
                    id: ty_id,
                    loc: loc.clone(),
                }
            }
            Ty::Str { loc } => {
                let ty_def = TypeDef::Str {};
                let ty_id = self.add_new_type_def(ty_def, None);
                TypeInst {
                    id: ty_id,
                    loc: loc.clone(),
                }
            }
            Ty::Char { loc } => {
                let ty_def = TypeDef::Char {};
                let ty_id = self.add_new_type_def(ty_def, None);
                TypeInst {
                    id: ty_id,
                    loc: loc.clone(),
                }
            }
            Ty::Void { loc } => {
                let ty_def = TypeDef::Void {};
                let ty_id = self.add_new_type_def(ty_def, None);
                TypeInst {
                    id: ty_id,
                    loc: loc.clone(),
                }
            }
            Ty::Bool { loc } => {
                let ty_def = TypeDef::Bool {};
                let ty_id = self.add_new_type_def(ty_def, None);
                TypeInst {
                    id: ty_id,
                    loc: loc.clone(),
                }
            }
            Ty::Func {
                params,
                ret,
                loc,
                is_const,
            } => {
                let ret_inst = self.intern_type(ret.clone());
                let mut nparams = vec![];
                for param in params.iter() {
                    let param_inst = self.intern_type(param.clone());
                    nparams.push(param_inst.id);
                }
                let ty_def = TypeDef::Func {
                    params: nparams,
                    ret: ret_inst.id,
                };
                let ty_id = self.add_new_type_def(ty_def, None);
                TypeInst {
                    id: ty_id,
                    loc: loc.clone(),
                }
            }
            Ty::StaticArray { sub_ty, size, loc } => {
                todo!()
            }
            Ty::Slice { sub_ty, loc } => todo!(),
            Ty::Optional { sub_ty, loc } => todo!(),
            Ty::Struct {
                fields,
                static_funcs,
                methods,
                loc,
            } => todo!(),
            Ty::NamedType { name, loc } => todo!(),
            Ty::AccessMemberType { target, mem, loc } => todo!(),
            Ty::Pointer { sub_ty, loc } => {
                let sub_ty_inst = self.intern_type(sub_ty.clone());
                let ty_def = TypeDef::Pointer {
                    sub_ty: sub_ty_inst.id,
                };
                let ty_id = self.add_new_type_def(ty_def, None);
                TypeInst {
                    id: ty_id,
                    loc: loc.clone(),
                }
            }
            Ty::Tuple { sub_tys, loc } => todo!(),
            Ty::ErrorType { loc } => unreachable!("Error type should not be interned."),
        }
    }

    pub fn add_new_type_def(
        &mut self,
        ty_def: TypeDef,
        def_location: Option<Rc<SourceRef>>,
    ) -> TypeId {
        if let Some(&id) = self.types.get(&ty_def) {
            id
        } else {
            let id = self.next_type_id;
            self.next_type_id += 1;
            self.types.insert(ty_def.clone(), id);
            self.definitions.push((ty_def.clone(), def_location));
            id
        }
    }

    pub fn type_id_exists(&self, id: TypeId) -> bool {
        self.definitions.get(id).is_some()
    }

    pub fn get_type_def_using_id(&self, id: TypeId) -> Option<&(TypeDef, Option<Rc<SourceRef>>)> {
        self.definitions.get(id)
    }

    pub fn type_def_to_str(&self, idx: usize) -> String {
        let (ty_def, _) = &self.definitions[idx];
        match ty_def {
            TypeDef::TypeAlias { actual_type } => {
                format!("aliased to {}", self.type_def_to_str(*actual_type))
            }
            TypeDef::Signed { size, is_int } => {
                format!(
                    "i{}",
                    if *is_int {
                        "nt".to_string()
                    } else {
                        size.to_string()
                    }
                )
            }
            TypeDef::Unsigned { size, is_uint } => {
                format!(
                    "u{}",
                    if *is_uint {
                        "int".to_string()
                    } else {
                        size.to_string()
                    }
                )
            }
            TypeDef::Float { size } => {
                format!("f{}", size.to_string())
            }
            TypeDef::Str => format!("str"),
            TypeDef::Char => format!("char"),
            TypeDef::Void => format!("void"),
            TypeDef::Bool => format!("bool"),
            TypeDef::Func { params, ret } => {
                format!(
                    "\\({}) {}",
                    params
                        .iter()
                        .map(|param| { self.type_def_to_str(*param) })
                        .collect::<Vec<String>>()
                        .join(", "),
                    self.type_def_to_str(*ret)
                )
            }
            TypeDef::Method {
                params,
                ret,
                is_const,
            } => todo!(),
            TypeDef::StaticArray { sub_ty, size } => todo!(),
            TypeDef::Slice { sub_ty } => todo!(),
            TypeDef::Struct { fields, funcs } => todo!(),
            TypeDef::Optional { sub_ty } => {
                format!("?{}", self.type_def_to_str(*sub_ty))
            }
            TypeDef::Pointer { sub_ty } => {
                format!("*{}", self.type_def_to_str(*sub_ty))
            }
            TypeDef::ErrorType => todo!(),
        }
    }

    pub fn display(&self) {
        for idx in 0..self.next_type_id {
            let idx_string = self.type_def_to_str(idx);
            println!("{idx}. {idx_string}")
        }
    }
}
