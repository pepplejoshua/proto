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
    Type {
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
        name: String,
        fields: BTreeMap<String, TypeId>,
        funcs: BTreeMap<String, TypeId>,
    },
    Pointer {
        sub_ty: TypeId,
    },
    ErrorType,
}

// an instance of a type definition
pub struct TypeInst {
    pub id: TypeId,
    pub scope_id: usize,
    pub loc: Rc<SourceRef>,
}

pub fn make_inst(id: TypeId, scope_id: usize, loc: Rc<SourceRef>) -> TypeInst {
    TypeInst { id, scope_id, loc }
}

pub struct TypeTable {
    types: HashMap<TypeDef, TypeId>,
    definitions: Vec<(TypeDef, Option<Rc<SourceRef>>)>,
    next_type_id: usize,
}

impl TypeTable {
    pub fn new() -> TypeTable {
        return TypeTable {
            types: HashMap::new(),
            definitions: vec![],
            next_type_id: 0,
        };
    }

    pub fn intern_type(&mut self, ty: &Rc<Ty>) {
        match ty.as_ref() {
            Ty::Type { loc } => todo!(),
            Ty::Signed { size, is_int, loc } => todo!(),
            Ty::Unsigned { size, is_uint, loc } => todo!(),
            Ty::Float { size, loc } => todo!(),
            Ty::Str { loc } => todo!(),
            Ty::Char { loc } => todo!(),
            Ty::Void { loc } => todo!(),
            Ty::Bool { loc } => todo!(),
            Ty::Func {
                params,
                ret,
                loc,
                is_const,
            } => todo!(),
            Ty::StaticArray { sub_ty, size, loc } => todo!(),
            Ty::Slice { sub_ty, loc } => todo!(),
            Ty::Optional { sub_ty, loc } => todo!(),
            Ty::Struct {
                fields,
                static_funcs,
                methods,
                loc,
            } => todo!(),
            Ty::NamedType { loc } => todo!(),
            Ty::AccessMemberType { target, mem, loc } => todo!(),
            Ty::TypeFunc { func, args, loc } => todo!(),
            Ty::Pointer { sub_ty, loc } => todo!(),
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
}
