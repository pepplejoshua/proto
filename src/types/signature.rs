#![allow(dead_code)]
#![allow(unused_variables)]

use crate::parser::ast::Expr;
use crate::source::source::{SourceFile, SourceRef};
use std::sync::atomic::AtomicUsize;
use std::{
    borrow::BorrowMut,
    collections::{BTreeMap, HashMap},
    rc::Rc,
};

// the id for a type
pub type TypeId = usize;

// this is the concrete type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeDef {
    Type,
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
    pub loc: Rc<SourceRef>,
}

pub fn make_inst(id: TypeId, loc: Rc<SourceRef>) -> TypeInst {
    TypeInst { id, loc }
}

pub struct TypeTable {
    types: HashMap<TypeDef, TypeId>,
    definitions: Vec<TypeDef>,
    next_id: AtomicUsize,
}

impl TypeTable {
    pub fn new() -> TypeTable {
        return TypeTable {
            types: HashMap::new(),
            definitions: vec![],
            next_id: AtomicUsize::new(0),
        };
    }

    pub fn add_new_type_def(&mut self, ty_def: TypeDef) -> TypeId {
        if let Some(&id) = self.types.get(&ty_def) {
            id
        } else {
            let id = self
                .next_id
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            self.types.insert(ty_def.clone(), id);
            self.definitions.push(ty_def.clone());
            id
        }
    }

    pub fn type_id_exists(&self, id: TypeId) -> bool {
        self.definitions.get(id).is_some()
    }

    pub fn get_type_def_using_id(&self, id: TypeId) -> Option<&TypeDef> {
        self.definitions.get(id)
    }
}

#[derive(Debug, Clone)]
pub enum Ty {
    Type {
        loc: Rc<SourceRef>,
    },
    Signed {
        size: u8,
        is_int: bool,
        loc: Rc<SourceRef>,
    },
    Unsigned {
        size: u8,
        is_uint: bool,
        loc: Rc<SourceRef>,
    },
    Float {
        size: u8,
        loc: Rc<SourceRef>,
    },
    Str {
        loc: Rc<SourceRef>,
    },
    Char {
        loc: Rc<SourceRef>,
    },
    Void {
        loc: Rc<SourceRef>,
    },
    Bool {
        loc: Rc<SourceRef>,
    },
    Func {
        params: Vec<Rc<Ty>>,
        ret: Rc<Ty>,
        loc: Rc<SourceRef>,
        is_const: bool,
    },
    StaticArray {
        sub_ty: Rc<Ty>,
        size: Expr,
        loc: Rc<SourceRef>,
    },
    Slice {
        sub_ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    Optional {
        sub_ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    Struct {
        fields: HashMap<String, Rc<Ty>>,
        funcs: HashMap<String, Rc<Ty>>,
        loc: Rc<SourceRef>,
    },
    NamedType {
        name: String,
        loc: Rc<SourceRef>,
    },
    Pointer {
        sub_ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    Tuple {
        sub_tys: Vec<Rc<Ty>>,
        loc: Rc<SourceRef>,
    },
    ErrorType {
        loc: Rc<SourceRef>,
    },
}

impl Ty {
    pub fn is_float_ty(&self) -> bool {
        match self {
            Ty::Float { .. } => true,
            _ => false,
        }
    }

    pub fn is_num_ty(&self) -> bool {
        match self {
            Ty::Signed { .. } | Ty::Unsigned { .. } => true,
            _ => false,
        }
    }

    pub fn is_void(&self) -> bool {
        match self {
            Ty::Void { .. } => true,
            _ => false,
        }
    }

    pub fn is_signed_ty(&self) -> bool {
        match self {
            Ty::Signed { .. } => true,
            _ => false,
        }
    }

    pub fn is_unsigned_ty(&self) -> bool {
        match self {
            Ty::Unsigned { .. } => true,
            _ => false,
        }
    }

    pub fn is_error_ty(&self) -> bool {
        match self {
            Ty::ErrorType { .. } => true,
            _ => false,
        }
    }

    pub fn is_indexable(&self) -> bool {
        match self {
            Ty::Str { .. } | Ty::StaticArray { .. } | Ty::Slice { .. } => true,
            _ => false,
        }
    }

    pub fn is_sliceable(&self) -> bool {
        match self {
            Ty::StaticArray { .. } | Ty::Slice { .. } => true,
            _ => false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        match self {
            Ty::Pointer { .. } => true,
            _ => false,
        }
    }

    pub fn get_loc(&self) -> Rc<SourceRef> {
        match self {
            Ty::Type { loc }
            | Ty::Signed { loc, .. }
            | Ty::Unsigned { loc, .. }
            | Ty::Str { loc, .. }
            | Ty::Char { loc }
            | Ty::Void { loc }
            | Ty::Bool { loc }
            | Ty::Func { loc, .. }
            | Ty::StaticArray { loc, .. }
            | Ty::Slice { loc, .. }
            | Ty::Optional { loc, .. }
            | Ty::Struct { loc, .. }
            | Ty::NamedType { loc, .. }
            | Ty::Pointer { loc, .. }
            | Ty::Float { loc, .. }
            | Ty::Tuple { loc, .. }
            | Ty::ErrorType { loc } => loc.clone(),
        }
    }

    pub fn set_loc(&mut self, n_loc: Rc<SourceRef>) {
        match self {
            Ty::Type { loc }
            | Ty::Signed { loc, .. }
            | Ty::Unsigned { loc, .. }
            | Ty::Str { loc, .. }
            | Ty::Char { loc }
            | Ty::Void { loc }
            | Ty::Bool { loc }
            | Ty::Func { loc, .. }
            | Ty::StaticArray { loc, .. }
            | Ty::Slice { loc, .. }
            | Ty::Struct { loc, .. }
            | Ty::NamedType { loc, .. }
            | Ty::Optional { loc, .. }
            | Ty::Pointer { loc, .. }
            | Ty::Float { loc, .. }
            | Ty::Tuple { loc, .. }
            | Ty::ErrorType { loc } => {
                let b_loc = loc.borrow_mut();
                *b_loc = n_loc;
            }
        }
    }

    pub fn as_str(&self, src: &SourceFile) -> String {
        match self {
            Ty::Type { .. } => "type".to_string(),
            Ty::Signed { size, is_int, .. } => {
                if *is_int {
                    "int".into()
                } else {
                    format!("i{size}")
                }
            }
            Ty::Unsigned { size, is_uint, .. } => {
                if *is_uint {
                    "uint".into()
                } else {
                    format!("u{size}")
                }
            }
            Ty::Float { size, loc } => {
                format!("f{size}")
            }
            Ty::Str { .. } => "str".into(),
            Ty::Char { .. } => "char".into(),
            Ty::Void { .. } => "void".into(),
            Ty::Bool { .. } => "bool".into(),
            Ty::Func { params, ret, .. } => {
                format!(
                    "fn ({}) {}",
                    params
                        .iter()
                        .map(|p| { p.as_str(src) })
                        .collect::<Vec<String>>()
                        .join(", "),
                    ret.as_str(src)
                )
            }
            Ty::StaticArray { sub_ty, size, .. } => {
                let size_s = size.as_str(src);
                format!("[{}, {size_s}]", sub_ty.as_str(src))
            }
            Ty::Slice { sub_ty, .. } => format!("[{}]", sub_ty.as_str(src)),
            Ty::Optional { sub_ty, .. } => format!("?{}", sub_ty.as_str(src)),
            Ty::NamedType { .. } | Ty::Struct { .. } => "type".to_string(),
            Ty::Pointer { sub_ty, .. } => format!("*{}", sub_ty.as_str(src)),
            Ty::Tuple { sub_tys, .. } => {
                if sub_tys.len() == 1 {
                    format!("({},)", sub_tys[0].as_str(src))
                } else {
                    format!(
                        "({})",
                        sub_tys
                            .iter()
                            .map(|ty| { ty.as_str(src) })
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                }
            }
            Ty::ErrorType { .. } => "err!".into(),
        }
    }

    pub fn clone_loc(&self, loc: Rc<SourceRef>) -> Rc<Ty> {
        let mut ty = self.clone();
        ty.set_loc(loc);
        Rc::new(ty)
    }

    pub fn get_int_ty(loc: Rc<SourceRef>) -> Rc<Ty> {
        Rc::new(Ty::Signed {
            size: Ty::get_platform_size(),
            is_int: true,
            loc,
        })
    }

    pub fn get_uint_ty(loc: Rc<SourceRef>) -> Rc<Ty> {
        Rc::new(Ty::Unsigned {
            size: Ty::get_platform_size(),
            is_uint: true,
            loc,
        })
    }

    pub fn get_platform_size() -> u8 {
        if std::mem::size_of::<usize>() == 8 {
            64
        } else {
            32
        }
    }
}
