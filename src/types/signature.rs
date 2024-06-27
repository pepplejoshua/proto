#![allow(dead_code)]
#![allow(unused_variables)]

use std::borrow::BorrowMut;

use crate::{parser::ast::Expr, source::source::SourceRef};

#[derive(Debug, Clone)]
pub enum Ty {
    Signed {
        size: u8,
        is_int: bool,
        loc: SourceRef,
    },
    Unsigned {
        size: u8,
        is_uint: bool,
        loc: SourceRef,
    },
    Str {
        loc: SourceRef,
    },
    Char {
        loc: SourceRef,
    },
    Void {
        loc: SourceRef,
    },
    Bool {
        loc: SourceRef,
    },
    Func {
        params: Vec<Ty>,
        ret: Box<Ty>,
        loc: SourceRef,
    },
    StaticArray {
        sub_ty: Box<Ty>,
        size: Option<Expr>,
        loc: SourceRef,
    },
    Slice {
        sub_ty: Box<Ty>,
        loc: SourceRef,
    },
    Optional {
        sub_ty: Box<Ty>,
        loc: SourceRef,
    },
    ErrorType {
        loc: SourceRef,
    },
}

// this is parsed from user source code and is used to inform the
// generation of the above and the type checking as well
#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub enum Sig {
    UserDefinedType,
    Bool,
    Char,
    Void,
    Str,
    I8,
    I16,
    I32,
    I64,
    Int,
    U8,
    U16,
    U32,
    U64,
    UInt,
    Function,
    StaticArray,
    Slice,
    Optional,
    ErrorType,
}

impl Ty {
    pub fn is_num_ty(&self) -> bool {
        match self {
            Ty::Signed { .. } | Ty::Unsigned { .. } => true,
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

    pub fn get_loc(&self) -> SourceRef {
        match self {
            Ty::Signed { loc, .. }
            | Ty::Unsigned { loc, .. }
            | Ty::Str { loc }
            | Ty::Char { loc }
            | Ty::Void { loc }
            | Ty::Bool { loc }
            | Ty::Func { loc, .. }
            | Ty::StaticArray { loc, .. }
            | Ty::Slice { loc, .. }
            | Ty::Optional { loc, .. }
            | Ty::ErrorType { loc } => loc.clone(),
        }
    }

    pub fn set_loc(&mut self, n_loc: SourceRef) {
        match self {
            Ty::Signed { loc, .. }
            | Ty::Unsigned { loc, .. }
            | Ty::Str { loc }
            | Ty::Char { loc }
            | Ty::Void { loc }
            | Ty::Bool { loc }
            | Ty::Func { loc, .. }
            | Ty::StaticArray { loc, .. }
            | Ty::Slice { loc, .. }
            | Ty::Optional { loc, .. }
            | Ty::ErrorType { loc } => {
                let b_loc = loc.borrow_mut();
                *b_loc = n_loc;
            }
        }
    }

    pub fn as_str(&self) -> String {
        match self {
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
            Ty::Str { .. } => "str".into(),
            Ty::Char { .. } => "char".into(),
            Ty::Void { .. } => "void".into(),
            Ty::Bool { .. } => "bool".into(),
            Ty::Func { params, ret, .. } => {
                format!(
                    "fn ({}) {}",
                    params
                        .iter()
                        .map(|p| { p.as_str() })
                        .collect::<Vec<String>>()
                        .join(", "),
                    ret.as_str()
                )
            }
            Ty::StaticArray { sub_ty, size, .. } => {
                let size_s = if let Some(s) = size {
                    s.as_str()
                } else {
                    "_".into()
                };
                format!("[{}, {size_s}]", sub_ty.as_str())
            }
            Ty::Slice { sub_ty, .. } => format!("[{}]", sub_ty.as_str()),
            Ty::Optional { sub_ty, .. } => format!("?{}", sub_ty.as_str()),
            Ty::ErrorType { .. } => "err!".into(),
        }
    }

    pub fn clone_loc(&self, loc: SourceRef) -> Ty {
        let mut ty = self.clone();
        ty.set_loc(loc);
        ty
    }

    pub fn get_int_ty(loc: SourceRef) -> Ty {
        Ty::Signed {
            size: Ty::get_platform_size(),
            is_int: true,
            loc,
        }
    }

    pub fn get_uint_ty(loc: SourceRef) -> Ty {
        Ty::Unsigned {
            size: Ty::get_platform_size(),
            is_uint: true,
            loc,
        }
    }

    pub fn get_platform_size() -> u8 {
        if std::mem::size_of::<usize>() == 8 {
            64
        } else {
            32
        }
    }
}

impl Sig {
    pub fn is_numerical_type(&self) -> bool {
        match self {
            Sig::I8
            | Sig::I16
            | Sig::I32
            | Sig::I64
            | Sig::Int
            | Sig::U8
            | Sig::U16
            | Sig::U32
            | Sig::U64
            | Sig::UInt => true,
            _ => false,
        }
    }

    pub fn is_signed_type(&self) -> bool {
        match self {
            Sig::I8 | Sig::I16 | Sig::I32 | Sig::I64 | Sig::Int => true,
            _ => false,
        }
    }

    pub fn is_unsigned_type(&self) -> bool {
        match self {
            Sig::U8 | Sig::U16 | Sig::U32 | Sig::U64 | Sig::UInt => true,
            _ => false,
        }
    }

    pub fn is_error_type(&self) -> bool {
        match self {
            Sig::ErrorType => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Type {
    pub tag: Sig,
    pub name: Option<String>,
    pub sub_types: Vec<Type>,
    pub aux_type: Option<Box<Type>>,
    pub sub_expr: Option<Expr>,
    pub loc: SourceRef,
}

impl Type {
    pub fn new(sig: Sig, loc: SourceRef) -> Type {
        Type {
            tag: sig,
            name: None,
            sub_types: vec![],
            aux_type: None,
            sub_expr: None,
            loc,
        }
    }

    pub fn as_str(&self) -> String {
        match self.tag {
            Sig::UserDefinedType => {
                if let Some(name) = &self.name {
                    name.clone()
                } else {
                    panic!("Identifier type has no name");
                }
            }
            Sig::Bool => "bool".to_string(),
            Sig::Char => "char".to_string(),
            Sig::Void => "void".to_string(),
            Sig::Str => "str".to_string(),
            Sig::I8 => "i8".to_string(),
            Sig::I16 => "i16".to_string(),
            Sig::I32 => "i32".to_string(),
            Sig::I64 => "i64".to_string(),
            Sig::Int => "int".to_string(),
            Sig::U8 => "u8".to_string(),
            Sig::U16 => "u16".to_string(),
            Sig::U32 => "u32".to_string(),
            Sig::U64 => "u64".to_string(),
            Sig::UInt => "uint".to_string(),
            Sig::Function => {
                let mut s = "fn (".to_string();
                for (i, sub_type) in self.sub_types.iter().enumerate() {
                    s.push_str(&sub_type.as_str());
                    if i < self.sub_types.len() - 1 {
                        s.push_str(", ");
                    }
                }
                s.push_str(") ");
                s.push_str(&self.aux_type.as_ref().unwrap().as_str());
                s
            }
            Sig::StaticArray => match (&self.sub_expr, &self.aux_type) {
                (None, Some(arr_ty)) => {
                    format!("[{}, _]", arr_ty.as_str())
                }
                (Some(size), Some(arr_ty)) => {
                    format!("[{}, {}]", arr_ty.as_str(), size.as_str())
                }
                (a, b) => unreachable!(
                    "Type::as_str(): Static Array type not matching expected pattern: {a:#?}, {b:#?}"
                ),
            },
            Sig::Slice => format!("[{}]", self.aux_type.as_ref().unwrap().as_str()),
            Sig::Optional => format!("?{}", self.aux_type.as_ref().unwrap().as_str()),
            Sig::ErrorType => "<error>".to_string(),

        }
    }
}
