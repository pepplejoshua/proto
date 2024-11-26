#![allow(dead_code)]
#![allow(unused_variables)]

use crate::parser::ast::Expr;
use crate::source::source::SourceRef;
use std::borrow::BorrowMut;
use std::{collections::HashMap, rc::Rc};

#[derive(Debug, Clone)]
pub enum Ty {
    // the type assigned to integer literals which can be coerced
    // into other integer types (untyped or typed)
    UntypedInt {
        literal: usize,
        loc: Rc<SourceRef>,
    },
    // the type assigned to floating-point literals which can be coerced
    // into other float types (f32 or f64)
    // UntypedFloat {
    //     literal: f64,
    //     loc: Rc<SourceRef>,
    // },
    // i8, i16, i32, i64
    SignedInt {
        size: u8,
        loc: Rc<SourceRef>,
    },
    // u8, u16, u32, u64
    UnsignedInt {
        size: u8,
        loc: Rc<SourceRef>,
    },
    // f32, f64
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
        static_funcs: HashMap<String, Rc<Ty>>,
        methods: HashMap<String, Rc<Ty>>,
        loc: Rc<SourceRef>,
    },
    NamedType {
        name: Rc<String>,
        loc: Rc<SourceRef>,
    },
    AccessMemberType {
        target: Rc<Ty>,
        mem: Rc<Ty>,
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

    pub fn is_bool_ty(&self) -> bool {
        match self {
            Ty::Bool { .. } => true,
            _ => false,
        }
    }

    pub fn is_num_ty(&self) -> bool {
        match self {
            Ty::SignedInt { .. }
            | Ty::UnsignedInt { .. }
            | Ty::UntypedInt { .. }
            | Ty::Float { .. } => true,
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
            Ty::SignedInt { .. } => true,
            _ => false,
        }
    }

    pub fn is_unsigned_ty(&self) -> bool {
        match self {
            Ty::UnsignedInt { .. } => true,
            _ => false,
        }
    }

    pub fn is_untyped_int_ty(&self) -> bool {
        match self {
            Ty::UntypedInt { .. } => true,
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
            Ty::SignedInt { loc, .. }
            | Ty::UntypedInt { loc, .. }
            | Ty::UnsignedInt { loc, .. }
            | Ty::Str { loc, .. }
            | Ty::Char { loc }
            | Ty::Void { loc }
            | Ty::Bool { loc }
            | Ty::Func { loc, .. }
            | Ty::AccessMemberType { loc, .. }
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
            Ty::SignedInt { loc, .. }
            | Ty::UntypedInt { loc, .. }
            | Ty::UnsignedInt { loc, .. }
            | Ty::Str { loc, .. }
            | Ty::Char { loc }
            | Ty::Void { loc }
            | Ty::Bool { loc }
            | Ty::Func { loc, .. }
            | Ty::AccessMemberType { loc, .. }
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

    pub fn as_str(&self) -> String {
        match self {
            Ty::UntypedInt { literal, .. } => {
                format!("untyped_int[{literal}]")
            }
            Ty::SignedInt { size, .. } => {
                format!("i{size}")
            }
            Ty::UnsignedInt { size, .. } => {
                format!("u{size}")
            }
            Ty::Float { size, loc } => {
                format!("f{size}")
            }
            Ty::Str { .. } => "str".into(),
            Ty::Char { .. } => "char".into(),
            Ty::Void { .. } => "void".into(),
            Ty::Bool { .. } => "bool".into(),
            Ty::AccessMemberType { target, mem, .. } => {
                format!("{}.{}", target.as_str(), mem.as_str())
            }
            Ty::Func { params, ret, .. } => {
                format!(
                    "\\({}) {}",
                    params
                        .iter()
                        .map(|p| { p.as_str() })
                        .collect::<Vec<String>>()
                        .join(", "),
                    ret.as_str()
                )
            }
            Ty::StaticArray { sub_ty, size, .. } => {
                let size_s = size.as_str();
                format!("[{size_s}]{}", sub_ty.as_str())
            }
            Ty::Slice { sub_ty, .. } => format!("[]{}", sub_ty.as_str()),
            Ty::Optional { sub_ty, .. } => format!("?{}", sub_ty.as_str()),
            Ty::NamedType { name, loc } => name.as_ref().clone(),
            Ty::Struct { .. } => "type".to_string(),
            Ty::Pointer { sub_ty, .. } => format!("*{}", sub_ty.as_str()),
            Ty::Tuple { sub_tys, .. } => {
                format!(
                    "({})",
                    sub_tys
                        .iter()
                        .map(|ty| { ty.as_str() })
                        .collect::<Vec<String>>()
                        .join(", ")
                )
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
        Rc::new(Ty::SignedInt {
            size: Ty::get_platform_size(),
            loc,
        })
    }

    pub fn get_uint_ty(loc: Rc<SourceRef>) -> Rc<Ty> {
        Rc::new(Ty::UnsignedInt {
            size: Ty::get_platform_size(),
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
