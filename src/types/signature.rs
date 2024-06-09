#![allow(dead_code)]
#![allow(unused_variables)]

use crate::source::source::SourceRef;

// this is parsed from user source code and is used to inform the
// generation of the above and the type checking as well
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum Sig {
    Identifier,
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
    ErrorType,
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

    pub fn is_simple_type(&self) -> bool {
        match self {
            Sig::Bool | Sig::Char | Sig::Void | Sig::Str => true,
            ty if ty.is_numerical_type() => true,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub tag: Sig,
    pub name: Option<String>,
    pub sub_types: Vec<Type>,
    pub aux_type: Option<Box<Type>>,
    pub loc: SourceRef,
}

impl Type {
    pub fn new(sig: Sig, loc: SourceRef) -> Type {
        Type {
            tag: sig,
            name: None,
            sub_types: vec![],
            aux_type: None,
            loc,
        }
    }

    pub fn as_str(&self) -> String {
        match self.tag {
            Sig::Identifier => {
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
            Sig::ErrorType => "<error>".to_string(),
        }
    }
}
