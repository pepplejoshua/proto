use super::{bcode::Index, source::SourceRef};

#[allow(dead_code)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TSTag {
    // inferred types
    ComptimeInt,
    Bool,
    Char,
    Void,
    Str,
    Type,

    // specifiable types
    I8,
    I16,
    I32,
    I64,
    Isize,
    U8,
    U16,
    U32,
    U64,
    Usize,
    // Function Type 5 (Type:2, Type:2, ...)
    // where the first type index is the return type of the function.
    // The remaining type indices are the types of the arguments.
    Function,
    // NameRef "name"
    // where "name" is the name of the type.
    NameRef,
    // types of type tokens written in the source file
    BoolTy,
    CharTy,
    VoidTy,
    StrTy,
    TypeTy,
    I8Ty,
    I16Ty,
    I32Ty,
    I64Ty,
    IsizeTy,
    U8Ty,
    U16Ty,
    U32Ty,
    U64Ty,
    UsizeTy,
}

#[allow(dead_code)]
impl TSTag {
    pub fn is_simple_type(&self) -> bool {
        matches!(
            self,
            TSTag::ComptimeInt
                | TSTag::I8
                | TSTag::I16
                | TSTag::I32
                | TSTag::I64
                | TSTag::Isize
                | TSTag::U8
                | TSTag::U16
                | TSTag::U32
                | TSTag::U64
                | TSTag::Usize
                | TSTag::Bool
                | TSTag::Char
                | TSTag::Void
                | TSTag::Str
        )
    }

    pub fn is_literal_type_token(&self) -> bool {
        matches!(
            self,
            TSTag::BoolTy
                | TSTag::CharTy
                | TSTag::VoidTy
                | TSTag::StrTy
                | TSTag::I8Ty
                | TSTag::I16Ty
                | TSTag::I32Ty
                | TSTag::I64Ty
                | TSTag::IsizeTy
                | TSTag::U8Ty
                | TSTag::U16Ty
                | TSTag::U32Ty
                | TSTag::U64Ty
                | TSTag::UsizeTy
                | TSTag::TypeTy
        )
    }

    pub fn accepted_numerical_type(&self) -> TSTag {
        match self {
            TSTag::ComptimeInt
            | TSTag::Bool
            | TSTag::Char
            | TSTag::Void 
            | TSTag::Str
            | TSTag::Type
            | TSTag::I8
            | TSTag::I16
            | TSTag::I32
            | TSTag::I64
            | TSTag::Isize 
            | TSTag::U8
            | TSTag::U16
            | TSTag::U32
            | TSTag::U64
            | TSTag::Usize
            | TSTag::Function
            | TSTag::BoolTy
            | TSTag::CharTy
            | TSTag::VoidTy
            | TSTag::StrTy
            | TSTag::TypeTy
            | TSTag::NameRef => unreachable!("unexpected non-numerical type token: {:?}", self),
            TSTag::I8Ty => TSTag::I8,
            TSTag::I16Ty => TSTag::I16,
            TSTag::I32Ty => TSTag::I32,
            TSTag::I64Ty => TSTag::I64,
            TSTag::IsizeTy => TSTag::Isize,
            TSTag::U8Ty => TSTag::U8,
            TSTag::U16Ty => TSTag::U16,
            TSTag::U32Ty => TSTag::U32,
            TSTag::U64Ty => TSTag::U64,
            TSTag::UsizeTy => TSTag::Usize,
        }
    }

    pub fn is_numerical_type(&self) -> bool {
        matches!(
            self,
            TSTag::ComptimeInt
                | TSTag::I8
                | TSTag::I16
                | TSTag::I32
                | TSTag::I64
                | TSTag::Isize
                | TSTag::U8
                | TSTag::U16
                | TSTag::U32
                | TSTag::U64
                | TSTag::Usize
                | TSTag::Bool
                | TSTag::Char
                | TSTag::Void
                | TSTag::Str
        )
    }
}

#[derive(Debug, Clone)]
pub struct TypeSignature {
    pub tag: TSTag,
    pub src: SourceRef,
    pub indices: Vec<Index>,
}

#[allow(dead_code)]
impl TypeSignature {
    pub fn get_source_ref(&self) -> SourceRef {
        self.src.clone()
    }
}

/*
  |---------------------> type <------------------|
  |             |               |                 |
  |---> i8      |---> char      |---> bool        |---> type, keyword
    [1, 2, 3]      ['a', 'b']     [true,false]     the type of type is also type
*/
