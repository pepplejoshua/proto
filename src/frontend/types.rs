use super::{bcode::Index, source::SourceRef};

// this is parsed from user source code and is used to inform the 
// generation of the above and the type checking as well
#[allow(dead_code)]
#[derive(Debug, Clone, Hash, PartialEq, Eq, Copy)]
pub enum TypeSignatureTag {
    // TypeAlias "name"
    // where "name" is the name of the type.
    TypeNameRefTS,
    BoolTS,
    CharTS,
    VoidTS,
    StrTS,
    TypeTS,
    I8TS,
    I16TS,
    I32TS,
    I64TS,
    IsizeTS,
    U8TS,
    U16TS,
    U32TS,
    U64TS,
    UsizeTS,
    // FunctionTS return_ty (arg_ty1, arg_ty2, ...)
    FunctionTS,
}

#[allow(dead_code)]
impl TypeSignatureTag {
    pub fn to_value_type_tag(&self) -> ValueTypeTag {
        match self {
            TypeSignatureTag::TypeNameRefTS => ValueTypeTag::TypeNameRef,
            TypeSignatureTag::BoolTS => ValueTypeTag::Bool,
            TypeSignatureTag::CharTS => ValueTypeTag::Char,
            TypeSignatureTag::VoidTS => ValueTypeTag::Void,
            TypeSignatureTag::StrTS => ValueTypeTag::Str,
            TypeSignatureTag::TypeTS => ValueTypeTag::Type,
            TypeSignatureTag::I8TS => ValueTypeTag::I8,
            TypeSignatureTag::I16TS => ValueTypeTag::I16,
            TypeSignatureTag::I32TS => ValueTypeTag::I32,
            TypeSignatureTag::I64TS => ValueTypeTag::I64,
            TypeSignatureTag::IsizeTS => ValueTypeTag::Isize,
            TypeSignatureTag::U8TS => ValueTypeTag::U8,
            TypeSignatureTag::U16TS => ValueTypeTag::U16,
            TypeSignatureTag::U32TS => ValueTypeTag::U32,
            TypeSignatureTag::U64TS => ValueTypeTag::U64,
            TypeSignatureTag::UsizeTS => ValueTypeTag::Usize,
            TypeSignatureTag::FunctionTS => ValueTypeTag::Type,
        }
    }

    pub fn is_numerical_type_sig(&self) -> bool {
        matches!(
            self,
            TypeSignatureTag::I8TS
                | TypeSignatureTag::I16TS
                | TypeSignatureTag::I32TS
                | TypeSignatureTag::I64TS
                | TypeSignatureTag::IsizeTS
                | TypeSignatureTag::U8TS
                | TypeSignatureTag::U16TS
                | TypeSignatureTag::U32TS
                | TypeSignatureTag::U64TS
                | TypeSignatureTag::UsizeTS
        )
    }

    pub fn is_simple_type_sig(&self) -> bool {
        matches!(
            self,
            TypeSignatureTag::BoolTS
                | TypeSignatureTag::CharTS
                | TypeSignatureTag::VoidTS
                | TypeSignatureTag::StrTS
                | TypeSignatureTag::TypeTS
        ) || self.is_numerical_type_sig()
    }
}

#[derive(Debug, Clone)]
pub struct TypeSignature {
    pub tag: TypeSignatureTag,
    pub src: SourceRef,
    pub indices: Vec<Index>,
}

#[allow(dead_code)]
impl TypeSignature {
    pub fn get_source_ref(&self) -> SourceRef {
        self.src.clone()
    }
}

// this is generated by the compiler from expressions
#[allow(dead_code)]
#[derive(Debug, Clone, Hash, PartialEq, Eq, Copy)]
pub enum ValueTypeTag {
    TypeNameRef,
    Bool,
    Char,
    Void,
    Str,
    Type,
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
}

#[allow(dead_code)]
impl ValueTypeTag {
    pub fn to_type_signature_tag(&self) -> TypeSignatureTag {
        match self {
            ValueTypeTag::TypeNameRef => TypeSignatureTag::TypeNameRefTS,
            ValueTypeTag::Bool => TypeSignatureTag::BoolTS,
            ValueTypeTag::Char => TypeSignatureTag::CharTS,
            ValueTypeTag::Void => TypeSignatureTag::VoidTS,
            ValueTypeTag::Str => TypeSignatureTag::StrTS,
            ValueTypeTag::Type => TypeSignatureTag::TypeTS,
            ValueTypeTag::I8 => TypeSignatureTag::I8TS,
            ValueTypeTag::I16 => TypeSignatureTag::I16TS,
            ValueTypeTag::I32 => TypeSignatureTag::I32TS,
            ValueTypeTag::I64 => TypeSignatureTag::I64TS,
            ValueTypeTag::Isize => TypeSignatureTag::IsizeTS,
            ValueTypeTag::U8 => TypeSignatureTag::U8TS,
            ValueTypeTag::U16 => TypeSignatureTag::U16TS,
            ValueTypeTag::U32 => TypeSignatureTag::U32TS,
            ValueTypeTag::U64 => TypeSignatureTag::U64TS,
            ValueTypeTag::Usize => TypeSignatureTag::UsizeTS,
        }
    }

    pub fn is_numerical_value_type(&self) -> bool {
        matches!(
            self,
            ValueTypeTag::I8
                | ValueTypeTag::I16
                | ValueTypeTag::I32
                | ValueTypeTag::I64
                | ValueTypeTag::Isize
                | ValueTypeTag::U8
                | ValueTypeTag::U16
                | ValueTypeTag::U32
                | ValueTypeTag::U64
                | ValueTypeTag::Usize
        )
    }

    pub fn is_simple_value_type(&self) -> bool {
        matches!(
            self,
            ValueTypeTag::Bool
                | ValueTypeTag::Char
                | ValueTypeTag::Void
                | ValueTypeTag::Str
                | ValueTypeTag::Type
        ) || self.is_numerical_value_type()
    }
}

#[derive(Debug, Clone)]
pub struct ValueType {
    pub tag: ValueTypeTag,
    pub src: SourceRef,
    pub data: Vec<Index>,
}

#[allow(dead_code)]
impl ValueType {
    pub fn get_source_ref(&self) -> SourceRef {
        self.src.clone()
    }
}
