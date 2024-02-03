#![allow(dead_code)]

// this is parsed from user source code and is used to inform the
// generation of the above and the type checking as well
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
    IntTS,
    U8TS,
    U16TS,
    U32TS,
    U64TS,
    UIntTS,
    // FunctionTS return_ty (arg_ty1, arg_ty2, ...)
    FunctionTS,
    // StaticArrayTS size inner_type
    StaticArrayTS,
}

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
            TypeSignatureTag::IntTS => ValueTypeTag::Int,
            TypeSignatureTag::U8TS => ValueTypeTag::U8,
            TypeSignatureTag::U16TS => ValueTypeTag::U16,
            TypeSignatureTag::U32TS => ValueTypeTag::U32,
            TypeSignatureTag::U64TS => ValueTypeTag::U64,
            TypeSignatureTag::UIntTS => ValueTypeTag::UInt,
            TypeSignatureTag::FunctionTS => ValueTypeTag::Type,
            TypeSignatureTag::StaticArrayTS => ValueTypeTag::StaticArray,
        }
    }

    pub fn is_numerical_type_sig(&self) -> bool {
        matches!(
            self,
            TypeSignatureTag::I8TS
                | TypeSignatureTag::I16TS
                | TypeSignatureTag::I32TS
                | TypeSignatureTag::I64TS
                | TypeSignatureTag::IntTS
                | TypeSignatureTag::U8TS
                | TypeSignatureTag::U16TS
                | TypeSignatureTag::U32TS
                | TypeSignatureTag::U64TS
                | TypeSignatureTag::UIntTS
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

impl TypeSignature {
    pub fn get_source_ref(&self) -> SourceRef {
        self.src.clone()
    }
}

// this is generated by the compiler from expressions
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
    Int,
    U8,
    U16,
    U32,
    U64,
    UInt,
    // Function return_ty (arg_ty1, arg_ty2, ...)
    Function,
    // StaticArray inner_type
    StaticArray,
}

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
            ValueTypeTag::Int => TypeSignatureTag::IntTS,
            ValueTypeTag::U8 => TypeSignatureTag::U8TS,
            ValueTypeTag::U16 => TypeSignatureTag::U16TS,
            ValueTypeTag::U32 => TypeSignatureTag::U32TS,
            ValueTypeTag::U64 => TypeSignatureTag::U64TS,
            ValueTypeTag::UInt => TypeSignatureTag::UIntTS,
            ValueTypeTag::Function => TypeSignatureTag::FunctionTS,
            ValueTypeTag::StaticArray => TypeSignatureTag::StaticArrayTS,
        }
    }

    pub fn is_numerical_value_type(&self) -> bool {
        matches!(
            self,
            ValueTypeTag::I8
                | ValueTypeTag::I16
                | ValueTypeTag::I32
                | ValueTypeTag::I64
                | ValueTypeTag::Int
                | ValueTypeTag::U8
                | ValueTypeTag::U16
                | ValueTypeTag::U32
                | ValueTypeTag::U64
                | ValueTypeTag::UInt
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

impl ValueType {
    pub fn get_source_ref(&self) -> SourceRef {
        self.src.clone()
    }
}

#[derive(Debug, Clone)]
pub enum EInfo {
    // store the string and the index it originated from
    ImmediateNum {
        str_i: Index,
        from: usize,
    },
    ReferenceToType {
        type_i: Index,
        from: usize,
    },
    StaticArray {
        item_type_i: Index,
        from: usize,
        items: Vec<Index>,
    },
    Bool {
        value: Option<bool>, // this will be None if the value is not known
        from: usize,
    },
    Char {
        value: Option<char>, // this will be None if the value is not known
        from: usize,
    },
    Void {
        from: usize,
    },
    Type {
        from: usize,
    },
    Str {
        value: Option<String>, // this will be None if the value is not known
        from: usize,
    },
    I8 {
        value: Option<i8>, // this will be None if the value is not known
        from: usize,
    },
    I16 {
        value: Option<i16>, // this will be None if the value is not known
        from: usize,
    },
    I32 {
        value: Option<i32>, // this will be None if the value is not known
        from: usize,
    },
    I64 {
        value: Option<i64>, // this will be None if the value is not known
        from: usize,
    },
    Int {
        value: Option<isize>, // this will be None if the value is not known
        from: usize,
    },
    U8 {
        value: Option<u8>, // this will be None if the value is not known
        from: usize,
    },
    U16 {
        value: Option<u16>, // this will be None if the value is not known
        from: usize,
    },
    U32 {
        value: Option<u32>, // this will be None if the value is not known
        from: usize,
    },
    U64 {
        value: Option<u64>, // this will be None if the value is not known
        from: usize,
    },
    UInt {
        value: Option<usize>, // this will be None if the value is not known
        from: usize,
    },
    NoInfo,
    Function {
        fn_ret_type_i: Index,
        from: usize,
    },
    Error {
        msg: String,
        from: usize,
    },
    NextPassCheck {
        from: usize,
    },
}

impl EInfo {
    pub fn is_next_pass_check(&self) -> bool {
        matches!(self, EInfo::NextPassCheck { .. })
    }
}