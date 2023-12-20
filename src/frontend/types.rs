use super::{bcode::Index, source::SourceRef};

#[allow(dead_code)]
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TSTag {
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
    Bool,
    Char,
    Void,
    Str,
    // This will cover the type of a type.
    // For example, the type of i32 is Type.
    // The type of type is Type.
    // Type Type:2?
    // where Type:2 is the type of the inner type.
    Type,
    // SizedArray "size" Type:2
    // where Type:2 is the type of the elements in the array.
    // The size of the array is stored as a string in the first
    // index of the indices array.
    SizedArray,
    // UnsizedArray Type:2
    // where Type:2 is the type of the elements in the array.
    Array,
    // NameRef "name"
    // where "name" is the name of the type.
    NameRef,
    // Function Type 5 (Type:2, Type:2, ...)
    // where the first type index is the return type of the function.
    // The remaining type indices are the types of the arguments.
    Function,
}

impl TSTag {
    pub fn is_simple_type(&self) -> bool {
        matches!(
            self,
            TSTag::I8
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
