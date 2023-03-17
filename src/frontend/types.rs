#[derive(Debug, Clone)]
pub enum Type {
    I8,
    I16,
    I32,
    I64,
    ISize,
    U8,
    U16,
    U32,
    U64,
    USize,
    Bool,
    Char,
}

impl Type {
    fn as_str(&self) -> String {
        match &self {
            Type::I8 => "i8".into(),
            Type::I16 => "i16".into(),
            Type::I32 => "i32".into(),
            Type::I64 => "i64".into(),
            Type::ISize => "isize".into(),
            Type::U8 => "u8".into(),
            Type::U16 => "u16".into(),
            Type::U32 => "u32".into(),
            Type::U64 => "u64".into(),
            Type::USize => "usize".into(),
            Type::Bool => "bool".into(),
            Type::Char => "char".into(),
        }
    }
}
