#![allow(dead_code)]
#![allow(unused_variables)]

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
    Infer,
}

pub struct Type {
    pub tag: Sig,
    pub name: Option<String>,
    pub sub_types: Vec<Type>,
}

impl Type {
    pub fn as_str(&self) -> String {
        match self.tag {
            Sig::Identifier => {
                if let Some(name) = &self.name {
                    name.clone()
                } else {
                    panic!("Identifier type has no name");
                }
            }
            Sig::Infer => "<infer>".to_string(),
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
                let mut s = "(".to_string();
                for (i, sub_type) in self.sub_types.iter().enumerate() {
                    s.push_str(&sub_type.as_str());
                    if i < self.sub_types.len() - 1 {
                        s.push_str(", ");
                    }
                }
                s.push_str(") -> ");
                s.push_str(&self.sub_types.last().unwrap().as_str());
                s
            }
        }
    }
}
