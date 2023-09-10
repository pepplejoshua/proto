#![allow(dead_code)]
use crate::frontend::{ast::TypeReference, source::SourceRef};

pub enum ITag {
    String,
    UIR,
    TypeReference,
}

pub struct Index {
    pub tag: ITag,
    pub loc: usize,
}

#[derive(Debug)]
pub enum Tag {
    // add_fn arg_count "name" ret_type [arg_type_index...]
    AddFn,
    // param "name" type_index
    Param,
    // name_ref "name"
    NameRef,
    // add value_index value_index
    OpAdd,
    // cast_to type_index value_index
    CastTo,
    // uninit
    UninitializedValue,
    // const "name" type_index value_index
    Const,
    // ty_ref type_index
    TyRef,
    // int string_index
    Int,
    // call "name" [arg_index...]
    Call,
}

pub struct UIRIns {
    pub tag: Tag,
    pub aux_data: usize,
    pub src: Option<SourceRef>,
    pub indices: Vec<Index>,
}

pub struct Bundle {
    types: Vec<TypeReference>,
    uir: Vec<UIRIns>,
    strings: Vec<String>,
    pub top_level_uir: Vec<Index>,
}

impl Bundle {
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
            uir: Vec::new(),
            strings: Vec::new(),
            top_level_uir: Vec::new(),
        }
    }

    pub fn intern_string(&mut self, s: String) -> Index {
        let idx = self.strings.len();
        self.strings.push(s);
        Index {
            tag: ITag::String,
            loc: idx,
        }
    }

    pub fn intern_type(&mut self, t: TypeReference) -> Index {
        for (idx, ty) in self.types.iter().enumerate() {
            if ty == &t {
                return Index {
                    tag: ITag::TypeReference,
                    loc: idx,
                };
            }
        }
        let idx = self.types.len();
        self.types.push(t);
        Index {
            tag: ITag::TypeReference,
            loc: idx,
        }
    }

    pub fn intern_uir(&mut self, u: UIRIns) -> Index {
        let idx = self.uir.len();
        self.uir.push(u);
        Index {
            tag: ITag::UIR,
            loc: idx,
        }
    }

    pub fn get_string(&self, idx: &Index) -> &str {
        match idx.tag {
            ITag::String => &self.strings[idx.loc],
            _ => unreachable!("get_string called on non-string index"),
        }
    }

    pub fn get_type(&self, idx: &Index) -> &TypeReference {
        match idx.tag {
            ITag::TypeReference => &self.types[idx.loc],
            _ => unreachable!("get_type called on non-type index"),
        }
    }

    pub fn get_uir(&self, idx: &Index) -> &UIRIns {
        match idx.tag {
            ITag::UIR => &self.uir[idx.loc],
            _ => unreachable!("get_uir called on non-uir index"),
        }
    }

    pub fn show(&self) {
        for (index, ins) in self.uir.iter().enumerate() {
            let str = self.show_ins(ins);
            println!("{}. {}", index, str);
        }
    }

    pub fn show_ins(&self, ins: &UIRIns) -> String {
        // println!("tag: {:#?}", ins.tag);
        match ins.tag {
            Tag::Int => {
                format!("int {}", self.get_string(&ins.indices[0]))
            }
            Tag::Param => {
                format!(
                    "param \"{}\" {}",
                    self.get_string(&ins.indices[0]),
                    self.get_type(&ins.indices[1]).as_str()
                )
            }
            Tag::TyRef => {
                format!("ty_ref {}", self.get_type(&ins.indices[0]).as_str())
            }
            Tag::NameRef => {
                format!("name_ref \"{}\"", self.get_string(&ins.indices[0]))
            }
            Tag::OpAdd => {
                format!("add {} {}", ins.indices[0].loc, ins.indices[1].loc)
            }
            Tag::CastTo => {
                let ty = self.get_type(&ins.indices[0]);
                format!("cast_to {} {}", ty.as_str(), ins.indices[1].loc)
            }
            Tag::UninitializedValue => "uninitialized_value".to_string(),
            Tag::Const => {
                let ty = self.get_type(&ins.indices[1]);
                format!(
                    "const \"{}\" {} {}",
                    self.get_string(&ins.indices[0]),
                    ty.as_str(),
                    ins.indices[2].loc
                )
            }
            _ => {
                todo!("unimplemented instruction")
            }
        }
    }
}
