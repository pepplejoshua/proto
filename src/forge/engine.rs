#![allow(dead_code)]
#![allow(unused_variables)]

use crate::{
    frontend::{
        bcode::{CodeBundle, CodeTag, Index, IndexTag},
        types::{EInfo, TypeSignatureTag, ValueType, ValueTypeTag},
    },
    symbol_info::symbol_info::SymbolTable,
};

pub struct PassEngine {
    pub info: Vec<EInfo>,
}

impl PassEngine {
    pub fn new() -> Self {
        PassEngine { info: Vec::new() }
    }

    fn add_info(&mut self, info: EInfo) {
        self.info.push(info);
    }

    fn add_next_pass_info(&mut self, from: usize) {
        self.add_info(EInfo::NextPassCheck { from });
    }
}

impl PassEngine {
    pub fn run(&mut self, code: CodeBundle) {
        if !self.info.is_empty() {
            self.info.clear();
        }
        let mut sym_table = SymbolTable::new();

        for (loc, ins) in code.ins.iter().enumerate() {
            match ins.tag {
                CodeTag::LIStr => {
                    // LIStr "string"
                    let str_i = ins.data[0];
                    let from = loc;
                    self.add_info(EInfo::Str {
                        value: Some(code.get_string(&str_i)),
                        from,
                    });
                }
                CodeTag::LIChar => {
                    // LIChar 'c'
                    let char_i = ins.data[0];
                    let char = code.get_string(&char_i).chars().next().unwrap();
                    let from = loc;
                    self.add_info(EInfo::Char {
                        value: Some(char),
                        from,
                    });
                }
                CodeTag::LINum => {
                    // LINum 123
                    let num_i = ins.data[0];
                    let from = loc;
                    self.add_info(EInfo::ImmediateNum { str_i: num_i, from });
                }
                CodeTag::NewConstant => {
                    // NewConstant "name" TypeSignature:19? Info:20
                    let name_i = ins.data[0];
                    let (type_i, init_i) = if ins.data.len() == 2 {
                        (None, ins.data[1])
                    } else {
                        (Some(ins.data[1]), ins.data[2])
                    };

                    if let Some(const_ty_i) = type_i {
                    } else {
                        // get the type of the init_i value
                    }
                }
                CodeTag::EnterScope => {
                    sym_table = SymbolTable::make_child_env(sym_table);
                }
                CodeTag::ExitScope => {
                    sym_table = sym_table.return_parent_env().unwrap();
                }
                _ => {
                    unimplemented!("Pass1Engine::run: {:?}", ins.tag)
                }
            }
        }
    }

    fn to_value_type(
        &self,
        code: &CodeBundle,
        info: &EInfo,
        inference_ctx: Option<usize>,
    ) -> ValueType {
        match info {
            EInfo::ImmediateNum { str_i, from } => {
                if let Some(ctx) = inference_ctx {
                    // infer the type of the number based on the type provided
                    let ctx_ty = code.get_type_unsafe(ctx);
                    if ctx_ty.tag.is_numerical_type_sig() {
                        match ctx_ty.tag {
                            TypeSignatureTag::I8TS => {
                                let num_s = code.get_string(str_i);
                                let num = num_s.parse::<i8>();
                                if let Ok(num) = num {
                                    let code = code.get_ins(Index {
                                        tag: IndexTag::Code,
                                        index: *from,
                                    });
                                    ValueType {
                                        tag: ValueTypeTag::I8,
                                        src: code.src.clone(),
                                        data: vec![],
                                    }
                                } else {
                                    panic!("Pass1Engine::to_value_type: could not convert {num_s} to i8")
                                }
                            }
                            TypeSignatureTag::I16TS => {
                                let num_s = code.get_string(str_i);
                                let num = num_s.parse::<i16>();
                                if let Ok(num) = num {
                                    let code = code.get_ins(Index {
                                        tag: IndexTag::Code,
                                        index: *from,
                                    });
                                    ValueType {
                                        tag: ValueTypeTag::I16,
                                        src: code.src.clone(),
                                        data: vec![],
                                    }
                                } else {
                                    panic!("Pass1Engine::to_value_type: could not convert {num_s} to i16")
                                }
                            }
                            TypeSignatureTag::I32TS => {
                                let num_s = code.get_string(str_i);
                                let num = num_s.parse::<i32>();
                                if let Ok(num) = num {
                                    let code = code.get_ins(Index {
                                        tag: IndexTag::Code,
                                        index: *from,
                                    });
                                    ValueType {
                                        tag: ValueTypeTag::I32,
                                        src: code.src.clone(),
                                        data: vec![],
                                    }
                                } else {
                                    panic!("Pass1Engine::to_value_type: could not convert {num_s} to i32")
                                }
                            }
                            TypeSignatureTag::I64TS => {
                                let num_s = code.get_string(str_i);
                                let num = num_s.parse::<i64>();
                                if let Ok(num) = num {
                                    let code = code.get_ins(Index {
                                        tag: IndexTag::Code,
                                        index: *from,
                                    });
                                    ValueType {
                                        tag: ValueTypeTag::I64,
                                        src: code.src.clone(),
                                        data: vec![],
                                    }
                                } else {
                                    panic!("Pass1Engine::to_value_type: could not convert {num_s} to i64")
                                }
                            }
                            TypeSignatureTag::IntTS => {
                                let num_s = code.get_string(str_i);
                                let num = num_s.parse::<isize>();
                                if let Ok(num) = num {
                                    let code = code.get_ins(Index {
                                        tag: IndexTag::Code,
                                        index: *from,
                                    });
                                    ValueType {
                                        tag: ValueTypeTag::Int,
                                        src: code.src.clone(),
                                        data: vec![],
                                    }
                                } else {
                                    panic!("Pass1Engine::to_value_type: could not convert {num_s} to i64")
                                }
                            }
                            TypeSignatureTag::U8TS => {
                                let num_s = code.get_string(str_i);
                                let num = num_s.parse::<u8>();
                                if let Ok(num) = num {
                                    let code = code.get_ins(Index {
                                        tag: IndexTag::Code,
                                        index: *from,
                                    });
                                    ValueType {
                                        tag: ValueTypeTag::U8,
                                        src: code.src.clone(),
                                        data: vec![],
                                    }
                                } else {
                                    panic!("Pass1Engine::to_value_type: could not convert {num_s} to u8")
                                }
                            }
                            TypeSignatureTag::U16TS => {
                                let num_s = code.get_string(str_i);
                                let num = num_s.parse::<u16>();
                                if let Ok(num) = num {
                                    let code = code.get_ins(Index {
                                        tag: IndexTag::Code,
                                        index: *from,
                                    });
                                    ValueType {
                                        tag: ValueTypeTag::U16,
                                        src: code.src.clone(),
                                        data: vec![],
                                    }
                                } else {
                                    panic!("Pass1Engine::to_value_type: could not convert {num_s} to u16")
                                }
                            }
                            TypeSignatureTag::U32TS => {
                                let num_s = code.get_string(str_i);
                                let num = num_s.parse::<u32>();
                                if let Ok(num) = num {
                                    let code = code.get_ins(Index {
                                        tag: IndexTag::Code,
                                        index: *from,
                                    });
                                    ValueType {
                                        tag: ValueTypeTag::U32,
                                        src: code.src.clone(),
                                        data: vec![],
                                    }
                                } else {
                                    panic!("Pass1Engine::to_value_type: could not convert {num_s} to u32")
                                }
                            }
                            TypeSignatureTag::U64TS => {
                                let num_s = code.get_string(str_i);
                                let num = num_s.parse::<u64>();
                                if let Ok(num) = num {
                                    let code = code.get_ins(Index {
                                        tag: IndexTag::Code,
                                        index: *from,
                                    });
                                    ValueType {
                                        tag: ValueTypeTag::U64,
                                        src: code.src.clone(),
                                        data: vec![],
                                    }
                                } else {
                                    panic!("Pass1Engine::to_value_type: could not convert {num_s} to u64")
                                }
                            }
                            TypeSignatureTag::UIntTS => {
                                let num_s = code.get_string(str_i);
                                let num = num_s.parse::<usize>();
                                if let Ok(num) = num {
                                    let code = code.get_ins(Index {
                                        tag: IndexTag::Code,
                                        index: *from,
                                    });
                                    ValueType {
                                        tag: ValueTypeTag::UInt,
                                        src: code.src.clone(),
                                        data: vec![],
                                    }
                                } else {
                                    panic!("Pass1Engine::to_value_type: could not convert {num_s} to usize")
                                }
                            }
                            _ => unreachable!(
                                "Pass1Engine::to_value_type: {ctx_ty} is not a numerical type",
                                ctx_ty = code.type_as_str_unsafe(ctx)
                            ),
                        }
                    } else {
                        let ty_s = code.type_as_str_unsafe(ctx);
                        panic!("Pass1Engine::to_value_type: {ty_s} is not a numerical type")
                    }
                } else {
                    // try to convert the number to i32
                    let num_str = code.get_string(str_i);
                    let num = num_str.parse::<isize>();
                    if let Ok(num) = num {
                        let code = code.get_ins(Index {
                            tag: IndexTag::Code,
                            index: *from,
                        });
                        ValueType {
                            tag: ValueTypeTag::Int,
                            src: code.src.clone(),
                            data: vec![],
                        }
                    } else {
                        panic!("Pass1Engine::to_value_type: could not convert {num_str} to i32")
                    }
                }
            }
            EInfo::ReferenceToType { type_i, from } => {
                let ty = code.get_type(type_i);
                ValueType {
                    tag: ValueTypeTag::Type,
                    src: ty.src,
                    data: vec![*type_i],
                }
            }
            EInfo::StaticArray {
                item_type_i,
                from,
                items,
            } => {
                let code_ = code.get_ins_unsafe(*from);
                ValueType {
                    tag: ValueTypeTag::StaticArray,
                    src: code_.src.clone(),
                    data: vec![*item_type_i],
                }
            }
            EInfo::Bool { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                ValueType {
                    tag: ValueTypeTag::Bool,
                    src: code.src.clone(),
                    data: vec![],
                }
            }
            EInfo::Char { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                ValueType {
                    tag: ValueTypeTag::Char,
                    src: code.src.clone(),
                    data: vec![],
                }
            }
            EInfo::Void { from } => {
                let code = code.get_ins_unsafe(*from);
                ValueType {
                    tag: ValueTypeTag::Void,
                    src: code.src.clone(),
                    data: vec![],
                }
            }
            EInfo::Str { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                ValueType {
                    tag: ValueTypeTag::Str,
                    src: code.src.clone(),
                    data: vec![],
                }
            }
            EInfo::I8 { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                ValueType {
                    tag: ValueTypeTag::I8,
                    src: code.src.clone(),
                    data: vec![],
                }
            }
            EInfo::I16 { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                ValueType {
                    tag: ValueTypeTag::I16,
                    src: code.src.clone(),
                    data: vec![],
                }
            }
            EInfo::I32 { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                ValueType {
                    tag: ValueTypeTag::I32,
                    src: code.src.clone(),
                    data: vec![],
                }
            }
            EInfo::I64 { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                ValueType {
                    tag: ValueTypeTag::I64,
                    src: code.src.clone(),
                    data: vec![],
                }
            }
            EInfo::Int { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                ValueType {
                    tag: ValueTypeTag::Int,
                    src: code.src.clone(),
                    data: vec![],
                }
            }
            EInfo::U8 { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                ValueType {
                    tag: ValueTypeTag::U8,
                    src: code.src.clone(),
                    data: vec![],
                }
            }
            EInfo::U16 { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                ValueType {
                    tag: ValueTypeTag::U16,
                    src: code.src.clone(),
                    data: vec![],
                }
            }
            EInfo::U32 { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                ValueType {
                    tag: ValueTypeTag::U32,
                    src: code.src.clone(),
                    data: vec![],
                }
            }
            EInfo::U64 { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                ValueType {
                    tag: ValueTypeTag::U64,
                    src: code.src.clone(),
                    data: vec![],
                }
            }
            EInfo::UInt { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                ValueType {
                    tag: ValueTypeTag::UInt,
                    src: code.src.clone(),
                    data: vec![],
                }
            }
            EInfo::NoInfo => {
                panic!("Pass1Engine::to_value_type: NoInfo")
            }
            EInfo::Function {
                fn_type_i, from, ..
            } => {
                let code_ = code.get_ins_unsafe(*from);
                let fn_type = code.get_type(fn_type_i);
                ValueType {
                    tag: ValueTypeTag::Function,
                    src: code_.src.clone(),
                    data: fn_type.indices.clone(),
                }
            }
            EInfo::Error { msg, from } => {
                panic!("Pass1Engine::to_value_type: {msg}")
            }
            EInfo::NextPassCheck { from } => {
                panic!("Pass1Engine::to_value_type: NextPassCheck")
            }
        }
    }
}
