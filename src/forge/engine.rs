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
                            _ => todo!(),
                        }
                    } else {
                        let ty_s = code.type_as_str_unsafe(ctx);
                        panic!("Pass1Engine::to_value_type: {ty_s} is not a numerical type")
                    }
                } else {
                    // try to convert the number to i32
                    let num_str = code.get_string(str_i);
                    let num = num_str.parse::<i32>();
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
                        panic!("Pass1Engine::to_value_type: could not convert {num_str} to i32")
                    }
                }
            }
            EInfo::ReferenceToType { type_i, from } => todo!(),
            EInfo::StaticArray {
                item_type_i,
                from,
                items,
            } => todo!(),
            EInfo::Bool { value, from } => todo!(),
            EInfo::Char { value, from } => todo!(),
            EInfo::Void { from } => todo!(),
            EInfo::Str { value, from } => todo!(),
            EInfo::I8 { value, from } => todo!(),
            EInfo::I16 { value, from } => todo!(),
            EInfo::I32 { value, from } => todo!(),
            EInfo::I64 { value, from } => todo!(),
            EInfo::Int { value, from } => todo!(),
            EInfo::U8 { value, from } => todo!(),
            EInfo::U16 { value, from } => todo!(),
            EInfo::U32 { value, from } => todo!(),
            EInfo::U64 { value, from } => todo!(),
            EInfo::UInt { value, from } => todo!(),
            EInfo::NoInfo => {
                panic!("Pass1Engine::to_value_type: NoInfo")
            }
            EInfo::Function {
                name,
                fn_type_i,
                fn_start_index,
                fn_end_index,
                from,
            } => todo!(),
            EInfo::Error { msg, from } => {
                panic!("Pass1Engine::to_value_type: {msg}")
            }
            EInfo::NextPassCheck { from } => {
                panic!("Pass1Engine::to_value_type: NextPassCheck")
            }
        }
    }
}
