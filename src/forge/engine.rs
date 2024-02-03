#![allow(dead_code)]
#![allow(unused_variables)]

use crate::{
    frontend::{
        bcode::{CodeBundle, CodeTag, Index, IndexTag},
        types::{EInfo, TypeSignature, TypeSignatureTag, ValueType, ValueTypeTag},
    },
    symbol_info::symbol_info::SymbolTable,
};

pub struct PassEngine {
    pub info: Vec<EInfo>,
    pub needs_another_pass: bool,
}

impl PassEngine {
    pub fn new() -> Self {
        PassEngine {
            info: Vec::new(),
            needs_another_pass: false,
        }
    }

    fn add_info(&mut self, info: EInfo) {
        self.info.push(info);
    }

    fn add_no_info(&mut self) {
        self.add_info(EInfo::NoInfo);
    }

    fn add_next_pass_info(&mut self, from: usize) {
        self.needs_another_pass = true;
        self.add_info(EInfo::NextPassCheck { from });
    }

    fn verify_type_sig(&self, code: &CodeBundle, sym_tab: &SymbolTable, type_sig: &Index) -> bool {
        let type_sig = &code.types[type_sig.index];
        if type_sig.tag.is_simple_type_sig() {
            true
        } else {
            match type_sig.tag {
                TypeSignatureTag::TypeNameRefTS => {
                    let name = code.get_string(&type_sig.indices[0]);
                    sym_tab.check_name(&name)
                }
                TypeSignatureTag::StaticArrayTS => {
                    let item_type_i = type_sig.indices[0];
                    self.verify_type_sig(code, sym_tab, &item_type_i)
                }
                _ => false,
            }
        }
    }

    fn verify_value_type(
        &self,
        code: &CodeBundle,
        sym_tab: &SymbolTable,
        val_ty: &ValueType,
    ) -> bool {
        if val_ty.tag.is_simple_value_type() {
            true
        } else {
            match val_ty.tag {
                ValueTypeTag::TypeNameRef => {
                    let name = code.get_string(&val_ty.data[0]);
                    sym_tab.check_name(&name)
                }
                ValueTypeTag::StaticArray => {
                    let type_i = val_ty.data[0];
                    self.verify_type_sig(code, sym_tab, &type_i)
                }
                _ => false,
            }
        }
    }

    pub fn show_info(&self, code: &CodeBundle) {
        for (i, info) in self.info.iter().enumerate() {
            match info {
                EInfo::ImmediateNum { str_i, from } => {
                    println!(
                        "info[{}]: ImmediateNum: {} from: {}",
                        i,
                        code.get_string(str_i),
                        from
                    );
                }
                EInfo::ReferenceToType { type_i, from } => {
                    println!(
                        "info[{}]: ReferenceToType: {} from: {}",
                        i,
                        code.type_as_str(type_i),
                        from
                    );
                }
                EInfo::StaticArray {
                    item_type_i,
                    from,
                    items,
                } => {
                    let items_str = items.iter().map(|item| format!("{}", item.index));
                    let items_str = items_str.collect::<Vec<String>>().join(", ");
                    println!(
                        "info[{}]: StaticArray: {} from: {} items: [{}]",
                        i,
                        code.type_as_str(item_type_i),
                        from,
                        items_str
                    );
                }
                EInfo::Bool { value, from } => {
                    if let Some(value) = value {
                        println!("info[{}]: Bool: {} from: {}", i, value, from);
                    } else {
                        println!("info[{}]: Bool: Dyn from: {}", i, from);
                    }
                }
                EInfo::Char { value, from } => {
                    if let Some(value) = value {
                        println!("info[{}]: Char: {} from: {}", i, value, from);
                    } else {
                        println!("info[{}]: Char: Dyn from: {}", i, from);
                    }
                }
                EInfo::Void { from } => {
                    println!("info[{}]: Void from: {}", i, from);
                }
                EInfo::Str { value, from } => {
                    if let Some(value) = value {
                        println!("info[{}]: Str: {} from: {}", i, value, from);
                    } else {
                        println!("info[{}]: Str: Dyn from: {}", i, from);
                    }
                }
                EInfo::I8 { value, from } => {
                    if let Some(value) = value {
                        println!("info[{}]: I8: {} from: {}", i, value, from);
                    } else {
                        println!("info[{}]: I8: Dyn from: {}", i, from);
                    }
                }
                EInfo::I16 { value, from } => {
                    if let Some(value) = value {
                        println!("info[{}]: I16: {} from: {}", i, value, from);
                    } else {
                        println!("info[{}]: I16: Dyn from: {}", i, from);
                    }
                }
                EInfo::I32 { value, from } => {
                    if let Some(value) = value {
                        println!("info[{}]: I32: {} from: {}", i, value, from);
                    } else {
                        println!("info[{}]: I32: Dyn from: {}", i, from);
                    }
                }
                EInfo::I64 { value, from } => {
                    if let Some(value) = value {
                        println!("info[{}]: I64: {} from: {}", i, value, from);
                    } else {
                        println!("info[{}]: I64: Dyn from: {}", i, from);
                    }
                }
                EInfo::Int { value, from } => {
                    if let Some(value) = value {
                        println!("info[{}]: Int: {} from: {}", i, value, from);
                    } else {
                        println!("info[{}]: Int: Dyn from: {}", i, from);
                    }
                }
                EInfo::U8 { value, from } => {
                    if let Some(value) = value {
                        println!("info[{}]: U8: {} from: {}", i, value, from);
                    } else {
                        println!("info[{}]: U8: Dyn from: {}", i, from);
                    }
                }
                EInfo::U16 { value, from } => {
                    if let Some(value) = value {
                        println!("info[{}]: U16: {} from: {}", i, value, from);
                    } else {
                        println!("info[{}]: U16: Dyn from: {}", i, from);
                    }
                }
                EInfo::U32 { value, from } => {
                    if let Some(value) = value {
                        println!("info[{}]: U32: {} from: {}", i, value, from);
                    } else {
                        println!("info[{}]: U32: Dyn from: {}", i, from);
                    }
                }
                EInfo::U64 { value, from } => {
                    if let Some(value) = value {
                        println!("info[{}]: U64: {} from: {}", i, value, from);
                    } else {
                        println!("info[{}]: U64: Dyn from: {}", i, from);
                    }
                }
                EInfo::UInt { value, from } => {
                    if let Some(value) = value {
                        println!("info[{}]: UInt: {} from: {}", i, value, from);
                    } else {
                        println!("info[{}]: UInt: Dyn from: {}", i, from);
                    }
                }
                EInfo::NoInfo => {
                    println!("info[{}]: NoInfo", i);
                }
                EInfo::NextPassCheck { from } => {
                    println!("info[{}]: NextPassCheck from: {}", i, from);
                }
                EInfo::Function {
                    name,
                    fn_ret_type_i,
                    fn_start_index,
                    fn_end_index,
                    from,
                } => {
                    println!(
                        "info[{}]: Function: {} {} from: {}",
                        i,
                        code.get_string(name),
                        code.type_as_str(fn_ret_type_i),
                        from
                    );
                }
                EInfo::Error { msg, from } => {
                    println!("info[{}]: Error: {} from: {}", i, msg, from);
                }
            }
        }
    }
}

impl PassEngine {
    pub fn run(&mut self, code: CodeBundle) -> SymbolTable {
        if !self.info.is_empty() {
            self.info.clear();
            self.needs_another_pass = false;
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

                    let init_info = &self.info[init_i.index];

                    if let Some(const_ty_i) = type_i {
                        // verify that the type if possible, else wait for next pass
                        if !self.verify_type_sig(&code, &sym_table, &const_ty_i) {
                            self.add_next_pass_info(loc);
                            continue;
                        }

                        let val_ty = self.infer_type(&code, &init_i, Some(const_ty_i.index));
                        match val_ty {
                            Ok(val_ty) => {
                                if !self.verify_value_type(&code, &sym_table, &val_ty) {
                                    // if we can't verify the type, we will wait for the next pass
                                    self.add_next_pass_info(loc);
                                    continue;
                                }

                                // make sure the type signature accepts the value type

                                // bind name to type in the symbol table
                                let const_name = code.get_string(&name_i);
                                sym_table.register(const_name, val_ty);
                                self.add_no_info();
                            }
                            Err(_) => {
                                // we need more information to infer the type so we will check in the next pass
                                self.add_next_pass_info(loc);
                            }
                        }
                    } else {
                        // get the type of the init_i value
                        let val_ty = self.infer_type(&code, &init_i, None);
                        match val_ty {
                            Ok(val_ty) => {
                                if !self.verify_value_type(&code, &sym_table, &val_ty) {
                                    // if we can't verify the type, we will wait for the next pass
                                    self.add_next_pass_info(loc);
                                    continue;
                                }
                                // bind name to type in the symbol table
                                let const_name = code.get_string(&name_i);
                                sym_table.register(const_name, val_ty);
                                self.add_no_info();
                            }
                            Err(_) => {
                                // we need more information to infer the type so we will check in the next pass
                                self.add_next_pass_info(loc);
                            }
                        }
                    }
                }
                CodeTag::EnterScope => {
                    sym_table = SymbolTable::make_child_env(sym_table);
                }
                CodeTag::ExitScope => {
                    sym_table = sym_table.return_parent_env().unwrap();
                }
                CodeTag::SrcComment => {
                    // SrcComment "comment"
                    self.add_no_info();
                }
                CodeTag::NameRef => {
                    // NameRef "name"
                    let name_i = ins.data[0];
                    let name = code.get_string(&name_i);
                    let val_ty_op = sym_table.get(&name);
                    match val_ty_op {
                        Some(val_ty) => {
                            // now we need to convert the ValueType to an EInfo
                        }
                        None => self.add_next_pass_info(loc),
                    }
                }
                _ => {
                    unimplemented!("Pass1Engine::run: {:?}", ins.tag)
                }
            }
        }
        sym_table
    }

    fn infer_type(
        &self,
        code: &CodeBundle,
        info_i: &Index,
        inference_ctx: Option<usize>,
    ) -> Result<ValueType, ()> {
        let info = &self.info[info_i.index];
        match info {
            EInfo::Type { from } => {
                let code_ = code.get_ins_unsafe(*from);
                Ok(ValueType {
                    tag: ValueTypeTag::Type,
                    src: code_.src.clone(),
                    data: vec![],
                })
            }
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
                                    Ok(ValueType {
                                        tag: ValueTypeTag::I8,
                                        src: code.src.clone(),
                                        data: vec![],
                                    })
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
                                    Ok(ValueType {
                                        tag: ValueTypeTag::I16,
                                        src: code.src.clone(),
                                        data: vec![],
                                    })
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
                                    Ok(ValueType {
                                        tag: ValueTypeTag::I32,
                                        src: code.src.clone(),
                                        data: vec![],
                                    })
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
                                    Ok(ValueType {
                                        tag: ValueTypeTag::I64,
                                        src: code.src.clone(),
                                        data: vec![],
                                    })
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
                                    Ok(ValueType {
                                        tag: ValueTypeTag::Int,
                                        src: code.src.clone(),
                                        data: vec![],
                                    })
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
                                    Ok(ValueType {
                                        tag: ValueTypeTag::U8,
                                        src: code.src.clone(),
                                        data: vec![],
                                    })
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
                                    Ok(ValueType {
                                        tag: ValueTypeTag::U16,
                                        src: code.src.clone(),
                                        data: vec![],
                                    })
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
                                    Ok(ValueType {
                                        tag: ValueTypeTag::U32,
                                        src: code.src.clone(),
                                        data: vec![],
                                    })
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
                                    Ok(ValueType {
                                        tag: ValueTypeTag::U64,
                                        src: code.src.clone(),
                                        data: vec![],
                                    })
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
                                    Ok(ValueType {
                                        tag: ValueTypeTag::UInt,
                                        src: code.src.clone(),
                                        data: vec![],
                                    })
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
                        Ok(ValueType {
                            tag: ValueTypeTag::Int,
                            src: code.src.clone(),
                            data: vec![],
                        })
                    } else {
                        panic!("Pass1Engine::to_value_type: could not convert {num_str} to i32")
                    }
                }
            }
            EInfo::ReferenceToType { type_i, from } => {
                let ty = code.get_type(type_i);
                Ok(ValueType {
                    tag: ValueTypeTag::Type,
                    src: ty.src,
                    data: vec![*type_i],
                })
            }
            EInfo::StaticArray {
                item_type_i,
                from,
                items,
            } => {
                let code_ = code.get_ins_unsafe(*from);
                Ok(ValueType {
                    tag: ValueTypeTag::StaticArray,
                    src: code_.src.clone(),
                    data: vec![*item_type_i],
                })
            }
            EInfo::Bool { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                Ok(ValueType {
                    tag: ValueTypeTag::Bool,
                    src: code.src.clone(),
                    data: vec![],
                })
            }
            EInfo::Char { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                Ok(ValueType {
                    tag: ValueTypeTag::Char,
                    src: code.src.clone(),
                    data: vec![],
                })
            }
            EInfo::Void { from } => {
                let code = code.get_ins_unsafe(*from);
                Ok(ValueType {
                    tag: ValueTypeTag::Void,
                    src: code.src.clone(),
                    data: vec![],
                })
            }
            EInfo::Str { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                Ok(ValueType {
                    tag: ValueTypeTag::Str,
                    src: code.src.clone(),
                    data: vec![],
                })
            }
            EInfo::I8 { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                Ok(ValueType {
                    tag: ValueTypeTag::I8,
                    src: code.src.clone(),
                    data: vec![],
                })
            }
            EInfo::I16 { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                Ok(ValueType {
                    tag: ValueTypeTag::I16,
                    src: code.src.clone(),
                    data: vec![],
                })
            }
            EInfo::I32 { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                Ok(ValueType {
                    tag: ValueTypeTag::I32,
                    src: code.src.clone(),
                    data: vec![],
                })
            }
            EInfo::I64 { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                Ok(ValueType {
                    tag: ValueTypeTag::I64,
                    src: code.src.clone(),
                    data: vec![],
                })
            }
            EInfo::Int { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                Ok(ValueType {
                    tag: ValueTypeTag::Int,
                    src: code.src.clone(),
                    data: vec![],
                })
            }
            EInfo::U8 { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                Ok(ValueType {
                    tag: ValueTypeTag::U8,
                    src: code.src.clone(),
                    data: vec![],
                })
            }
            EInfo::U16 { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                Ok(ValueType {
                    tag: ValueTypeTag::U16,
                    src: code.src.clone(),
                    data: vec![],
                })
            }
            EInfo::U32 { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                Ok(ValueType {
                    tag: ValueTypeTag::U32,
                    src: code.src.clone(),
                    data: vec![],
                })
            }
            EInfo::U64 { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                Ok(ValueType {
                    tag: ValueTypeTag::U64,
                    src: code.src.clone(),
                    data: vec![],
                })
            }
            EInfo::UInt { from, .. } => {
                let code = code.get_ins_unsafe(*from);
                Ok(ValueType {
                    tag: ValueTypeTag::UInt,
                    src: code.src.clone(),
                    data: vec![],
                })
            }
            EInfo::NoInfo => {
                panic!("Pass1Engine::to_value_type: NoInfo")
            }
            EInfo::Function {
                fn_ret_type_i,
                from,
                ..
            } => {
                let code_ = code.get_ins_unsafe(*from);
                let fn_type = code.get_type(fn_ret_type_i);
                Ok(ValueType {
                    tag: ValueTypeTag::Function,
                    src: code_.src.clone(),
                    data: vec![*fn_ret_type_i],
                })
            }
            EInfo::Error { msg, from } => {
                panic!("Pass1Engine::to_value_type: {msg}")
            }
            EInfo::NextPassCheck { from } => Err(()),
        }
    }

    fn generate_einfo(&self, val: &ValueType, loc: usize) -> EInfo {
        match val.tag {
            ValueTypeTag::TypeNameRef => todo!(),
            ValueTypeTag::Bool => EInfo::Bool {
                value: None,
                from: loc,
            },
            ValueTypeTag::Char => EInfo::Char {
                value: None,
                from: loc,
            },
            ValueTypeTag::Void => EInfo::Void { from: loc },
            ValueTypeTag::Str => EInfo::Str {
                value: None,
                from: loc,
            },
            ValueTypeTag::Type => EInfo::Type { from: loc },
            ValueTypeTag::I8 => EInfo::I8 {
                value: None,
                from: loc,
            },
            ValueTypeTag::I16 => EInfo::I16 {
                value: None,
                from: loc,
            },
            ValueTypeTag::I32 => EInfo::I32 {
                value: None,
                from: loc,
            },
            ValueTypeTag::I64 => EInfo::I64 {
                value: None,
                from: loc,
            },
            ValueTypeTag::Int => EInfo::Int {
                value: None,
                from: loc,
            },
            ValueTypeTag::U8 => EInfo::U8 {
                value: None,
                from: loc,
            },
            ValueTypeTag::U16 => EInfo::U16 {
                value: None,
                from: loc,
            },
            ValueTypeTag::U32 => EInfo::U32 {
                value: None,
                from: loc,
            },
            ValueTypeTag::U64 => EInfo::U64 {
                value: None,
                from: loc,
            },
            ValueTypeTag::UInt => EInfo::UInt {
                value: None,
                from: loc,
            },
            ValueTypeTag::StaticArray => EInfo::StaticArray {
                item_type_i: val.data[0],
                from: loc,
                items: vec![],
            },
            ValueTypeTag::Function => EInfo::Function {
                fn_ret_type_i: val.data[0],
                from: loc,
            },
        }
    }
}
