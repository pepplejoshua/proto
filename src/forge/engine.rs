#![allow(dead_code)]
#![allow(unused_variables)]

use crate::{
    frontend::{
        bcode::{CodeBundle, CodeTag},
        types::EInfo,
    },
    symbol_info::symbol_info::SymbolTable,
};

pub struct Pass1Engine {
    pub info: Vec<EInfo>,
}

impl Pass1Engine {
    pub fn new() -> Self {
        Pass1Engine { info: Vec::new() }
    }

    pub fn add_info(&mut self, info: EInfo) {
        self.info.push(info);
    }
}

impl Pass1Engine {
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
                CodeTag::NameRef => {}
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
}
