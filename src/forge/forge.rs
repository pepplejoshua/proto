use core::panic;

use crate::frontend::bcode::{CodeBundle, CodeTag, Index, IndexTag};

use super::env::Env;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum EInfo {
    // store the string and the index it originated from
    ImmediateNum(Index, usize),
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Engine {
    env: Env,
    information: Vec<EInfo>,
    code: CodeBundle,
}

#[allow(dead_code)]
impl Engine {
    pub fn new(code: CodeBundle) -> Self {
        Engine {
            env: Env::new(),
            information: Vec::new(),
            code,
        }
    }

    pub fn read_str(&self, loc: Index) -> &str {
        assert!(matches!(loc.tag, IndexTag::String), "expected string index");
        self.code.strings[loc.index].as_str()
    }

    pub fn run(&mut self) {
        for (loc, ins) in self.code.ins.iter().enumerate() {
            match ins.tag {
                CodeTag::LINum => {
                    // LINum "12233"
                    let num_i = ins.data[0];
                    let info = EInfo::ImmediateNum(num_i, loc);
                    self.information.push(info);
                }
                CodeTag::NewConstant => {
                    // NewConstant
                }
                _ => panic!("unimplemented: {:?}", ins.tag),
            }
        }
    }
}