#![allow(dead_code)]
#![allow(unused_variables)]

use crate::frontend::types::TypeSignature;

pub enum PIR {
  Global {
    name: String,
    ty: TypeSignature,
    init: usize,
  },
  Function {
    name: String,
    ty: TypeSignature,
    body_start: usize,
    body_end: usize,
  },
  Block {
    body_start: usize,
    body_end: usize,
  }
}