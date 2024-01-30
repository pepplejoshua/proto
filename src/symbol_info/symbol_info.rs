// there is one ProgramSymbolInfo per program
#![allow(dead_code)]
#![allow(unused_variables)]

#[derive(Debug)]
pub struct ProgramSymbolInfo {
  // module map: name -> matching ModuleInfo
  module_map: HashMap<String, ModuleInfo>,
  // global symbols. this is passed around during the 2
  // pass analysis of files in the program
  // it will get updated with globals across files
  global_symbols: Vec<CodeSymbolInfo>,
}

impl ProgramSymbolInfo {
}

pub struct ModuleInfo {
  mod_name: String,
  mod_code: Vec<CodeSymbolInfo>,
  mod_symbols: Vec<CodeSymbolInfo>,
}

impl ModuleInfo {
}

// global symbols can be:
// - pub constants
// - pub functions
