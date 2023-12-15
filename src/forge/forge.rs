use crate::frontend::bcode::CodeBundle;

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, Hash)]
pub struct Forge {}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, Hash)]
pub enum FTag {
    ImmStr,
    ConstVar,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, Hash)]
pub struct ForgeInfo {}

impl Forge {
    // it will evaluate bcode instructions and collect information
    // about the code, using it to analyze and partially evaluate the
    // code to generate a type checked version or just C++
    pub fn eval(&mut self, _code: &mut CodeBundle) {}
}
