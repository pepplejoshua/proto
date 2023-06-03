use crate::ir8::lowir::LowIRModule;

pub struct ExtractDependencies<'a> {
    module: &'a LowIRModule,
}

impl<'a> ExtractDependencies<'a> {
    pub fn new(module: &'a LowIRModule) -> Self {
        ExtractDependencies { module }
    }
}
