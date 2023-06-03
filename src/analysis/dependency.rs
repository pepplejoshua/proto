use crate::ir8::lowir::{LowIRIns, LowIRModule};

pub fn extract(module: &LowIRModule) -> Vec<LowIRIns> {
    let mut dependencies = vec![];

    for ins in &module.ins_pool.pool {
        if let LowIRIns::UseDependency { paths: _, src: _ } = ins {
            dependencies.push(ins.clone());
        }
    }
    dependencies
}

pub fn resolve_use_to_path(use_ins: &LowIRIns) -> String {
    todo!("resolve_use_to_path")
}
