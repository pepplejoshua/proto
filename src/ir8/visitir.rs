use super::lowir::{ExprRef, InsRef, KeyValueBindings, LowIRModule};

#[allow(dead_code)]
pub enum VisitIRError {}

#[allow(dead_code)]
pub trait VisitsLowIR<A, B, C> {
    fn visit_ins(&mut self, ins: &InsRef) -> Result<A, VisitIRError>;
    fn visit_expr(&mut self, expr: &ExprRef) -> Result<B, VisitIRError>;
    fn visit_pairs(&mut self, kv: &KeyValueBindings) -> Result<C, VisitIRError>;
}

pub fn apply_visitor<A, B, C, V: VisitsLowIR<A, B, C>>(
    visitor: &mut V,
    ins: &InsRef,
) -> Result<A, VisitIRError> {
    visitor.visit_ins(ins)
}

pub fn apply_to_module<A, B, C, V: VisitsLowIR<A, B, C>>(
    visitor: &mut V,
    module: &LowIRModule,
) -> Vec<Result<A, VisitIRError>> {
    let mut result = vec![];
    for ins_ref in module.top_level.iter() {
        let res = apply_visitor(visitor, &ins_ref);
        result.push(res);
    }
    result
}
