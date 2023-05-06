use phf::phf_map;

#[allow(dead_code)]
pub const DIRECTIVES_EXPRS: phf::Map<&str, bool> = phf_map!(
    "run" => true,
    "filename" => false,
    "line" => false,
    "col" => false,
    "platform" => false,
    "datetime" => false,
    "date" => false,
    "error" => true,
);

#[allow(dead_code)]
pub const DIRECTIVES_INS: phf::Map<&str, bool> = phf_map!("run" => true, "inline" => true);
