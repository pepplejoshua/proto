use phf::phf_map;

#[allow(dead_code)]
pub const DIRECTIVES_EXPRS: phf::Map<&str, bool> = phf_map!(
    "run" => true,          // type dependent on parameter
    "filename" => false,    // str
    "line" => false,        // usize
    "col" => false,         // usize
    "platform" => false,    // str
    "datetime" => false,    // str
    "date" => false,        // str
    "error" => true,        // void
);

#[allow(dead_code)]
pub const DIRECTIVES_INS: phf::Map<&str, bool> =
    phf_map!("run" => true, "inline" => true, "register" => true);
