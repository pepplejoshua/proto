#[allow(dead_code)]
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum PIRTypes {
    BuiltIn {
        name: String,
    },
    Struct {
        name: String,
        definition_loc: Option<usize>,
        depth: usize,
    },
    Function {
        name: String,                  // name of the function
        param_signature: Vec<usize>,   // indices of the types of the parameters
        return_type: usize,            // index of the type of the return value
        definition_loc: Option<usize>, // index of the instruction where the function is defined
        depth: usize,                  // depth of the scope where the function is defined
    },
}

#[allow(dead_code)]
impl PIRTypes {
    pub fn as_str(&self) -> String {
        match self {
            PIRTypes::BuiltIn { name }
            | PIRTypes::Struct { name, .. }
            | PIRTypes::Function { name, .. } => name.clone(),
        }
    }

    pub fn get_name(&self) -> String {
        match self {
            PIRTypes::BuiltIn { name, .. }
            | PIRTypes::Struct { name, .. }
            | PIRTypes::Function { name, .. } => name.clone(),
        }
    }

    pub fn get_definition_loc(&self) -> Option<usize> {
        match self {
            PIRTypes::BuiltIn { .. } => None,
            PIRTypes::Struct { definition_loc, .. } | PIRTypes::Function { definition_loc, .. } => {
                *definition_loc
            }
        }
    }

    pub fn get_depth(&self) -> usize {
        match self {
            PIRTypes::BuiltIn { .. } => 0,
            PIRTypes::Struct { depth, .. } | PIRTypes::Function { depth, .. } => *depth,
        }
    }
}
