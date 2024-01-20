use super::{
    source::SourceRef,
    types::{TypeSignatureTag, TypeSignature},
};

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, Hash)]
pub enum IndexTag {
    String,
    Code,
    TypeSignature,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, Hash)]
pub struct Index {
    pub tag: IndexTag,
    pub index: usize,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum CodeTag {
    // LoadImmediateNum "100000"
    LINum,
    // LoadImmediateStr "hello world"
    LIStr,
    // LoadImmediateChar "a"
    LIChar,
    // NameRef "list"
    NameRef,
    // Update Code:19 Code:20
    // where 19 is the index of the target code node
    // and 20 is the index of the new value of the target
    Update,
    /*
        MakePublic Code:20
        where 20 is the index of a preceding code node
        to modify
    */
    MakePublic,
    /*
        NewConstant "name" Type:19?, Code:20,
        where 19 is the type index for the type signature, if any,
        and 20 is the code index for the init value
        if the number of code indices is 2, there is no type signature.
        just a name and an init value
    */
    NewConstant,
    /*
        NewTypedVariable "name" Type:19, Code:20?,
        where 19 is the code index for the type signature
        and 20 is the code index for the init value, if any
        if the number of code indices is 2, there is no init value.
        just a name and a type signature
    */
    NTVariable,
    /*
        NewUntypedVariable "name" Code:20,
        20 is the code index for the init value
    */
    NUVariable,
    /*
        NewFunction "name" Type:2 Code:30
        where Type:2 is the type index for the type signature
        30 is the end of the function body.
        Granted there will always be a name index and a function type index,
        the last index is for the end of the function body.
    */
    NewFunction,
    /*
        Param "name" Type:19 <Code:20 (Future upgrade when I allow function call parameter labels)>
        where name is the name of the parameter
        19 is the type index for the type signature
        <20 is the code index for the default value, if any>
        Eventually, I will allow function call parameter labels
        This is a non mutable parameter
    */
    Param,
    /*
        VarParam "name" Type:19 <Code:20 (Future upgrade when I allow function call parameter labels)>
        where name is the name of the parameter
        19 is the type index for the type signature
        <20 is the code index for the default value, if any>
        Eventually, I will allow function call parameter labels
        This is a mutable parameter
    */
    VarParam,
    /*
        ExpectTypeIs Type:19, Code:20
        where 19 is the type index for the expected type
        and 20 is the code index for the value (which could be a type itself)
        to check against the expected type
    */
    ExpectTypeIs,
    /*
        ExpectTypesMatch Type:19, Type:20
        where 19 is the code index for the first value (which could be a type itself)
        and 20 is the code index for the second value (which could be a type itself)
        to check if the types match
    */
    ExpectTypesMatch,
    /*
        TypeRef Type:19
        where 19 is the type index for the type signature
    */
    TypeRef,
    /*
        LoadTrue
        loads the boolean value true
    */
    LoadTrue,
    /*
        LoadFalse
        loads the boolean value false
    */
    LoadFalse,
    /*
        # "comment"
        where "comment" is the comment text found in the string table
    */
    SrcComment,
    /*
        *Operator* Code:9 Code:10
        where 9 is the code index for the left operand
        and 10 is the code index for the right operand
        and *Operator* is one of the below operators
    */
    Add,
    Sub,
    Negate,
    Mult,
    Div,
    Modulo,
    Not,
    Eq,
    Neq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    And,
    Or,
    /*
        AccessMember Code:9 Code:10
        where 9 is the code index for the struct
        and 10 is the code index for the member name
    */
    AccessMember,
    /*
        AccessIndex Code:9 Code:10
        where 9 is the code index for the array
        and 10 is the code index for the index
    */
    AccessIndex,
    /*
        Call Code:8 Code:[9:12]
        where 8 is the code index for the function name or function object
        and 9 is the code index for the first parameter
        and 12 is the code index for the last parameter
    */
    Call,
    /*
        Return Code:9
        where 9 is the code index for the return value
    */
    Return,
    /*
        EnterScope
        enters a new scope
    */
    EnterScope,
    /*
        ExitScope
        exits the current scope
    */
    ExitScope,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Code {
    pub tag: CodeTag,
    pub indices: Vec<Index>,
    pub src: SourceRef,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct CodeBundle {
    pub ins: Vec<Code>,
    pub strings: Vec<String>,
    pub types: Vec<TypeSignature>,
}

#[allow(dead_code)]
impl CodeBundle {
    pub fn new() -> Self {
        Self {
            ins: vec![],
            strings: vec![],
            types: vec![],
        }
    }

    pub fn as_str(&self) -> String {
        let mut s = Vec::new();
        let left_pad = self.ins.len().to_string().len() + 1;
        for (index, code) in self.ins.iter().enumerate() {
            let num = format!("{:0>width$}", index, width = left_pad);
            match code.tag {
                CodeTag::LINum => {
                    let imm = self.get_string(&code.indices[0]);
                    s.push(format!("{num} LoadImmediateNum `{imm}`\n"));
                }
                CodeTag::LIStr => {
                    let imm = self.get_string(&code.indices[0]);
                    s.push(format!("{num} LoadImmediateStr \"{imm}\"\n"));
                }
                CodeTag::LIChar => {
                    let imm = self.get_string(&code.indices[0]);
                    s.push(format!("{num} LoadImmediateChar '{imm}'\n"));
                }
                CodeTag::NameRef => {
                    let name = self.get_string(&code.indices[0]);
                    s.push(format!("{num} NameRef `{name}`\n"));
                }
                CodeTag::MakePublic => {
                    let target = code.indices[0].index;
                    s.push(format!("{num} MakePublic Code:{target}\n"));
                }
                CodeTag::NewConstant => {
                    let name = self.get_string(&code.indices[0]);
                    if code.indices.len() == 2 {
                        let value = code.indices[1].index;
                        s.push(format!(
                            "{num} NewConstant `{name}` Code:{value}\n",
                            name = name,
                            value = value
                        ));
                    } else {
                        let type_s = self.type_as_str(code.indices[1]);
                        let value = code.indices[2].index;
                        s.push(format!(
                            "{num} NewConstant `{name}` {type_s} Code:{value}\n",
                        ));
                    }
                }
                CodeTag::NTVariable => {
                    let name = self.get_string(&code.indices[0]);
                    let type_s = self.type_as_str(code.indices[1]);
                    if code.indices.len() == 3 {
                        let value = code.indices[2].index;
                        s.push(format!(
                            "{num} NewTypedVariable `{name}` {type_s} Code:{value}\n",
                        ));
                    } else {
                        s.push(format!("{num} NewTypedVariable `{name}` {type_s}\n",));
                    }
                }
                CodeTag::NUVariable => {
                    let name = self.get_string(&code.indices[0]);
                    let value = code.indices[1].index;
                    s.push(format!("{num} NewUntypedVariable `{name}` Code:{value}\n"));
                }
                CodeTag::NewFunction => {
                    let name = self.get_string(&code.indices[0]);
                    let func_ty_i = code.indices[1];
                    let func_ty_s = self.type_as_str(func_ty_i);
                    let body_end = code.indices[2].index;
                    s.push(format!(
                        "\n{num} NewFunction `{name}` {func_ty_s} Code:{body_end}\n"
                    ));
                }
                CodeTag::Param => {
                    let name = self.get_string(&code.indices[0]);
                    let type_s = self.type_as_str(code.indices[1]);
                    s.push(format!("{num} Param `{name}` {type_s}\n"));
                }
                CodeTag::VarParam => {
                    let name = self.get_string(&code.indices[0]);
                    let type_s = self.type_as_str(code.indices[1]);
                    s.push(format!("{num} VarParam `{name}` {type_s}\n"));
                }
                CodeTag::ExpectTypeIs => {
                    let a_type_s = self.type_as_str(code.indices[0]);
                    let val_s = code.indices[1].index;
                    s.push(format!("{num} ExpectTypeIs {a_type_s} Code:{val_s}\n"));
                }
                CodeTag::ExpectTypesMatch => {
                    let a_type_s = self.type_as_str(code.indices[0]);
                    let b_type_s = self.type_as_str(code.indices[1]);
                    s.push(format!("{num} ExpectTypesMatch {a_type_s} {b_type_s}\n",));
                }
                CodeTag::TypeRef => {
                    let type_s = self.type_as_str(code.indices[0]);
                    s.push(format!("{num} TypeRef {type_s}\n"));
                }
                CodeTag::LoadTrue => {
                    s.push(format!("{num} LoadTrue\n"));
                }
                CodeTag::LoadFalse => {
                    s.push(format!("{num} LoadFalse\n"));
                }
                CodeTag::SrcComment => {
                    // let comment = self.get_string(&code.indices[0]);
                    // s.push(format!("{num} {comment}\n"));
                    // TODO(@pepplejoshua): uncomment this later
                }
                CodeTag::Add => {
                    let a = code.indices[0].index;
                    let b = code.indices[1].index;
                    s.push(format!("{num} Add Code:{a} Code:{b}\n"));
                }
                CodeTag::Sub => {
                    let a = code.indices[0].index;
                    let b = code.indices[1].index;
                    s.push(format!("{num} Sub Code:{a} Code:{b}\n"));
                }
                CodeTag::Negate => {
                    let a = code.indices[0].index;
                    s.push(format!("{num} Negate Code:{a}\n"));
                }
                CodeTag::Mult => {
                    let a = code.indices[0].index;
                    let b = code.indices[1].index;
                    s.push(format!("{num} Mult Code:{a} Code:{b}\n"));
                }
                CodeTag::Div => {
                    let a = code.indices[0].index;
                    let b = code.indices[1].index;
                    s.push(format!("{num} Div Code:{a} Code:{b}\n"));
                }
                CodeTag::Modulo => {
                    let a = code.indices[0].index;
                    let b = code.indices[1].index;
                    s.push(format!("{num} Modulo Code:{a} Code:{b}\n"));
                }
                CodeTag::Not => {
                    let a = code.indices[0].index;
                    s.push(format!("{num} Not Code:{a}\n"));
                }
                CodeTag::Eq => {
                    let a = code.indices[0].index;
                    let b = code.indices[1].index;
                    s.push(format!("{num} Eq Code:{a} Code:{b}\n"));
                }
                CodeTag::Neq => {
                    let a = code.indices[0].index;
                    let b = code.indices[1].index;
                    s.push(format!("{num} Neq Code:{a} Code:{b}\n"));
                }
                CodeTag::Lt => {
                    let a = code.indices[0].index;
                    let b = code.indices[1].index;
                    s.push(format!("{num} Lt Code:{a} Code:{b}\n"));
                }
                CodeTag::Gt => {
                    let a = code.indices[0].index;
                    let b = code.indices[1].index;
                    s.push(format!("{num} Gt Code:{a} Code:{b}\n"));
                }
                CodeTag::LtEq => {
                    let a = code.indices[0].index;
                    let b = code.indices[1].index;
                    s.push(format!("{num} LtEq Code:{a} Code:{b}\n"));
                }
                CodeTag::GtEq => {
                    let a = code.indices[0].index;
                    let b = code.indices[1].index;
                    s.push(format!("{num} GtEq Code:{a} Code:{b}\n"));
                }
                CodeTag::And => {
                    let a = code.indices[0].index;
                    let b = code.indices[1].index;
                    s.push(format!("{num} And Code:{a} Code:{b}\n"));
                }
                CodeTag::Or => {
                    let a = code.indices[0].index;
                    let b = code.indices[1].index;
                    s.push(format!("{num} Or Code:{a} Code:{b}\n"));
                }
                CodeTag::AccessMember => {
                    let a = code.indices[0].index;
                    let b = code.indices[1].index;
                    s.push(format!("{num} AcessMember Code:{a} Code:{b}\n"));
                }
                CodeTag::AccessIndex => {
                    let a = code.indices[0].index;
                    let b = code.indices[1].index;
                    s.push(format!("{num} AccessIndex Code:{a} Code:{b}\n"));
                }
                CodeTag::Call => {
                    let a = code.indices[0].index;
                    let b = code.indices[1].index;
                    s.push(format!("{num} Call Code:{a} Code:{b}\n"));
                }
                CodeTag::Return => {
                    if code.indices.len() == 0 {
                        s.push(format!("{num} Return\n"));
                    } else {
                        let a = code.indices[0].index;
                        s.push(format!("{num} Return Code:{a}\n"));
                    }
                }
                CodeTag::EnterScope => {
                    s.push(format!("{num} EnterScope\n"));
                }
                CodeTag::ExitScope => {
                    s.push(format!("{num} ExitScope\n"));
                }
                CodeTag::Update => {
                    let a = code.indices[0].index;
                    let b = code.indices[1].index;
                    s.push(format!("{num} Update Code:{a} Code:{b}\n"));
                }
            }
        }
        s.join("")
    }

    pub fn reserve_ins(&mut self) -> Index {
        let index = self.ins.len();
        // temporary placeholder
        self.ins.push(Code {
            tag: CodeTag::LINum,
            indices: vec![],
            src: SourceRef::dud(),
        });
        Index {
            index,
            tag: IndexTag::Code,
        }
    }

    pub fn add_ins(&mut self, code: Code) -> Index {
        let index = self.ins.len();
        self.ins.push(code);
        Index {
            index,
            tag: IndexTag::Code,
        }
    }

    pub fn update_ins(&mut self, index: Index, code: Code) {
        if let IndexTag::Code = index.tag {
            self.ins[index.index] = code;
        } else {
            panic!("Index tag is not a code but updating code...")
        }
    }

    pub fn get_ins(&self, index: Index) -> Code {
        if let IndexTag::Code = index.tag {
            self.ins[index.index].clone()
        } else {
            panic!("Index tag is not a code but getting code...")
        }
    }

    pub fn add_string(&mut self, string: String) -> Index {
        let index = self.strings.len();
        self.strings.push(string);
        Index {
            index,
            tag: IndexTag::String,
        }
    }

    pub fn get_string(&self, index: &Index) -> String {
        if let IndexTag::String = index.tag {
            self.strings[index.index].clone()
        } else {
            panic!("Index tag is not a string but getting string...")
        }
    }

    pub fn add_type(&mut self, type_sig: TypeSignature) -> Index {
        let index = self.types.len();
        self.types.push(type_sig);
        Index {
            index,
            tag: IndexTag::TypeSignature,
        }
    }

    pub fn type_exists(&self, sig: &TypeSignature) -> Option<Index> {
        todo!()
    }

    pub fn get_type(&self, index: &Index) -> TypeSignature {
        if let IndexTag::TypeSignature = index.tag {
            self.types[index.index].clone()
        } else {
            unreachable!("Index tag is not a type signature but getting type signature...")
        }
    }

    pub fn type_as_strl(&self, ty: &TypeSignature) -> String {
        todo!()
    }

    pub fn type_as_str(&self, index: Index) -> String {
        todo!()
    }
}
