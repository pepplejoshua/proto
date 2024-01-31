use crate::{frontend::{bcode::{CodeBundle, CodeTag, Index, IndexTag}, types::{EInfo, TypeSignature, TypeSignatureTag, ValueType, ValueTypeTag}}, symbol_info::symbol_info::{SymbolTable, TableType}};

use super::env::Env;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Engine {
    envs: Vec<Env>,
    information: Vec<EInfo>,
    code: CodeBundle,
    cur_fn_end_index: Option<Index>,
    cur_fn_return_ty_index: Option<Index>,
}

// simple helper functions
#[allow(dead_code)]
impl Engine {
    pub fn new(code: CodeBundle) -> Self {
        Engine {
            envs: vec![],
            information: Vec::new(),
            code,
            cur_fn_end_index: None,
            cur_fn_return_ty_index: None,
        }
    }

    fn enter_scope(&mut self) {
        let env = Env::new();
        self.envs.push(env);
    }

    fn exit_scope(&mut self) {
        // display information about the current scope and then pop it
        let env = self.envs.last().unwrap();
        env.show_env_info();

        self.envs.pop();
    }

    fn cur_scope(&mut self) -> &mut Env {
        self.envs.last_mut().unwrap()
    }

    fn read_str(&self, loc: &Index) -> String {
        assert!(matches!(loc.tag, IndexTag::String), "expected string index");
        self.code.strings[loc.index].clone()
    }

    fn push_no_info(&mut self) {
        self.information.push(EInfo::NoInfo);
    }
}

// semi complex stuff
#[allow(dead_code)]
impl Engine {
    fn check_name(&self, name: &str) -> bool {
        for env in self.envs.iter().rev() {
            if env.check_name(name) {
                return true;
            }
        }
        false
    }

    fn get_info_for_name(&self, name: &str) -> Option<EInfo> {
        for env in self.envs.iter().rev() {
            if env.check_name(name) {
                return env.get_info_for_name(name);
            }
        }
        None
    }

    fn verify_type_sig(&self, type_i: &Index) -> bool {
        let type_sig = &self.code.types[type_i.index];
        if type_sig.tag.is_simple_type_sig() {
            true
        } else {
            match type_sig.tag {
                TypeSignatureTag::TypeNameRefTS => {
                    let name = self.read_str(&type_sig.indices[0]);
                    if self.check_name(&name) {
                        true
                    } else {
                        false
                    }
                }
                TypeSignatureTag::StaticArrayTS => {
                    let type_i = &type_sig.indices[1];
                    self.verify_type_sig(type_i)
                }
                TypeSignatureTag::FunctionTS => {
                    for type_i in type_sig.indices.iter() {
                        if !self.verify_type_sig(type_i) {
                            return false;
                        }
                    }
                    true
                }
                _ => false,
            }
        }
    }

    fn verify_value_type(&mut self, val_ty: &ValueType) -> bool {
        if val_ty.tag.is_simple_value_type() {
            true
        } else {
            match val_ty.tag {
                ValueTypeTag::TypeNameRef => {
                    let name = self.read_str(&val_ty.data[0]);
                    if self.check_name(&name) {
                        true
                    } else {
                        false
                    }
                }
                ValueTypeTag::StaticArray => {
                    let type_i = &val_ty.data[0];
                    self.verify_type_sig(type_i)
                }
                _ => false,
            }
        }
    }

    fn type_sig_accepts(&self, type_sig: &TypeSignature, val_ty: &ValueType) -> bool {
        let _val_tag = val_ty.tag;
        type_sig.tag.to_value_type_tag() == _val_tag
    }

    fn infer_type(&self, loc: &Index, with: Option<&Index>) -> (ValueType, EInfo) {
        let info = &self.information[loc.index];

        match info {
            EInfo::ImmediateNum { str_i, from } => {
                if with.is_some() {
                    let with_ty = self.code.types.get(with.unwrap().index).unwrap();
                    if !with_ty.tag.is_numerical_type_sig() {
                        panic!("cannot promote number to {:?} type", with_ty.tag);
                    }
                    let num_s = self.read_str(str_i);
                    match with_ty.tag {
                        TypeSignatureTag::I8TS => {
                            let num = num_s.parse::<i8>();
                            if num.is_err() {
                                panic!("invalid number of i8 type: {}", num_s);
                            }

                            let num = num.unwrap();
                            let info = EInfo::I8 {
                                value: Some(num),
                                from: *from,
                            };
                            let src = self.code.ins.get(*from).unwrap().src.clone();
                            let val_ty = ValueType {
                                tag: ValueTypeTag::I8, 
                                src, 
                                data: vec![],
                            };
                            (val_ty, info)
                        }
                        TypeSignatureTag::I16TS => {
                            let num = num_s.parse::<i16>();
                            if num.is_err() {
                                panic!("invalid number of i16 type: {}", num_s);
                            }

                            let num = num.unwrap();
                            let info = EInfo::I16 {
                                value: Some(num),
                                from: *from,
                            };
                            let src = self.code.ins.get(*from).unwrap().src.clone();
                            let val_ty = ValueType {
                                tag: ValueTypeTag::I16, 
                                src, 
                                data: vec![],
                            };
                            (val_ty, info)
                        }
                        TypeSignatureTag::I32TS => {
                            let num = num_s.parse::<i32>();
                            if num.is_err() {
                                panic!("invalid number of i32 type: {}", num_s);
                            }

                            let num = num.unwrap();
                            let info = EInfo::I32 {
                                value: Some(num),
                                from: *from,
                            };
                            let src = self.code.ins.get(*from).unwrap().src.clone();
                            let val_ty = ValueType {
                                tag: ValueTypeTag::I32, 
                                src, 
                                data: vec![],
                            };
                            (val_ty, info)
                        }
                        TypeSignatureTag::I64TS => {
                            let num = num_s.parse::<i64>();
                            if num.is_err() {
                                panic!("invalid number of i64 type: {}", num_s);
                            }

                            let num = num.unwrap();
                            let info = EInfo::I64 {
                                value: Some(num),
                                from: *from,
                            };
                            let src = self.code.ins.get(*from).unwrap().src.clone();
                            let val_ty = ValueType {
                                tag: ValueTypeTag::I64, 
                                src, 
                                data: vec![],
                            };
                            (val_ty, info)
                        }
                        TypeSignatureTag::IsizeTS => {
                            let num = num_s.parse::<isize>();
                            if num.is_err() {
                                panic!("invalid number of isize type: {}", num_s);
                            }

                            let num = num.unwrap();
                            let info = EInfo::Isize {
                                value: Some(num),
                                from: *from,
                            };
                            let src = self.code.ins.get(*from).unwrap().src.clone();
                            let val_ty = ValueType {
                                tag: ValueTypeTag::Isize, 
                                src, 
                                data: vec![],
                            };
                            (val_ty, info)
                        }
                        TypeSignatureTag::U8TS => {
                            let num = num_s.parse::<u8>();
                            if num.is_err() {
                                panic!("invalid number of u8 type: {}", num_s);
                            }

                            let num = num.unwrap();
                            let info = EInfo::U8 {
                                value: Some(num),
                                from: *from,
                            };
                            let src = self.code.ins.get(*from).unwrap().src.clone();
                            let val_ty = ValueType {
                                tag: ValueTypeTag::U8, 
                                src, 
                                data: vec![],
                            };
                            (val_ty, info)
                        }
                        TypeSignatureTag::U16TS => {
                            let num = num_s.parse::<u16>();
                            if num.is_err() {
                                panic!("invalid number of u16 type: {}", num_s);
                            }

                            let num = num.unwrap();
                            let info = EInfo::U16 {
                                value: Some(num),
                                from: *from,
                            };
                            let src = self.code.ins.get(*from).unwrap().src.clone();
                            let val_ty = ValueType {
                                tag: ValueTypeTag::U16, 
                                src, 
                                data: vec![],
                            };
                            (val_ty, info)
                        }
                        TypeSignatureTag::U32TS => {
                            let num = num_s.parse::<u32>();
                            if num.is_err() {
                                panic!("invalid number of u32 type: {}", num_s);
                            }

                            let num = num.unwrap();
                            let info = EInfo::U32 {
                                value: Some(num),
                                from: *from,
                            };
                            let src = self.code.ins.get(*from).unwrap().src.clone();
                            let val_ty = ValueType {
                                tag: ValueTypeTag::U32, 
                                src, 
                                data: vec![],
                            };
                            (val_ty, info)
                        }
                        TypeSignatureTag::U64TS => {
                            let num = num_s.parse::<u64>();
                            if num.is_err() {
                                panic!("invalid number of u64 type: {}", num_s);
                            }

                            let num = num.unwrap();
                            let info = EInfo::U64 {
                                value: Some(num),
                                from: *from,
                            };
                            let src = self.code.ins.get(*from).unwrap().src.clone();
                            let val_ty = ValueType {
                                tag: ValueTypeTag::U64, 
                                src, 
                                data: vec![],
                            };
                            (val_ty, info)
                        }
                        TypeSignatureTag::UsizeTS => {
                            let num = num_s.parse::<usize>();
                            if num.is_err() {
                                panic!("invalid number of usize type: {}", num_s);
                            }

                            let num = num.unwrap();
                            let info = EInfo::Usize {
                                value: Some(num),
                                from: *from,
                            };
                            let src = self.code.ins.get(*from).unwrap().src.clone();
                            let val_ty = ValueType {
                                tag: ValueTypeTag::Usize, 
                                src, 
                                data: vec![],
                            };
                            (val_ty, info)
                        }

                        _ => unreachable!("unreachable code for number promotion")
                    }
                } else {
                    let num_s = self.read_str(str_i);
                    let num = num_s.parse::<i32>();
                    if num.is_err() {
                        panic!("invalid number of i32 type: {}", num_s);
                    }

                    let num = num.unwrap();
                    let info = EInfo::I32 {
                        value: Some(num),
                        from: *from,
                    };
                    let src = self.code.ins.get(*from).unwrap().src.clone();
                    let val_ty = ValueType {
                        tag: ValueTypeTag::I32, 
                        src,
                        data: vec![],
                    };
                    (val_ty, info)
                }
            }
            EInfo::Char { from, ..} => {
                let src = self.code.ins.get(*from).unwrap().src.clone();
                let val_ty = ValueType {
                    tag: ValueTypeTag::Char, 
                    src, 
                    data: vec![],
                };
                (val_ty, info.clone())
            }
            EInfo::Str { from, ..} => {
                let src = self.code.ins.get(*from).unwrap().src.clone();
                let val_ty = ValueType {
                    tag: ValueTypeTag::Str, 
                    src, 
                    data: vec![],
                };
                (val_ty, info.clone())
            }
            EInfo::NoInfo => panic!("no information available. this should not occur."),
            EInfo::I8 { from, ..} => {
                let src = self.code.ins.get(*from).unwrap().src.clone();
                let val_ty = ValueType {
                    tag: ValueTypeTag::I8, 
                    src, 
                    data: vec![],
                };
                (val_ty, info.clone())
            }
            EInfo::I16 { from, ..} => {
                let src = self.code.ins.get(*from).unwrap().src.clone();
                let val_ty = ValueType {
                    tag: ValueTypeTag::I16, 
                    src, 
                    data: vec![],
                };
                (val_ty, info.clone())
            }
            EInfo::I32 { from, ..} => {
                let src = self.code.ins.get(*from).unwrap().src.clone();
                let val_ty = ValueType {
                    tag: ValueTypeTag::I32, 
                    src, 
                    data: vec![],
                };
                (val_ty, info.clone())
            }
            EInfo::I64 { from, ..} => {
                let src = self.code.ins.get(*from).unwrap().src.clone();
                let val_ty = ValueType {
                    tag: ValueTypeTag::I64, 
                    src, 
                    data: vec![],
                };
                (val_ty, info.clone())
            }
            EInfo::Isize { from, ..} => {
                let src = self.code.ins.get(*from).unwrap().src.clone();
                let val_ty = ValueType {
                    tag: ValueTypeTag::Isize, 
                    src, 
                    data: vec![],
                };
                (val_ty, info.clone())
            }
            EInfo::U8 { from, ..} => {
                let src = self.code.ins.get(*from).unwrap().src.clone();
                let val_ty = ValueType {
                    tag: ValueTypeTag::U8, 
                    src, 
                    data: vec![],
                };
                (val_ty, info.clone())
            }
            EInfo::U16 { from, ..} => {
                let src = self.code.ins.get(*from).unwrap().src.clone();
                let val_ty = ValueType {
                    tag: ValueTypeTag::U16, 
                    src, 
                    data: vec![],
                };
                (val_ty, info.clone())
            }
            EInfo::U32 { from, ..} => {
                let src = self.code.ins.get(*from).unwrap().src.clone();
                let val_ty = ValueType {
                    tag: ValueTypeTag::U32, 
                    src, 
                    data: vec![],
                };
                (val_ty, info.clone())
            }
            EInfo::U64 { from, ..} => {
                let src = self.code.ins.get(*from).unwrap().src.clone();
                let val_ty = ValueType {
                    tag: ValueTypeTag::U64, 
                    src, 
                    data: vec![],
                };
                (val_ty, info.clone())
            }
            EInfo::Usize { from, ..} => {
                let src = self.code.ins.get(*from).unwrap().src.clone();
                let val_ty = ValueType {
                    tag: ValueTypeTag::Usize, 
                    src, 
                    data: vec![],
                };
                (val_ty, info.clone())
            }
            EInfo::ReferenceToType { type_i, .. } => {
                let src = self.code.ins.get(type_i.index).unwrap().src.clone();
                let val_ty = ValueType {
                    tag: ValueTypeTag::Type,
                    src,
                    data: vec![type_i.clone()]
                };
                (val_ty, info.clone())
            }
            EInfo::Bool { from, .. } => {
                let src = self.code.ins.get(*from).unwrap().src.clone();
                let val_ty = ValueType {
                    tag: ValueTypeTag::Bool,
                    src,
                    data: vec![],
                };
                (val_ty, info.clone())
            }
            EInfo::StaticArray { item_type_i, .. } => {
                let src = self.code.ins.get(item_type_i.index).unwrap().src.clone();
                let val_ty = ValueType {
                    tag: ValueTypeTag::StaticArray,
                    src,
                    data: vec![item_type_i.clone()],
                };
                (val_ty, info.clone())
            }
            EInfo::Function { fn_type_i, from, .. } => {
                let src = self.code.ins.get(*from).unwrap().src.clone();
                let fn_type = &self.code.types[fn_type_i.index];
                let val_ty = ValueType {
                    tag: ValueTypeTag::Function,
                    src,
                    data: fn_type.indices.clone(),
                };
                (val_ty, info.clone())
            }
            _ => unimplemented!("unimplemented information type: {:#?}", info),
        }
    }
}

// more complex functions
#[allow(dead_code)]
impl Engine {
    pub fn pass_1(&mut self) -> SymbolTable {
        // this pass is used to learn about the types and constants in the code
        // for example, global constants and types are declared here
        let sym_table = SymbolTable::new(TableType::Preserved);
        let code = self.code.clone();
        for (loc, ins) in code.ins.iter().enumerate() {
            // println!("pass_1: {:#?}", ins.tag);
            match ins.tag {
                CodeTag::LIStr => {
                    // LIStr "string"
                    let str_i = ins.data[0];
                    let info = EInfo::Str {
                        value: Some(self.read_str(&str_i)),
                        from: loc,
                    };
                    self.information.push(info);
                }
                CodeTag::LIChar => {
                    // LIChar 'c'
                    let char_i = ins.data[0];
                    let info = EInfo::Char {
                        value: Some(self.read_str(&char_i).chars().next().unwrap()),
                        from: loc,
                    };
                    self.information.push(info);
                }
                CodeTag::LINum => {
                    // LINum "12233"
                    let num_i = ins.data[0];
                    let info = EInfo::ImmediateNum {
                        str_i: num_i,
                        from: loc,
                    };
                    self.information.push(info);
                }
                CodeTag::NewConstant => {
                }
                CodeTag::NameRef => {
                }
                CodeTag::SrcComment => {
                }
                CodeTag::TypeRef => {
                }
                CodeTag::LoadTrue => {
                }
                CodeTag::LoadFalse => {
                }
                CodeTag::Not => {
                }
                CodeTag::Add => {
                }
                CodeTag::Sub => {
                }
                CodeTag::Mult => {
                }
                CodeTag::Div => {
                }
                CodeTag::Modulo => {
                }
                CodeTag::Negate => {
                }
                CodeTag::Eq => {
                }
                CodeTag::Neq => {
                }
                CodeTag::Lt => {
                }
                CodeTag::Gt => {
                }
                CodeTag::LtEq => {
                }
                CodeTag::GtEq => {
                }
                CodeTag::And => {
                }
                CodeTag::Or => {
                }
                CodeTag::MakeStaticArray => {
                }
                CodeTag::AccessIndex => {
                }
                CodeTag::NewFunction => {
                }
                CodeTag::EnterScope => {
                }
                CodeTag::ExitScope => {
                }
                _ => unimplemented!("pass_1: {:#?}", ins.tag),
            }
        }
        sym_table
    }

    pub fn run(&mut self) {
        self.enter_scope();
        let code = self.code.clone();
        for (loc, ins) in code.ins.iter().enumerate() {
            // println!("executing: {:#?}", ins.tag);
            match ins.tag {
                CodeTag::LIStr => {
                    // LIStr "string"
                    let str_i = ins.data[0];
                    let info = EInfo::Str {
                        value: Some(self.read_str(&str_i)),
                        from: loc,
                    };
                    self.information.push(info);
                }
                CodeTag::LIChar => {
                    // LIChar 'c'
                    let char_i = ins.data[0];
                    let info = EInfo::Char {
                        value: Some(self.read_str(&char_i).chars().next().unwrap()),
                        from: loc,
                    };
                    self.information.push(info);
                }
                CodeTag::LINum => {
                    // LINum "12233"
                    let num_i = ins.data[0];
                    let info = EInfo::ImmediateNum {
                        str_i: num_i,
                        from: loc,
                    };
                    self.information.push(info);
                }
                CodeTag::NewConstant => {
                    // NewConstant "name" TypeSignature:19? Info:20
                    let name_i = ins.data[0];
                    let (type_i, init_i) = if ins.data.len() == 2 {
                        (None, ins.data[1])
                    } else {
                        (Some(ins.data[1]), ins.data[2])
                    };

                    if let Some(type_i) = type_i {
                        // verify that type_i is a valid type
                        if !self.verify_type_sig(&type_i) {
                            let type_sig = &self.code.types[type_i.index];
                            panic!("reference to invalid type: {:?}", type_sig.tag);
                        }

                        // get the type of the init_i value
                        let (val_ty, add_info) = self.infer_type(&init_i, Some(&type_i));
                        if !self.verify_value_type(&val_ty) {
                            panic!("invalid type for constant initializer: {:?}", val_ty.tag);
                        }
                        
                        // if the type is a TypeNameRefTS (essentially not a built-in type)
                        // so it is an identifier, then we need to resolve the actual type it
                        // refers to
                        let mut type_sig = &self.code.types[type_i.index];
                        if type_sig.tag == TypeSignatureTag::TypeNameRefTS {
                            let name = self.read_str(&type_sig.indices[0]);
                            if !self.check_name(&name) {
                                panic!("name {} was not found.", name);
                            }
                            let info = self.get_info_for_name(&name).unwrap();
                            type_sig = match info {
                                EInfo::ReferenceToType { type_i, .. } => {
                                    &self.code.types[type_i.index]
                                }
                                _ => panic!("expected reference to type, found: {:#?}", info),
                            };
                        }

                        // make sure the type sig of type_i accepts val_ty
                        if !self.type_sig_accepts(type_sig, &val_ty) {
                            panic!("type {:?} does not accept value type {:?}", type_sig.tag, val_ty.tag);
                        }

                        // add the information to the environment
                        let name = self.read_str(&name_i); 
                        self.cur_scope().declare_constant(name, val_ty, add_info);
                    } else {
                        // get the type of the init_i value
                        let (val_ty, add_info) = self.infer_type(&init_i, None);
                        if !self.verify_value_type(&val_ty) {
                            panic!("invalid type for constant initializer: {:?}", val_ty.tag);
                        }

                        // add the information to the environment
                        let name = self.read_str(&name_i);
                        self.cur_scope().declare_constant(name, val_ty, add_info);
                    }
                    self.push_no_info()
                }
                CodeTag::NameRef => {
                    // NameRef "name"
                    let name_i = ins.data[0];
                    let name = self.read_str(&name_i);
                    if !self.check_name(&name) {
                        panic!("name {} was not found", name);
                    }
                    let info = self.get_info_for_name(&name).unwrap();
                    self.information.push(info);
                }
                CodeTag::SrcComment => {
                    // SrcComment "comment"
                    self.push_no_info();
                }
                CodeTag::TypeRef => {
                    // TypeRef TypeSignature:19
                    let type_i = ins.data[0];
                    if !self.verify_type_sig(&type_i) {
                        let type_sig = &self.code.types[type_i.index];
                        panic!("reference to invalid type: {:?}", type_sig.tag);
                    }
                    let info = EInfo::ReferenceToType {
                        type_i,
                        from: loc,
                    };
                    self.information.push(info);
                }
                CodeTag::LoadTrue => {
                    // LoadTrue
                    let info = EInfo::Bool {
                        value: Some(true),
                        from: loc,
                    };
                    self.information.push(info);
                }
                CodeTag::LoadFalse => {
                    // LoadFalse
                    let info = EInfo::Bool {
                        value: Some(false),
                        from: loc,
                    };
                    self.information.push(info);
                }
                CodeTag::Not => {
                    // Not Code:19
                    let code_i = ins.data[0];
                    let info = self.information.get(code_i.index).unwrap();
                    let info = match info {
                        EInfo::Bool { value, ..} => {
                            let value = value.map(|x| !x);
                            EInfo::Bool {
                                value,
                                from: loc,
                            }
                        }
                        _ => panic!("! operator used on a non-boolean value: {:#?}", info),
                    };
                    self.information.push(info);
                }
                CodeTag::Add => {
                    // Add Code:19 Code:20
                    // allowed configurations
                    // str + char => str
                    // str + str => str
                    // i{8,16,32,64,size} + i{8,16,32,64,size} => i{8,16,32,64,size}
                    // u{8,16,32,64,size} + u{8,16,32,64,size} => u{8,16,32,64,size}
                    // TODO(@pepplejoshua): look into checking for overflow or underflow manually

                    let a_i = ins.data[0];
                    let b_i = ins.data[1];

                    let (_, a_info) = self.infer_type(&a_i, None);
                    let (_, b_info) = self.infer_type(&b_i, None);

                    match (&a_info, &b_info) {
                        // TODO(@pepplejoshua): since we can kind of tell whether we have constant values within info
                        // and can already execute some of these operations, use the alternative (None for value) to
                        // generate the code for the operation in the target language or backend
                        (EInfo::Str { value: Some(a), .. }, EInfo::Str { value: Some(b), .. }) => {
                            let info = EInfo::Str {
                                value: Some(format!("{}{}", a, b)),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Str { value: Some(a), .. }, EInfo::Char { value: Some(b), .. }) => {
                            let info = EInfo::Str {
                                value: Some(format!("{}{}", a, b)),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I8 { value: Some(a), .. }, EInfo::I8 { value: Some(b), .. }) => {
                            let info = EInfo::I8 {
                                value: Some(a + b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I16 { value: Some(a), .. }, EInfo::I16 { value: Some(b), .. }) => {
                            let info = EInfo::I16 {
                                value: Some(a + b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I32 { value: Some(a), .. }, EInfo::I32 { value: Some(b), .. }) => {
                            let info = EInfo::I32 {
                                value: Some(a + b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I64 { value: Some(a), .. }, EInfo::I64 { value: Some(b), .. }) => {
                            let info = EInfo::I64 {
                                value: Some(a + b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Isize { value: Some(a), .. }, EInfo::Isize { value: Some(b), .. }) => {
                            let info = EInfo::Isize {
                                value: Some(a + b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U8 { value: Some(a), .. }, EInfo::U8 { value: Some(b), .. }) => {
                            let info = EInfo::U8 {
                                value: Some(a + b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U16 { value: Some(a), .. }, EInfo::U16 { value: Some(b), .. }) => {
                            let info = EInfo::U16 {
                                value: Some(a + b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U32 { value: Some(a), .. }, EInfo::U32 { value: Some(b), .. }) => {
                            let info = EInfo::U32 {
                                value: Some(a + b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U64 { value: Some(a), .. }, EInfo::U64 { value: Some(b), .. }) => {
                            let info = EInfo::U64 {
                                value: Some(a + b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Usize { value: Some(a), .. }, EInfo::Usize { value: Some(b), .. }) => {
                            let info = EInfo::Usize {
                                value: Some(a + b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        _ => panic!("invalid types for +: {:?} and {:?}", a_info, b_info),
                    }
                }
                CodeTag::Sub => {
                    // Sub Code:19 Code:20
                    // allowed configurations
                    // i{8,16,32,64,size} - i{8,16,32,64,size} => i{8,16,32,64,size}
                    // u{8,16,32,64,size} - u{8,16,32,64,size} => u{8,16,32,64,size}
                    // TODO(@pepplejoshua): look into checking for overflow or underflow manually

                    let a_i = ins.data[0];
                    let b_i = ins.data[1];

                    let (_, a_info) = self.infer_type(&a_i, None);
                    let (_, b_info) = self.infer_type(&b_i, None);

                    match (&a_info, &b_info) {
                        // TODO(@pepplejoshua): since we can kind of tell whether we have constant values within info
                        // and can already execute some of these operations, use the alternative (None for value) to
                        // generate the code for the operation in the target language or backend
                        (EInfo::I8 { value: Some(a), .. }, EInfo::I8 { value: Some(b), .. }) => {
                            let info = EInfo::I8 {
                                value: Some(a - b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I16 { value: Some(a), .. }, EInfo::I16 { value: Some(b), .. }) => {
                            let info = EInfo::I16 {
                                value: Some(a - b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I32 { value: Some(a), .. }, EInfo::I32 { value: Some(b), .. }) => {
                            let info = EInfo::I32 {
                                value: Some(a - b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I64 { value: Some(a), .. }, EInfo::I64 { value: Some(b), .. }) => {
                            let info = EInfo::I64 {
                                value: Some(a - b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Isize { value: Some(a), .. }, EInfo::Isize { value: Some(b), .. }) => {
                            let info = EInfo::Isize {
                                value: Some(a - b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U8 { value: Some(a), .. }, EInfo::U8 { value: Some(b), .. }) => {
                            let info = EInfo::U8 {
                                value: Some(a - b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U16 { value: Some(a), .. }, EInfo::U16 { value: Some(b), .. }) => {
                            let info = EInfo::U16 {
                                value: Some(a - b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U32 { value: Some(a), .. }, EInfo::U32 { value: Some(b), .. }) => {
                            let info = EInfo::U32 {
                                value: Some(a - b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U64 { value: Some(a), .. }, EInfo::U64 { value: Some(b), .. }) => {
                            let info = EInfo::U64 {
                                value: Some(a - b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Usize { value: Some(a), .. }, EInfo::Usize { value: Some(b), .. }) => {
                            let info = EInfo::Usize {
                                value: Some(a - b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        _ => panic!("invalid types for -: {:?} and {:?}", a_info, b_info),
                    }
                }
                CodeTag::Mult => {
                    // Mult Code:19 Code:20
                    // allowed configurations
                    // i{8,16,32,64,size} * i{8,16,32,64,size} => i{8,16,32,64,size}
                    // u{8,16,32,64,size} * u{8,16,32,64,size} => u{8,16,32,64,size}
                    // TODO(@pepplejoshua): look into checking for overflow or underflow manually

                    let a_i = ins.data[0];
                    let b_i = ins.data[1];

                    let (_, a_info) = self.infer_type(&a_i, None);
                    let (_, b_info) = self.infer_type(&b_i, None);

                    match (&a_info, &b_info) {
                        (EInfo::I8 { value: Some(a), .. }, EInfo::I8 { value: Some(b), .. }) => {
                            let info = EInfo::I8 {
                                value: Some(a * b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I16 { value: Some(a), .. }, EInfo::I16 { value: Some(b), .. }) => {
                            let info = EInfo::I16 {
                                value: Some(a * b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I32 { value: Some(a), .. }, EInfo::I32 { value: Some(b), .. }) => {
                            let info = EInfo::I32 {
                                value: Some(a * b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I64 { value: Some(a), .. }, EInfo::I64 { value: Some(b), .. }) => {
                            let info = EInfo::I64 {
                                value: Some(a * b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Isize { value: Some(a), .. }, EInfo::Isize { value: Some(b), .. }) => {
                            let info = EInfo::Isize {
                                value: Some(a * b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U8 { value: Some(a), .. }, EInfo::U8 { value: Some(b), .. }) => {
                            let info = EInfo::U8 {
                                value: Some(a * b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U16 { value: Some(a), .. }, EInfo::U16 { value: Some(b), .. }) => {
                            let info = EInfo::U16 {
                                value: Some(a * b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U32 { value: Some(a), .. }, EInfo::U32 { value: Some(b), .. }) => {
                            let info = EInfo::U32 {
                                value: Some(a * b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U64 { value: Some(a), .. }, EInfo::U64 { value: Some(b), .. }) => {
                            let info = EInfo::U64 {
                                value: Some(a * b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Usize { value: Some(a), .. }, EInfo::Usize { value: Some(b), .. }) => {
                            let info = EInfo::Usize {
                                value: Some(a * b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        _ => panic!("invalid types for *: {:?} and {:?}", a_info, b_info),
                    }
                }
                CodeTag::Div => {
                    // Div Code:19 Code:20
                    // allowed configurations
                    // i{8,16,32,64,size} / i{8,16,32,64,size} => i{8,16,32,64,size}
                    // u{8,16,32,64,size} / u{8,16,32,64,size} => u{8,16,32,64,size}
                    // TODO(@pepplejoshua): look into checking for overflow or underflow manually

                    let a_i = ins.data[0];
                    let b_i = ins.data[1];

                    let (_, a_info) = self.infer_type(&a_i, None);
                    let (_, b_info) = self.infer_type(&b_i, None);

                    match (&a_info, &b_info) {
                        (EInfo::I8 { value: Some(a), .. }, EInfo::I8 { value: Some(b), .. }) => {
                            if b == &0 {
                                panic!("division by zero is not allowed");
                            }
                            let info = EInfo::I8 {
                                value: Some(a / b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I16 { value: Some(a), .. }, EInfo::I16 { value: Some(b), .. }) => {
                            if b == &0 {
                                panic!("division by zero is not allowed");
                            }
                            let info = EInfo::I16 {
                                value: Some(a / b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I32 { value: Some(a), .. }, EInfo::I32 { value: Some(b), .. }) => {
                            if b == &0 {
                                panic!("division by zero is not allowed");
                            }
                            let info = EInfo::I32 {
                                value: Some(a / b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I64 { value: Some(a), .. }, EInfo::I64 { value: Some(b), .. }) => {
                            if b == &0 {
                                panic!("division by zero is not allowed");
                            }
                            let info = EInfo::I64 {
                                value: Some(a / b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Isize { value: Some(a), .. }, EInfo::Isize { value: Some(b), .. }) => {
                            if b == &0 {
                                panic!("division by zero is not allowed");
                            }
                            let info = EInfo::Isize {
                                value: Some(a / b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U8 { value: Some(a), .. }, EInfo::U8 { value: Some(b), .. }) => {
                            if b == &0 {
                                panic!("division by zero is not allowed");
                            }
                            let info = EInfo::U8 {
                                value: Some(a / b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U16 { value: Some(a), .. }, EInfo::U16 { value: Some(b), .. }) => {
                            if b == &0 {
                                panic!("division by zero is not allowed");
                            }
                            let info = EInfo::U16 {
                                value: Some(a / b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U32 { value: Some(a), .. }, EInfo::U32 { value: Some(b), .. }) => {
                            if b == &0 {
                                panic!("division by zero is not allowed");
                            }
                            let info = EInfo::U32 {
                                value: Some(a / b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U64 { value: Some(a), .. }, EInfo::U64 { value: Some(b), .. }) => {
                            if b == &0 {
                                panic!("division by zero is not allowed");
                            }
                            let info = EInfo::U64 {
                                value: Some(a / b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Usize { value: Some(a), .. }, EInfo::Usize { value: Some(b), .. }) => {
                            if b == &0 {
                                panic!("division by zero is not allowed");
                            }
                            let info = EInfo::Usize {
                                value: Some(a / b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        _ => panic!("invalid types for /: {:?} and {:?}", a_info, b_info),
                    }
                }
                CodeTag::Modulo => {
                    // Modulo Code:19 Code:20
                    // allowed configurations
                    // i{8,16,32,64,size} % i{8,16,32,64,size} => i{8,16,32,64,size}
                    // u{8,16,32,64,size} % u{8,16,32,64,size} => u{8,16,32,64,size}

                    let a_i = ins.data[0];
                    let b_i = ins.data[1];

                    let (_, a_info) = self.infer_type(&a_i, None);
                    let (_, b_info) = self.infer_type(&b_i, None);

                    match (&a_info, &b_info) {
                        (EInfo::I8 { value: Some(a), .. }, EInfo::I8 { value: Some(b), .. }) => {
                            if b == &0 {
                                panic!("division by zero is not allowed");
                            }
                            let info = EInfo::I8 {
                                value: Some(a % b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I16 { value: Some(a), .. }, EInfo::I16 { value: Some(b), .. }) => {
                            if b == &0 {
                                panic!("division by zero is not allowed");
                            }
                            let info = EInfo::I16 {
                                value: Some(a % b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I32 { value: Some(a), .. }, EInfo::I32 { value: Some(b), .. }) => {
                            if b == &0 {
                                panic!("division by zero is not allowed");
                            }
                            let info = EInfo::I32 {
                                value: Some(a % b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I64 { value: Some(a), .. }, EInfo::I64 { value: Some(b), .. }) => {
                            if b == &0 {
                                panic!("division by zero is not allowed");
                            }
                            let info = EInfo::I64 {
                                value: Some(a % b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Isize { value: Some(a), .. }, EInfo::Isize { value: Some(b), .. }) => {
                            if b == &0 {
                                panic!("division by zero is not allowed");
                            }
                            let info = EInfo::Isize {
                                value: Some(a % b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U8 { value: Some(a), .. }, EInfo::U8 { value: Some(b), .. }) => {
                            if b == &0 {
                                panic!("division by zero is not allowed");
                            }
                            let info = EInfo::U8 {
                                value: Some(a % b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U16 { value: Some(a), .. }, EInfo::U16 { value: Some(b), .. }) => {
                            if b == &0 {
                                panic!("division by zero is not allowed");
                            }
                            let info = EInfo::U16 {
                                value: Some(a % b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U32 { value: Some(a), .. }, EInfo::U32 { value: Some(b), .. }) => {
                            if b == &0 {
                                panic!("division by zero is not allowed");
                            }
                            let info = EInfo::U32 {
                                value: Some(a % b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U64 { value: Some(a), .. }, EInfo::U64 { value: Some(b), .. }) => {
                            if b == &0 {
                                panic!("division by zero is not allowed");
                            }
                            let info = EInfo::U64 {
                                value: Some(a % b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Usize { value: Some(a), .. }, EInfo::Usize { value: Some(b), .. }) => {
                            if b == &0 {
                                panic!("division by zero is not allowed");
                            }
                            let info = EInfo::Usize {
                                value: Some(a % b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        _ => panic!("invalid types for %: {:?} and {:?}", a_info, b_info),
                    }
                }
                CodeTag::Negate => {
                    // Negate Code:19
                    // allowed configurations
                    // -i{8,16,32,64,size} => i{8,16,32,64,size}

                    let a_i = ins.data[0];

                    let (_, a_info) = self.infer_type(&a_i, None);

                    match &a_info {
                        EInfo::I8 { value: Some(a), .. } => {
                            let info = EInfo::I8 {
                                value: Some(-a),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        EInfo::I16 { value: Some(a), .. } => {
                            let info = EInfo::I16 {
                                value: Some(-a),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        EInfo::I32 { value: Some(a), .. } => {
                            let info = EInfo::I32 {
                                value: Some(-a),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        EInfo::I64 { value: Some(a), .. } => {
                            let info = EInfo::I64 {
                                value: Some(-a),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        EInfo::Isize { value: Some(a), .. } => {
                            let info = EInfo::Isize {
                                value: Some(-a),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        _ => panic!("invalid type for unary -: {:?}", a_info),
                    }
                }
                CodeTag::Eq => {
                    // Eq Code:19 Code:20
                    // allowed configurations
                    // i{8,16,32,64,size} == i{8,16,32,64,size} => bool
                    // u{8,16,32,64,size} == u{8,16,32,64,size} => bool
                    // str == str => bool
                    // char == char => bool
                    // bool == bool => bool

                    let a_i = ins.data[0];
                    let b_i = ins.data[1];

                    let (_, a_info) = self.infer_type(&a_i, None);
                    let (_, b_info) = self.infer_type(&b_i, None);

                    match (&a_info, &b_info) {
                        (EInfo::I8 { value: Some(a), .. }, EInfo::I8 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a == b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I16 { value: Some(a), .. }, EInfo::I16 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a == b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I32 { value: Some(a), .. }, EInfo::I32 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a == b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I64 { value: Some(a), .. }, EInfo::I64 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a == b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Isize { value: Some(a), .. }, EInfo::Isize { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a == b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U8 { value: Some(a), .. }, EInfo::U8 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a == b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U16 { value: Some(a), .. }, EInfo::U16 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a == b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U32 { value: Some(a), .. }, EInfo::U32 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a == b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U64 { value: Some(a), .. }, EInfo::U64 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a == b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Usize { value: Some(a), .. }, EInfo::Usize { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a == b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Str { value: Some(a), .. }, EInfo::Str { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a == b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Char { value: Some(a), .. }, EInfo::Char { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a == b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Bool { value: Some(a), .. }, EInfo::Bool { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a == b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        _ => panic!("invalid types for ==: {:?} and {:?}", a_info, b_info),
                    }
                }
                CodeTag::Neq => {
                    // Neq Code:19 Code:20
                    // allowed configurations
                    // i{8,16,32,64,size} != i{8,16,32,64,size} => bool
                    // u{8,16,32,64,size} != u{8,16,32,64,size} => bool
                    // str != str => bool
                    // char != char => bool
                    // bool != bool => bool

                    let a_i = ins.data[0];
                    let b_i = ins.data[1];

                    let (_, a_info) = self.infer_type(&a_i, None);
                    let (_, b_info) = self.infer_type(&b_i, None);

                    match (&a_info, &b_info) {
                        (EInfo::I8 { value: Some(a), .. }, EInfo::I8 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a != b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I16 { value: Some(a), .. }, EInfo::I16 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a != b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I32 { value: Some(a), .. }, EInfo::I32 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a != b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I64 { value: Some(a), .. }, EInfo::I64 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a != b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Isize { value: Some(a), .. }, EInfo::Isize { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a != b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U8 { value: Some(a), .. }, EInfo::U8 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a != b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U16 { value: Some(a), .. }, EInfo::U16 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a != b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U32 { value: Some(a), .. }, EInfo::U32 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a != b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U64 { value: Some(a), .. }, EInfo::U64 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a != b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Usize { value: Some(a), .. }, EInfo::Usize { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a != b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Str { value: Some(a), .. }, EInfo::Str { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a != b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Char { value: Some(a), .. }, EInfo::Char { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a != b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Bool { value: Some(a), .. }, EInfo::Bool { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a != b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        _ => panic!("invalid types for !=: {:?} and {:?}", a_info, b_info),
                    }
                }
                CodeTag::Lt => {
                    // Lt Code:19 Code:20
                    // allowed configurations
                    // i{8,16,32,64,size} < i{8,16,32,64,size} => bool
                    // u{8,16,32,64,size} < u{8,16,32,64,size} => bool
                    // char < char => bool

                    let a_i = ins.data[0];
                    let b_i = ins.data[1];

                    let (_, a_info) = self.infer_type(&a_i, None);  
                    let (_, b_info) = self.infer_type(&b_i, None);

                    match (&a_info, &b_info) {
                        (EInfo::I8 { value: Some(a), .. }, EInfo::I8 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a < b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I16 { value: Some(a), .. }, EInfo::I16 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a < b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I32 { value: Some(a), .. }, EInfo::I32 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a < b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I64 { value: Some(a), .. }, EInfo::I64 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a < b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Isize { value: Some(a), .. }, EInfo::Isize { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a < b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U8 { value: Some(a), .. }, EInfo::U8 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a < b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U16 { value: Some(a), .. }, EInfo::U16 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a < b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U32 { value: Some(a), .. }, EInfo::U32 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a < b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U64 { value: Some(a), .. }, EInfo::U64 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a < b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Usize { value: Some(a), .. }, EInfo::Usize { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a < b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Char { value: Some(a), .. }, EInfo::Char { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a < b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        _ => panic!("invalid types for <: {:?} and {:?}", a_info, b_info),
                    }
                }
                CodeTag::Gt => {
                    // Gt Code:19 Code:20
                    // allowed configurations
                    // i{8,16,32,64,size} > i{8,16,32,64,size} => bool
                    // u{8,16,32,64,size} > u{8,16,32,64,size} => bool
                    // char > char => bool

                    let a_i = ins.data[0];
                    let b_i = ins.data[1];

                    let (_, a_info) = self.infer_type(&a_i, None);
                    let (_, b_info) = self.infer_type(&b_i, None);

                    match (&a_info, &b_info) {
                        (EInfo::I8 { value: Some(a), .. }, EInfo::I8 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a > b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I16 { value: Some(a), .. }, EInfo::I16 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a > b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I32 { value: Some(a), .. }, EInfo::I32 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a > b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I64 { value: Some(a), .. }, EInfo::I64 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a > b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Isize { value: Some(a), .. }, EInfo::Isize { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a > b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U8 { value: Some(a), .. }, EInfo::U8 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a > b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U16 { value: Some(a), .. }, EInfo::U16 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a > b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U32 { value: Some(a), .. }, EInfo::U32 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a > b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U64 { value: Some(a), .. }, EInfo::U64 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a > b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Usize { value: Some(a), .. }, EInfo::Usize { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a > b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Char { value: Some(a), .. }, EInfo::Char { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a > b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        _ => panic!("invalid types for >: {:?} and {:?}", a_info, b_info),
                    }
                }
                CodeTag::LtEq => {
                    // LtEq Code:19 Code:20
                    // allowed configurations
                    // i{8,16,32,64,size} <= i{8,16,32,64,size} => bool
                    // u{8,16,32,64,size} <= u{8,16,32,64,size} => bool
                    // char <= char => bool

                    let a_i = ins.data[0];
                    let b_i = ins.data[1];

                    let (_, a_info) = self.infer_type(&a_i, None);
                    let (_, b_info) = self.infer_type(&b_i, None);

                    match (&a_info, &b_info) {
                        (EInfo::I8 { value: Some(a), .. }, EInfo::I8 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a <= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I16 { value: Some(a), .. }, EInfo::I16 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a <= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I32 { value: Some(a), .. }, EInfo::I32 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a <= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I64 { value: Some(a), .. }, EInfo::I64 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a <= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Isize { value: Some(a), .. }, EInfo::Isize { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a <= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U8 { value: Some(a), .. }, EInfo::U8 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a <= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U16 { value: Some(a), .. }, EInfo::U16 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a <= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U32 { value: Some(a), .. }, EInfo::U32 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a <= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U64 { value: Some(a), .. }, EInfo::U64 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a <= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Usize { value: Some(a), .. }, EInfo::Usize { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a <= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Char { value: Some(a), .. }, EInfo::Char { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a <= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        _ => panic!("invalid types for <=: {:?} and {:?}", a_info, b_info),
                    }
                }
                CodeTag::GtEq => {
                    // GtEq Code:19 Code:20
                    // allowed configurations
                    // i{8,16,32,64,size} >= i{8,16,32,64,size} => bool
                    // u{8,16,32,64,size} >= u{8,16,32,64,size} => bool
                    // char >= char => bool

                    let a_i = ins.data[0];
                    let b_i = ins.data[1];

                    let (_, a_info) = self.infer_type(&a_i, None);
                    let (_, b_info) = self.infer_type(&b_i, None);

                    match (&a_info, &b_info) {
                        (EInfo::I8 { value: Some(a), .. }, EInfo::I8 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a >= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I16 { value: Some(a), .. }, EInfo::I16 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a >= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I32 { value: Some(a), .. }, EInfo::I32 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a >= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::I64 { value: Some(a), .. }, EInfo::I64 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a >= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Isize { value: Some(a), .. }, EInfo::Isize { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a >= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U8 { value: Some(a), .. }, EInfo::U8 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a >= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U16 { value: Some(a), .. }, EInfo::U16 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a >= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U32 { value: Some(a), .. }, EInfo::U32 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a >= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::U64 { value: Some(a), .. }, EInfo::U64 { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a >= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Usize { value: Some(a), .. }, EInfo::Usize { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a >= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        (EInfo::Char { value: Some(a), .. }, EInfo::Char { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(a >= b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        _ => panic!("invalid types for >=: {:?} and {:?}", a_info, b_info),
                    }
                }
                CodeTag::And => {
                    // And Code:19 Code:20
                    // allowed configurations
                    // bool && bool => bool

                    let a_i = ins.data[0];
                    let b_i = ins.data[1];

                    let (_, a_info) = self.infer_type(&a_i, None);
                    let (_, b_info) = self.infer_type(&b_i, None);

                    match (&a_info, &b_info) {
                        (EInfo::Bool { value: Some(a), .. }, EInfo::Bool { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(*a && *b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        _ => panic!("invalid types for &&: {:?} and {:?}", a_info, b_info),
                    }
                }
                CodeTag::Or => {
                    // Or Code:19 Code:20
                    // allowed configurations
                    // bool || bool => bool

                    let a_i = ins.data[0];
                    let b_i = ins.data[1];

                    let (_, a_info) = self.infer_type(&a_i, None);
                    let (_, b_info) = self.infer_type(&b_i, None);

                    match (&a_info, &b_info) {
                        (EInfo::Bool { value: Some(a), .. }, EInfo::Bool { value: Some(b), .. }) => {
                            let info = EInfo::Bool {
                                value: Some(*a || *b),
                                from: loc,
                            };
                            self.information.push(info);
                        }
                        _ => panic!("invalid types for ||: {:?} and {:?}", a_info, b_info),
                    }
                }
                CodeTag::MakeStaticArray => {
                    // MakeStaticArray Code:19 [Code:20, Code:21, ...]

                    // get the array type used to initialize this array
                    let array_ty_i = ins.data[0];
                    let array_ty = self.infer_type(&array_ty_i, None).1;

                    // we need a reference to a type to make a static array
                    if let EInfo::ReferenceToType { type_i, .. } = array_ty {
                        let arr_ty = self.code.get_type(&type_i);
                        // the reference has to be to a static array type
                        if matches!(arr_ty.tag, TypeSignatureTag::StaticArrayTS) {
                            // from this static array type, we grab the size and the type of the items
                            // in the array
                            let arr_size_i = arr_ty.indices[0];
                            let arr_item_ty_i = arr_ty.indices[1];

                            let arr_size = self.infer_type(&arr_size_i, None).1;
                            let (arr_item_ty, _) = self.infer_type(&arr_item_ty_i, None);
                            let arr_ty_i = arr_item_ty.data[0];
                            let arr_ty = self.code.get_type(&arr_ty_i);
                            let item_ty_i = arr_ty.indices[1];
                            let item_ty = self.code.get_type(&item_ty_i);

                            // we expect a known usize for the size of the array
                            match arr_size {
                                EInfo::Usize { value: Some(size), .. } => {
                                    // since we know the index for the type of the array (item_ty_i), we can use it
                                    // to infer and check the types of the items in the array
                                    let mut items = Vec::new();
                                    for i in 1..ins.data.len() {
                                        let item_i = ins.data[i];
                                        let (cur_item_ty, _) = self.infer_type(&item_i, Some(&item_ty_i));
                                        if self.type_sig_accepts(&item_ty, &cur_item_ty) {
                                            items.push(item_i);
                                        } else {
                                            panic!("expected type {:?} for array item, found {:?}", item_ty.tag, cur_item_ty.tag);
                                        }
                                    }
                                    // if the number of items in the array is the same as the size of the array,
                                    // we can infer the type of the array
                                    if items.len() == size {
                                        let info = EInfo::StaticArray { item_type_i: item_ty_i, from: loc, items };
                                        self.information.push(info);
                                    } else {
                                        panic!("expected {} items to initialize array, but found {}", size, items.len());
                                    }
                                }
                                EInfo::Usize { value: None, .. } => {
                                    panic!("expected constant usize known at compile time for array size");
                                }
                                _ => {
                                    panic!("expected usize for array size, found {:?}", arr_size);
                                }
                            }
                        } else {
                            panic!("expected static array type, found {:?}", arr_ty);
                        }
                    } else {
                        panic!("expected type as first index to MakeStaticArray: {:?}", array_ty);
                    }
                }
                CodeTag::AccessIndex => {
                    // AccessMember Code:19 Code:20
                    let arr_i = ins.data[0];
                    let index_i = ins.data[1];

                    let (_, arr_info) = self.infer_type(&arr_i, None);
                    let src = self.code.get_ins(index_i).src;
                    let usize_ty = TypeSignature {
                        tag: TypeSignatureTag::UsizeTS,
                        indices: Vec::new(),
                        src,
                    };
                    let usize_ty_i = self.code.add_type(usize_ty);
                    let (_, index_info) = self.infer_type(&index_i, Some(&usize_ty_i));
                    self.code.pop_last_type();

                    match (&arr_info, &index_info) {
                        (EInfo::StaticArray { item_type_i, items, .. }, EInfo::Usize { value: Some(index), .. }) => {
                            if *index >= items.len() {
                                panic!("index out of bounds: {} >= {}", index, items.len());
                            }
                            let item_i = items[*index];
                            let (_, item_info) = self.infer_type(&item_i, Some(&item_type_i));
                            self.information.push(item_info);
                        }
                        (EInfo::StaticArray { .. }, EInfo::Usize { value: None, .. }) => {
                            panic!("expected constant usize known at compile time for array index");
                        }
                        (EInfo::StaticArray { .. }, _) => {
                            panic!("expected usize for array index, found {:?}", index_info);
                        }
                        _ => panic!("expected static array type, found {:?}", arr_info),
                    }
                }
                CodeTag::NewFunction => {
                    // NewFunction "name" Type:2 Code:3
                    let name = ins.data[0];
                    let fn_type_i = ins.data[1];
                    let fn_body_end = ins.data[2];

                    if !self.verify_type_sig(&fn_type_i) {
                        panic!("invalid type signature used in function");
                    }

                    let fn_info = EInfo::Function { 
                        name,
                        fn_type_i,
                        fn_start_index: Index { tag: IndexTag::Code, index: loc },
                        fn_end_index: fn_body_end,
                        from: loc,
                    };
                    
                    self.information.push(fn_info);
                    let ret_type_i = Some(self.code.get_type(&fn_type_i).indices[0]);
                    let cur_env = self.cur_scope();
                    cur_env.cur_fn_end_index = Some(fn_body_end);
                    cur_env.cur_fn_return_ty_index = ret_type_i;
                    self.enter_scope();
                }
                CodeTag::EndFunction => {
                    // EndFunction
                    self.exit_scope();
                }
                CodeTag::EnterScope => {
                    // EnterScope
                    self.enter_scope();
                    let fn_end_i = self.cur_fn_end_index;
                    let fn_return_ty_i = self.cur_fn_return_ty_index;
                    let cur_scope = self.cur_scope();
                    cur_scope.cur_fn_end_index = fn_end_i;
                    cur_scope.cur_fn_return_ty_index = fn_return_ty_i;
                    self.cur_fn_end_index = None;
                    self.cur_fn_return_ty_index = None;
                    self.push_no_info();
                }
                CodeTag::ExitScope => {
                    // ExitScope
                    self.exit_scope();
                    self.push_no_info();
                }
                _ => panic!("unimplemented: {:?}", ins.tag),
            }
        }
        self.exit_scope();
    }
}

/*
first pass:
this pass will go through the bcode and populates the module GLOBAL symbol table with partial information
about the types of the symbols. the module GLOBAL table's parent will be the program GLOBAL symbol table.
- for each function, we will add a symbol to the symbol table with the name of the function and the type of the function
- for each global variable, we will add a symbol to the symbol table with the name of the variable and the type of the variable
- for each global constant, we will add a symbol to the symbol table with the name of the constant and the type of the constant
- for each type, we will add a symbol to the symbol table with the name of the type and the type of the type
  - struct
  - alias of other types

when we see an @import, we will go through Lex -> Parse -> Forge_Pass_1 for the path. it can be done in a different thread
it will result in a module GLOBAL symbol table that we will merge into the program GLOBAL symbol table.
*/

/*
second pass:
this pass will go through the bcode and populates the module GLOBAL symbol table with complete information, while also
populating the module GLOBAL type table with complete information.
*/