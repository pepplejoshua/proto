use core::panic;

use crate::frontend::{bcode::{CodeBundle, CodeTag, Index, IndexTag}, types::{ValueType, ValueTypeTag, TypeSignatureTag, TypeSignature}};

use super::env::Env;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum EInfo {
    // store the string and the index it originated from
    ImmediateNum {
        str_i: Index,
        from: usize,
    },
    ReferenceToType {
        type_i: Index,
        from: usize,
    },
    Bool {
        value: Option<bool>, // this will be None if the value is not known
        from: usize,
    },
    Char {
        value: Option<char>, // this will be None if the value is not known
        from: usize,
    },
    Void { from: usize },
    Str {
        value: Option<String>, // this will be None if the value is not known
        from: usize,
    },
    Type,
    I8 {
        value: Option<i8>, // this will be None if the value is not known
        from: usize,
    },
    I16 {
        value: Option<i16>, // this will be None if the value is not known
        from: usize,
    },
    I32 {
        value: Option<i32>, // this will be None if the value is not known
        from: usize,
    },
    I64 {
        value: Option<i64>, // this will be None if the value is not known
        from: usize,
    },
    Isize {
        value: Option<isize>, // this will be None if the value is not known
        from: usize,
    },
    U8 {
        value: Option<u8>, // this will be None if the value is not known
        from: usize,
    },
    U16 {
        value: Option<u16>, // this will be None if the value is not known
        from: usize,
    },
    U32 {
        value: Option<u32>, // this will be None if the value is not known
        from: usize,
    },
    U64 {
        value: Option<u64>, // this will be None if the value is not known
        from: usize,
    },
    Usize {
        value: Option<usize>, // this will be None if the value is not known
        from: usize,
    },
    NoInfo,
    Error {
        msg: String,
        from: usize,
    },
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Engine {
    envs: Vec<Env>,
    information: Vec<EInfo>,
    code: CodeBundle,
}

// simple helper functions
#[allow(dead_code)]
impl Engine {
    pub fn new(code: CodeBundle) -> Self {
        Engine {
            envs: vec![],
            information: Vec::new(),
            code,
        }
    }

    fn enter_scope(&mut self) {
        let env = Env::new();
        self.envs.push(env);
    }

    fn exit_scope(&mut self) {
        // display information about the current scope and then pop it
        let env = self.envs.last().unwrap();
        for (name, (val_ty, info)) in env.names.iter() {
            println!("{}\n  type: {:#?}, info: {:?}", name, val_ty.tag, info);
        }

        self.envs.pop();
    }

    fn cur_scope(&mut self) -> &mut Env {
        self.envs.last_mut().unwrap()
    }

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

    fn read_str(&self, loc: &Index) -> String {
        assert!(matches!(loc.tag, IndexTag::String), "expected string index");
        self.code.strings[loc.index].clone()
    }

    fn push_no_info(&mut self) {
        self.information.push(EInfo::NoInfo);
    }

    fn verify_type_sig(&mut self, type_i: &Index) -> bool {
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
                    data: vec![],
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
            _ => unimplemented!("unimplemented information type: {:#?}", info),
        }
    }
}

// more complex functions
#[allow(dead_code)]
impl Engine {
    pub fn run(&mut self) {
        self.enter_scope();
        let code = self.code.clone();
        for (loc, ins) in code.ins.iter().enumerate() {
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
                        _ => panic!("not used on a non-boolean value: {:#?}", info),
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
                        _ => panic!("invalid types for add: {:?} and {:?}", a_info, b_info),
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
                        _ => panic!("invalid types for sub: {:?} and {:?}", a_info, b_info),
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
                        _ => panic!("invalid types for mult: {:?} and {:?}", a_info, b_info),
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
                        _ => panic!("invalid types for div: {:?} and {:?}", a_info, b_info),
                    }
                }
                _ => panic!("unimplemented: {:?}", ins.tag),
            }
        }
        self.exit_scope();
    }
}