use core::panic;

use crate::frontend::{bcode::{CodeBundle, CodeTag, Index, IndexTag}, types::{ValueType, ValueTypeTag, TypeSignatureTag}};

use super::env::Env;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum EInfo {
    // store the string and the index it originated from
    ImmediateNum {
        str_i: Index,
        from: usize,
    },
    TypeNameRef,
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
        // let env = self.envs.last().unwrap();
        // for (name, (val_ty, info)) in env.names.iter() {
        //     println!("{}: {:?} {:?}", name, val_ty, info);
        // }

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

    fn verify_type_sig(&self, type_i: &Index) -> bool {
        let type_sig = &self.code.types[type_i.index];
        if type_sig.tag.is_simple_type_sig() {
            true
        } else {
            false
        }
    }

    fn verify_value_type(&self, val_ty: &ValueType) -> bool {
        if val_ty.tag.is_simple_value_type() {
            true
        } else {
            false
        }
    }

    fn type_sig_accepts(&self, type_i: &Index, val_ty: &ValueType) -> bool {
        let type_sig = &self.code.types[type_i.index];
        let _val_tag = val_ty.tag;
        println!("type sig: {:?} val_ty: {:?}", type_sig.tag.to_value_type_tag(), _val_tag);
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

                        // make sure the type sig of type_i accepts val_ty
                        if !self.type_sig_accepts(&type_i, &val_ty) {
                            let type_sig = &self.code.types[type_i.index];
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
                    if !self.cur_scope().check_name(&name) {
                        panic!("name {} not found in current scope", name);
                    }
                    let info = self.cur_scope().get_info_for_name(&name).unwrap();
                    self.information.push(info);
                }
                
                _ => panic!("unimplemented: {:?}", ins.tag),
            }
        }
        self.exit_scope();
    }
}