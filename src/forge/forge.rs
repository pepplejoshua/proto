use super::env::{Env, EnvironmentIndex};
use crate::frontend::{
    bcode::{CodeBundle, CodeTag, Index, IndexTag},
    source::{SourceFile, SourceRef},
    types::{TSTag, TypeSignature},
};

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum ForgeInfo {
    ImmStr {
        str_i: Index,
        len: usize,
        src: SourceRef,
    },
    ImmNum {
        num_i: Index,
        num: String,
        src: SourceRef,
    },
    ImmChar {
        char_i: Index,
        src: SourceRef,
    },
    ConstVar {
        name_i: Index,
        ty_i: Index,
        init_i: Index,
        src: SourceRef,
    },
    Dud {
        src: SourceRef,
    },
    Function {
        env_i: EnvironmentIndex,
        src: SourceRef,
    },
    TypeInfo {
        ty_i: Index,
    },
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Forge {
    code_info: Vec<ForgeInfo>,
    env_stack: Vec<Env>,
    strings: Vec<String>,
    types: Vec<TypeSignature>,
    code: CodeBundle,
    srcfile: SourceFile,
    fn_end_i: usize,
}

#[allow(dead_code)]
impl Forge {
    pub fn new(code: CodeBundle, srcfile: SourceFile) -> Self {
        let len = code.ins.len();
        let mut f = Forge {
            code_info: Vec::with_capacity(len),
            env_stack: Vec::new(),
            strings: Vec::new(),
            types: Vec::new(),
            code,
            srcfile,
            fn_end_i: 0,
        };
        f.enter_scope(); // global scope
        f
    }

    fn enter_scope(&mut self) {
        self.env_stack.push(Env::new());
    }

    fn exit_scope(&mut self) {
        self.env_stack.pop();
    }

    fn cur_scope(&mut self) -> &mut Env {
        self.env_stack.last_mut().unwrap()
    }

    // returns parent of current scope, if any
    fn parent_scope(&mut self) -> &mut Env {
        if self.env_stack.len() > 1 {
            let parent_i = self.env_stack.len() - 2;
            self.env_stack.get_mut(parent_i).unwrap()
        } else {
            panic!("no parent scope found");
        }
    }

    fn register_ty(&mut self, type_i: Index) {
        self.cur_scope().types.push(type_i);
    }

    fn register_name(&mut self, name_i: Index, type_i: Index) {
        self.cur_scope().names.push((name_i, type_i));
    }

    fn register_fn(&mut self, fn_name_i: Index, fn_type_i: Index) -> EnvironmentIndex {
        let index = EnvironmentIndex(self.cur_scope().functions.len());
        self.cur_scope().functions.push((fn_name_i, fn_type_i));
        index
    }

    fn intern_str(&mut self, n_str: String) -> Index {
        // check if string already exists
        for (i, s) in self.strings.iter().enumerate() {
            if s == &n_str {
                return Index {
                    tag: IndexTag::String,
                    index: i,
                };
            }
        }

        // if not, add it
        self.strings.push(n_str);
        Index {
            tag: IndexTag::String,
            index: self.strings.len() - 1,
        }
    }

    fn get_str(&self, str_i: &Index) -> &str {
        assert!(
            matches!(str_i.tag, IndexTag::String),
            "trying to get interned string with non-string index"
        );
        let s = self.strings.get(str_i.index).unwrap();
        s.as_str()
    }

    fn typecheck(&self, _a_ty_i: &Index, _b_ty_i: &Index) -> bool {
        let a_ty = self.code.get_type(_a_ty_i);
        let b_ty = self.code.get_type(_b_ty_i);

        self.accepts(&a_ty, &b_ty)
    }

    fn namecheck(&self, _a_name_i: &Index, b_name: &String) -> bool {
        let a_name = self.get_str(_a_name_i);

        a_name == b_name
    }

    fn name_exists(&self, name_s: &String) -> Option<Index> {
        // loop through all scopes, starting with current without popping it off the stack
        let mut len = self.env_stack.len();
        let mut cur_scope = self.env_stack.get(len - 1).unwrap();
        loop {
            for (n_i, ty_i) in cur_scope.names.iter() {
                if self.namecheck(&n_i, name_s) {
                    return Some(*ty_i);
                }
            }

            if len > 1 {
                len -= 1;
                cur_scope = self.env_stack.get(len - 1).unwrap();
            } else {
                break;
            }
        }
        None
    }

    fn verify_type(&mut self, ty: &TypeSignature) -> bool {
        match ty.tag {
            TSTag::ComptimeInt
            | TSTag::I8
            | TSTag::I16
            | TSTag::I32
            | TSTag::I64
            | TSTag::Isize
            | TSTag::U8
            | TSTag::U16
            | TSTag::U32
            | TSTag::U64
            | TSTag::Usize
            | TSTag::Bool
            | TSTag::Char
            | TSTag::Void
            | TSTag::Str
            | TSTag::Type
            | TSTag::BoolTy
            | TSTag::CharTy
            | TSTag::VoidTy
            | TSTag::StrTy
            | TSTag::I8Ty
            | TSTag::I16Ty
            | TSTag::I32Ty
            | TSTag::I64Ty
            | TSTag::IsizeTy
            | TSTag::U8Ty
            | TSTag::U16Ty
            | TSTag::U32Ty
            | TSTag::U64Ty
            | TSTag::UsizeTy
            | TSTag::TypeTy => true,

            TSTag::Function => {
                for ty in ty.indices.iter() {
                    let inner_ty = self.code.get_type(&ty);
                    if !self.verify_type(&inner_ty) {
                        return false;
                    }
                }
                true
            }
            TSTag::NameRef => {
                let name_i = ty.indices[0];
                let name_s = self.code.get_string(&name_i);
                let ty_i = self.name_exists(&name_s);
                if let Some(ty_i) = ty_i {
                    let ty = self.code.get_type(&ty_i);
                    if matches!(ty.tag, TSTag::Type) && ty.indices.is_empty() {
                        return self.verify_type(&ty);
                    }
                    false
                } else {
                    false
                }
            }
        }
    }

    fn insert_dud_info(&mut self, span: SourceRef) {
        let dud = ForgeInfo::Dud { src: span };
        self.code_info.push(dud);
    }

    fn is_valid_promotion(&self, ty: &TypeSignature, receiver: &TypeSignature, value: &str) -> bool {
        println!("trying to promote {} of type {:?} to type {:?}", value, ty.tag, receiver.tag);
        match (&ty.tag, &receiver.tag) {
            (TSTag::ComptimeInt, any) if any.is_literal_type_token() => match any {
                TSTag::ComptimeInt => true,
                TSTag::I8Ty => {
                    let val = value.parse::<i8>();
                    val.is_ok()
                }
                TSTag::I16Ty => {
                    let val = value.parse::<i16>();
                    val.is_ok()
                }
                TSTag::I32Ty => {
                    let val = value.parse::<i32>();
                    val.is_ok()
                }
                TSTag::I64Ty => {
                    let val = value.parse::<i64>();
                    val.is_ok()
                }
                TSTag::IsizeTy => {
                    let val = value.parse::<isize>();
                    val.is_ok()
                }
                TSTag::U8Ty => {
                    let val = value.parse::<u8>();
                    val.is_ok()
                }
                TSTag::U16Ty => {
                    let val = value.parse::<u16>();
                    val.is_ok()
                }
                TSTag::U32Ty => {
                    let val = value.parse::<u32>();
                    val.is_ok()
                }
                TSTag::U64Ty => {
                    let val = value.parse::<u64>();
                    val.is_ok()
                }
                TSTag::UsizeTy => {
                    let val = value.parse::<usize>();
                    val.is_ok()
                }
                TSTag::Function
                | TSTag::NameRef
                | _ => false,
            },
            _ => false,
        }
    }

    // implements simple type checking
    fn accepts(&self, receiver: &TypeSignature, val_ty: &TypeSignature) -> bool {
        // println!("accepts: {:?} <= {:?}", receiver.tag, val_ty.tag);
        match (&receiver.tag, &val_ty.tag) {
            (TSTag::BoolTy, TSTag::Bool)
            | (TSTag::TypeTy, TSTag::Type)
            | (TSTag::I8Ty, TSTag::I8)
            | (TSTag::I16Ty, TSTag::I16)
            | (TSTag::I32Ty, TSTag::I32)
            | (TSTag::I64Ty, TSTag::I64)
            | (TSTag::IsizeTy, TSTag::Isize)
            | (TSTag::U8Ty, TSTag::U8)
            | (TSTag::U16Ty, TSTag::U16)
            | (TSTag::U32Ty, TSTag::U32)
            | (TSTag::U64Ty, TSTag::U64)
            | (TSTag::UsizeTy, TSTag::Usize) 
            | (TSTag::CharTy, TSTag::Char)
            | (TSTag::VoidTy, TSTag::Void)
            | (TSTag::StrTy, TSTag::Str) => true,
            (TSTag::TypeTy, v_tag) if v_tag.is_literal_type_token() => true,
            _ => false
        }
    }

    // this will infer the type of a node (or at least try to) based on ForgeInfo it
    // has collected previously
    fn infer_type(&mut self, src_node: &Index, receiver: Option<&TypeSignature>) -> TypeSignature {
        assert!(
            matches!(src_node.tag, IndexTag::Code),
            "expected code index as input to infer_type"
        );
        let code_info = &self.code_info.get(src_node.index);
        if code_info.is_none() {
            panic!("{} is not a code info node.", src_node.index);
        }
        let code_info = code_info.unwrap();
        match code_info {
            ForgeInfo::ImmChar { .. } => {
                let char_ins = self.code.get_ins(*src_node);
                let src = char_ins.src;
                TypeSignature {
                    tag: TSTag::Char,
                    src: src.clone(),
                    indices: vec![],
                }
            }
            ForgeInfo::ImmStr { .. } => {
                let src_ins = self.code.get_ins(*src_node);
                let src = src_ins.src;
                TypeSignature {
                    tag: TSTag::Str,
                    src: src.clone(),
                    indices: vec![], // no indices for str
                }
            }
            ForgeInfo::ImmNum { num, .. } => {
                let src_ins = self.code.get_ins(*src_node);
                let src = src_ins.src;

                if let Some(ty) = receiver {
                    // we need to try to promote this untyped integer to the type
                    // the receiver has
                    let num_ty = TypeSignature {
                        tag: TSTag::ComptimeInt,
                        src: src.clone(),
                        indices: vec![],
                    };

                    if !self.is_valid_promotion(&num_ty, ty, &num) {
                        panic!(
                            "int cannot be promoted to {}",
                            self.code.type_as_strl(ty)
                        )
                    }
                    TypeSignature {
                        tag: ty.tag.accepted_numerical_type(),
                        src: src.clone(),
                        indices: vec![],
                    }
                } else {
                    let num_ty = TypeSignature {
                        tag: TSTag::ComptimeInt,
                        src: src.clone(),
                        indices: vec![], // no indices for str
                    };
                    
                    // try to convert the comptime int to an i32
                    let target_attempt = TypeSignature {
                        tag: TSTag::IsizeTy,
                        src: src.clone(),
                        indices: vec![],
                    };

                    if !self.is_valid_promotion(&num_ty, &target_attempt, &num) {
                        panic!("comptime_int cannot be promoted to isize");
                    }
                    TypeSignature {
                        tag: target_attempt.tag.accepted_numerical_type(),
                        src: src.clone(),
                        indices: vec![],
                    }
                }
            }
            ForgeInfo::TypeInfo { ty_i } => self.code.get_type(ty_i),
            ForgeInfo::ConstVar { .. } => {
                unreachable!("inferring type of constant variable should not be possible.")
            }
            ForgeInfo::Dud { .. } => {
                unreachable!("inferring type of dud should not be possible.")
            }
            ForgeInfo::Function { .. } => {
                unreachable!("inferring type of function should not be possible.")
            }
        }
    }

    // it will evaluate bcode instructions and collect information
    // about the code, using it to analyze and partially evaluate the
    // code to generate a type checked version or just C++
    pub fn eval(&mut self) {
        let instructions = self.code.ins.clone();
        instructions.iter().enumerate().for_each(|(_, ins)| {
            // println!("evaluating instruction: {:#?}", ins.tag);
            match ins.tag {
                CodeTag::SrcComment => {
                    // do nothing
                    self.insert_dud_info(ins.src.clone());
                }
                CodeTag::LIStr => {
                    // store info about an immediate string
                    let str_i = ins.indices[0];
                    let str_a = self.code.get_string(&str_i);
                    let len = str_a.len();
                    let n_str_i = self.intern_str(str_a);
                    self.code_info.push(ForgeInfo::ImmStr {
                        str_i: n_str_i,
                        len,
                        src: ins.src.clone(),
                    });
                }
                CodeTag::LINum => {
                    // store info about an immediate number
                    let num_i = ins.indices[0];
                    let num_s = self.code.get_string(&num_i);
                    let n_num_i = self.intern_str(num_s.clone());
                    self.code_info.push(ForgeInfo::ImmNum {
                        num_i: n_num_i,
                        num: num_s,
                        src: ins.src.clone(),
                    })
                }
                CodeTag::LIChar => {
                    // store info about an immediate char
                    let char_i = ins.indices[0];
                    let char_s = self.code.get_string(&char_i);
                    let n_char_i = self.intern_str(char_s);
                    self.code_info.push(ForgeInfo::ImmChar {
                        char_i: n_char_i,
                        src: ins.src.clone(),
                    })
                }
                CodeTag::NewConstant => {
                    // NewConstant "name" Type:19?, Code:20
                    let name_si = ins.indices[0];
                    let (ty_i, val_i) = if ins.indices.len() > 2 {
                        (Some(ins.indices[1]), ins.indices[2])
                    } else {
                        (None, ins.indices[1])
                    };

                    let name_s = self.code.get_string(&name_si);
                    let name_i = self.intern_str(name_s.clone());

                    // if it has some type, we want to make sure it's type is valid
                    // and then check it against the type of the init value
                    if let Some(ty_i) = ty_i {
                        let mut ty = self.code.get_type(&ty_i);
                        // unalias type if it is a name ref
                        if matches!(ty.tag, TSTag::NameRef) {
                            let ty_name_i = ty.indices[0];
                            let ty_name_s = self.code.get_string(&ty_name_i);
                            let ty_i = self.name_exists(&ty_name_s);
                            if let Some(ty_i) = ty_i {
                                ty = self.code.get_type(&ty_i);
                            } else {
                                panic!("invalid type given to constant");
                            }
                        }
                        let val_ty = self.infer_type(&val_i, Some(&ty));

                        // verify both types exist and are valid types
                        if !self.verify_type(&ty) {
                            panic!("invalid type given to constant");
                        }

                        if !self.verify_type(&val_ty) {
                            panic!("invalid type from constant init value");
                        }
                        
                        // let ty_s = self.code.type_as_str(ty_i);
                        // let val_ty_s = self.code.type_as_strl(&val_ty);
                        // println!("constant type: {ty_s}");
                        // println!("constant init type: {val_ty_s}");

                        // ensure they are the same type
                        if !self.accepts(&ty, &val_ty) {
                            let line = ins.src.start_line + 1;
                            let col = ins.src.start_col + 1;
                            let ty_s = self.code.type_as_str(ty_i);
                            let val_ty_s = self.code.type_as_strl(&val_ty);
                            panic!("type mismatch: {line}:{col} {ty_s} != {val_ty_s}");
                        }

                        // add the name to current scope
                        let val_ty_i = self.code.add_type(val_ty);
                        self.register_name(name_i, val_ty_i.clone());

                        // generate typed code

                        // generate code info for this node
                        self.code_info.push(ForgeInfo::ConstVar {
                            name_i,
                            ty_i: val_ty_i,
                            init_i: val_i,
                            src: ins.src.clone(),
                        });
                    } else {
                        // if it doesn't have a type, we want to infer it
                        let val_ty = self.infer_type(&val_i, None);

                        let val_ty_i = self.code.add_type(val_ty);

                        // add the name to current scope
                        self.register_name(name_i, val_ty_i);

                        // generate typed code

                        // generate code info for this node
                        self.code_info.push(ForgeInfo::ConstVar {
                            name_i,
                            ty_i: val_ty_i,
                            init_i: val_i,
                            src: ins.src.clone(),
                        });
                    }
                }
                CodeTag::NewFunction => {
                    // NewFunction "name" Type:2 Code:30
                    let fn_name_si = ins.indices[0];
                    let fn_name_s = self.code.get_string(&fn_name_si);
                    let fn_name_i = self.intern_str(fn_name_s);

                    // get the type of the function
                    let fn_ty_i = ins.indices[1];

                    // get the end of the function (since the body is inlined)
                    let fn_end_i = ins.indices[2].index;
                    self.fn_end_i = fn_end_i;

                    // add the function to current scope with the type
                    let env_i = self.register_fn(fn_name_i, fn_ty_i);

                    // generate typed code

                    // generate code info for this node
                    // we want to generate code info that tracks the Index to the function
                    // in Env.functions
                    self.code_info.push(ForgeInfo::Function {
                        env_i,
                        src: ins.src.clone(),
                    });
                }
                CodeTag::Param => {
                    // Param "name" Type:19
                    let name_si = ins.indices[0];
                    let name_s = self.code.get_string(&name_si);
                    let name_i = self.intern_str(name_s);

                    // get the type of the parameter
                    let ty_i = ins.indices[1];
                    let ty = self.code.get_type(&ty_i);
                    if ty.tag.is_literal_type_token() {
                        let n_ty = TypeSignature {
                            tag: ty.tag.accepted_numerical_type(),
                            src: ty.src.clone(),
                            indices: vec![],
                        };
                    }

                    // add the parameter to current scope with the type
                    self.register_name(name_i, ty_i);

                    // generate typed code

                    self.insert_dud_info(ins.src.clone());
                }
                CodeTag::EnterScope => {
                    // EnterScope
                    // println!("entering scope");
                    self.enter_scope();
                    self.insert_dud_info(ins.src.clone());
                }
                CodeTag::ExitScope => {
                    // ExitScope
                    // println!("exiting scope");
                    self.exit_scope();
                    self.insert_dud_info(ins.src.clone());

                    // instead of inserting a dud, we can generate code info
                    // about the scope we just exited
                    // - number of types declared
                    // - number of functions declared
                    // - number of variables declared
                    // - number of constants declared
                    // - number of parameters declared (for functions)
                }
                CodeTag::Return => {
                    // Return Code:30
                    // println!("returning");
                    self.insert_dud_info(ins.src.clone());
                }
                CodeTag::ExpectTypeIs => {
                    // ExpectTypeIs Type:19 Code:30
                    let ty_i = ins.indices[0];
                    let ty = self.code.get_type(&ty_i);
                    let code_i = ins.indices[1];
                    let code_ty = self.infer_type(&code_i, Some(&ty));

                    let ty_s = self.code.type_as_str(ty_i);
                    let val_ty_s = self.code.type_as_strl(&code_ty);
                    println!("target type: {ty_s} , {:#?}", ty.tag);
                    println!("value  type: {val_ty_s} , {:#?}", code_ty.tag);

                    if !self.accepts(&ty, &code_ty) {
                        let line = ins.src.start_line + 1;
                        let col = ins.src.start_col + 1;
                        let ty_s = self.code.type_as_str(ty_i);
                        let code_ty_s = self.code.type_as_strl(&code_ty);
                        panic!("type mismatch: {line}:{col} {ty_s} != {code_ty_s}");
                    }
                    let e_ty = TypeSignature {
                        tag: code_ty.tag,
                        src: ins.src.clone(),
                        indices: vec![],
                    };
                    let e_ty_i = self.code.add_type(e_ty);
                    self.code_info.push(ForgeInfo::TypeInfo { ty_i: e_ty_i });
                }
                CodeTag::LoadTrue => {
                    // LoadTrue
                    // println!("loading true");
                    let ty = TypeSignature {
                        tag: TSTag::Bool,
                        src: ins.src.clone(),
                        indices: vec![],
                    };
                    let ty_i = self.code.add_type(ty);

                    // generate typed code

                    // generate code info for this node
                    self.code_info.push(ForgeInfo::TypeInfo { ty_i });
                }
                CodeTag::LoadFalse => {
                    // LoadFalse
                    // println!("loading false");
                    let ty = TypeSignature {
                        tag: TSTag::Bool,
                        src: ins.src.clone(),
                        indices: vec![],
                    };
                    let ty_i = self.code.add_type(ty);

                    // generate typed code

                    // generate code info for this node
                    self.code_info.push(ForgeInfo::TypeInfo { ty_i });
                }
                CodeTag::MakePublic => {
                    let code_i = ins.indices[0].index;
                    let code_info = self.code_info.get(code_i).unwrap();
                    match code_info {
                        ForgeInfo::Function { .. } => {
                            // MakePublic Code:30
                            // println!("making public");
                            // TODO: when we have the notion of public vs private in
                            // the env, we can use that to make the function public to
                            // importing modules
                            self.insert_dud_info(ins.src.clone());
                        }
                        _ => panic!("not implemented: {:#?}", ins),
                    }
                }
                CodeTag::NameRef => {
                    // NameRef "name"
                    let name_si = ins.indices[0];
                    let name_s = self.code.get_string(&name_si);

                    // ensure the name exists
                    let maybe_ty_i = self.name_exists(&name_s);
                    if maybe_ty_i.is_none() {
                        panic!("name does not exist");
                    }

                    // generate typed code

                    // generate code info for this node
                    let ty_i = maybe_ty_i.unwrap();
                    println!("name ref: {name_s} is typed {:?}", self.code.get_type(&ty_i).tag);
                    self.code_info.push(ForgeInfo::TypeInfo { ty_i });
                }
                CodeTag::TypeRef => {
                    // TypeRef Type:19
                    let ty_i = ins.indices[0];
                    let ty = self.code.get_type(&ty_i);

                    // println!("type ref: {ty:#?}");

                    if !self.verify_type(&ty) {
                        panic!("invalid type referenced");
                    }

                    // generate typed code

                    // generate code info for this node
                    self.code_info.push(ForgeInfo::TypeInfo { ty_i });
                }
                _ => panic!("not implemented: {:#?}", ins),
            }
        });
        // println!("completed eval");
    }
}
