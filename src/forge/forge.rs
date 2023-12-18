use super::env::Env;
use crate::frontend::{
    bcode::{CTag, CodeBundle, ITag, Index},
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
    ConstVar {
        name_i: Index,
        ty_i: Index,
        init_i: Index,
        src: SourceRef,
    },
    Dud {
        src: SourceRef,
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

    fn register_fn(&mut self, fn_name_i: Index, fn_type_i: Index) {
        self.cur_scope().functions.push((fn_name_i, fn_type_i));
    }

    fn intern_str(&mut self, n_str: String) -> Index {
        // check if string already exists
        for (i, s) in self.strings.iter().enumerate() {
            if s == &n_str {
                return Index {
                    tag: ITag::String,
                    index: i,
                };
            }
        }

        // if not, add it
        self.strings.push(n_str);
        Index {
            tag: ITag::String,
            index: self.strings.len() - 1,
        }
    }

    fn typecheck(&self, _a_ty_i: &Index, _b_ty_i: &Index) -> bool {
        let a_ty = self.code.get_type(_a_ty_i);
        let b_ty = self.code.get_type(_b_ty_i);

        self.code.eq_types(&a_ty, &b_ty)
    }

    fn namecheck(&self, _a_name_i: &Index, _b_name_i: &Index) -> bool {
        let a_name = self.code.get_string(_a_name_i);
        let b_name = self.code.get_string(_b_name_i);

        a_name == b_name
    }

    fn name_exists(&mut self, name_i: &Index) -> bool {
        // loop through all scopes, starting with current without popping it off the stack
        let mut len = self.env_stack.len();
        let mut cur_scope = self.env_stack.get(len - 1).unwrap();
        loop {
            for (n_i, _) in cur_scope.names.iter() {
                if self.namecheck(&n_i, &name_i) {
                    return true;
                }
            }

            if len > 1 {
                len -= 1;
                cur_scope = self.env_stack.get(len - 1).unwrap();
            } else {
                break;
            }
        }
        false
    }

    fn verify_type(&self, ty: &TypeSignature) -> bool {
        match ty.tag {
            TSTag::I8
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
            | TSTag::Type => true,
            TSTag::SizedArray => {
                let inner_ty_i = ty.indices[1];
                let inner_ty = self.code.get_type(&inner_ty_i);
                self.verify_type(&inner_ty)
            }
            TSTag::Array => {
                let inner_ty_i = ty.indices[0];
                let inner_ty = self.code.get_type(&inner_ty_i);
                self.verify_type(&inner_ty)
            }
            TSTag::Function => {
                for ty in ty.indices.iter() {
                    let inner_ty = self.code.get_type(&ty);
                    if !self.verify_type(&inner_ty) {
                        return false;
                    }
                }
                true
            }
            TSTag::NameRef => todo!(),
        }
    }

    // this will infer the type of a node (or at least try to) based on ForgeInfo it
    // has collected previously
    fn infer_type(&self, src_node: &Index) -> TypeSignature {
        assert!(
            matches!(src_node.tag, ITag::Code),
            "expected code index as input to infer_type"
        );
        let code_info = &self.code_info[src_node.index];
        match code_info {
            ForgeInfo::ImmStr { .. } => {
                let src_ins = self.code.get_ins(*src_node);
                let src = src_ins.src;
                TypeSignature {
                    tag: TSTag::Str,
                    src,
                    indices: vec![], // no indices for str
                }
            }
            ForgeInfo::ConstVar { .. } => {
                unreachable!("inferring type of constant variable should not be possible.")
            }
        }
    }

    // it will evaluate bcode instructions and collect information
    // about the code, using it to analyze and partially evaluate the
    // code to generate a type checked version or just C++
    pub fn eval(&mut self) {
        let instructions = self.code.ins.clone();
        for (_, ins) in instructions.iter().enumerate() {
            // println!("evaluating instruction: {:#?}", ins.tag);
            match ins.tag {
                CTag::SrcComment => {
                    // SrcComment "comment" Code:0 (TODO: need to change opcode to this)
                    // do nothing
                }
                CTag::LIStr => {
                    // store info about an immediate string
                    let str_i = ins.indices[0];
                    let str_a = self.code.get_string(&str_i);
                    let len = str_a.len();
                    let n_str_i = self.intern_str(str_a);
                    // insert into the code_info vec at the same index as the instruction
                    self.code_info.push(ForgeInfo::ImmStr {
                        str_i: n_str_i,
                        len,
                        src: ins.src.clone(),
                    });
                }
                CTag::NewConstant => {
                    // NewConstant "name" Type:19?, Code:20
                    let name_si = ins.indices[0];
                    let (ty_i, val_i) = if ins.indices.len() > 2 {
                        (Some(ins.indices[1]), ins.indices[2])
                    } else {
                        (None, ins.indices[1])
                    };

                    let name_s = self.code.get_string(&name_si);
                    let name_i = self.intern_str(name_s);

                    // if it has some type, we want to make sure it's type is valid
                    // and then check it against the type of the init value
                    if let Some(ty_i) = ty_i {
                        let ty = self.code.get_type(&ty_i);
                        let val_ty = self.infer_type(&val_i);

                        // verify both types exist and are valid types
                        if !self.verify_type(&ty) {
                            panic!("invalid type given to constant");
                        }

                        if !self.verify_type(&val_ty) {
                            panic!("invalid type from constant init value");
                        }

                        // ensure they are the same type
                        if !self.code.eq_types(&ty, &val_ty) {
                            panic!("type mismatch");
                        }

                        // add the name to current scope
                        self.register_name(name_i, ty_i);

                        // generate typed code

                        // generate code info for this node
                        self.code_info.push(ForgeInfo::ConstVar {
                            name_i,
                            ty_i,
                            init_i: val_i,
                            src: ins.src.clone(),
                        });
                    } else {
                        // if it doesn't have a type, we want to infer it
                        let val_ty = self.infer_type(&val_i);
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
                CTag::NewFunction => {
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
                    self.register_fn(fn_name_i, fn_ty_i);
                }
                CTag::Param => {
                    // Param "name" Type:19
                    let name_si = ins.indices[0];
                    let name_s = self.code.get_string(&name_si);
                    let name_i = self.intern_str(name_s);

                    // get the type of the parameter
                    let ty_i = ins.indices[1];

                    // add the parameter to current scope with the type
                    self.register_name(name_i, ty_i);
                }
                CTag::EnterScope => {
                    // EnterScope
                    // println!("entering scope");
                    self.enter_scope();
                }
                CTag::ExitScope => {
                    // ExitScope
                    // println!("exiting scope");
                    self.exit_scope();
                }
                CTag::MakePublic => {
                    // todo
                }
                CTag::NameRef => {
                    // NameRef "name"
                    let name_si = ins.indices[0];
                    let name_s = self.code.get_string(&name_si);
                    let name_i = self.intern_str(name_s);

                    // ensure the name exists
                    if !self.name_exists(&name_i) {
                        panic!("name does not exist");
                    }

                    // get the type of the name
                    let ty_i = self.get_name_type(&name_i);
                }
                _ => panic!("not implemented: {:#?}", ins),
            }
        }
        // println!("completed eval");
    }
}
