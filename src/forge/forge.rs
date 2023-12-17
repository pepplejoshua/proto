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

    fn register_name_ty(&mut self, name_i: Index, type_i: Index) {
        panic!("register_name_ty is incomplete");
        // self.cur_scope().names.push(name_i);
        // self.cur_scope().types.push(type_i);
    }

    fn typecheck(&mut self, _a_ty_i: &Index, _b_ty_i: &Index) -> bool {
        let a_ty = self.code.get_type(_a_ty_i);
        let b_ty = self.code.get_type(_b_ty_i);

        self.code.eq_types(&a_ty, &b_ty)
    }

    fn namecheck(&mut self, _a_name_i: &Index, _b_name_i: &Index) -> bool {
        let a_name = self.code.get_string(_a_name_i);
        let b_name = self.code.get_string(_b_name_i);

        a_name == b_name
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
            println!("evaluating instruction: {:#?}", ins.tag);
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
                _ => panic!("not implemented: {:#?}", ins),
            }
        }
        println!("completed eval");
    }
}
