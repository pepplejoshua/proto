use super::env::Env;
use crate::frontend::{
    bcode::{CTag, CodeBundle, ITag, Index},
    types::TypeSignature,
};

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum ForgeInfo {
    ImmStr { str_i: Index, len: usize },
    ConstVar { name_i: Index, ty_i: Index },
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Forge {
    code_info: Vec<ForgeInfo>,
    env_stack: Vec<Env>,
    strings: Vec<String>,
    types: Vec<TypeSignature>,
}

#[allow(dead_code)]
impl Forge {
    pub fn new(len: usize) -> Self {
        Forge {
            code_info: Vec::with_capacity(len),
            env_stack: Vec::new(),
            strings: Vec::new(),
            types: Vec::new(),
        }
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

    fn register_name(&mut self, name_i: Index) {
        self.cur_scope().names.push(name_i);
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
        self.cur_scope().names.push(name_i);
        self.cur_scope().types.push(type_i);
    }

    fn typecheck(&mut self, _code: &mut CodeBundle, _a_ty_i: &Index, _b_ty_i: &Index) -> bool {
        let a_ty = _code.get_type(_a_ty_i);
        let b_ty = _code.get_type(_b_ty_i);

        _code.eq_types(&a_ty, &b_ty)
    }

    fn namecheck(&mut self, _code: &mut CodeBundle, _a_name_i: &Index, _b_name_i: &Index) -> bool {
        let a_name = _code.get_string(_a_name_i);
        let b_name = _code.get_string(_b_name_i);

        a_name == b_name
    }

    // this will infer the type of a node (or at least try to) based on ForgeInfo it
    // has collected previously
    fn infer_type(&mut self, src_node: &Index) -> TypeSignature {}

    // it will evaluate bcode instructions and collect information
    // about the code, using it to analyze and partially evaluate the
    // code to generate a type checked version or just C++
    pub fn eval(&mut self, _code: &mut CodeBundle) {
        for ins in &_code.ins {
            match ins.tag {
                CTag::LIStr => {
                    // store info about an immediate string
                    let str_i = ins.indices[0];
                    let str_a = _code.get_string(&str_i);
                    let len = str_a.len();
                    let n_str_i = self.intern_str(str_a);
                    self.code_info.push(ForgeInfo::ImmStr {
                        str_i: n_str_i,
                        len,
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

                    let name_s = _code.get_string(&name_si);
                    let name_i = self.intern_str(name_s);

                    // if it has some type, we want to make sure it's type is valid
                    // and then check it against the type of the init value
                    if let Some(ty_i) = ty_i {
                        let ty = _code.get_type(&ty_i);
                        let val_ty = self.infer_type(&val_i);

                        if !_code.eq_types(&ty, &val_ty) {
                            panic!("type mismatch");
                        }
                    } else {
                        // if it doesn't have a type, we want to infer it
                    }
                }
                _ => panic!("not implemented: {:?}", ins),
            }
        }
    }
}
