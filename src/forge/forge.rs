use super::env::{Env, EnvironmentIndex};
use crate::frontend::{
    bcode::{CodeBundle, CodeTag, Index, IndexTag},
    source::{SourceFile, SourceRef},
    types::{TypeSignatureTag, TypeSignature},
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
        todo!()
    }

    fn insert_dud_info(&mut self, span: SourceRef) {
        let dud = ForgeInfo::Dud { src: span };
        self.code_info.push(dud);
    }

    fn is_valid_promotion(&self, ty: &TypeSignature, receiver: &TypeSignature, value: &str) -> bool {
        todo!()
    }

    // implements simple type checking
    fn accepts(&self, receiver: &TypeSignature, val_ty: &TypeSignature) -> bool {
        todo!()
    }

    // this will infer the type of a node (or at least try to) based on ForgeInfo it
    // has collected previously
    fn infer_type(&mut self, src_node: &Index, receiver: Option<&TypeSignature>) -> TypeSignature {
        assert!(
            matches!(src_node.tag, IndexTag::Code),
            "expected code index as input to infer_type"
        );
        todo!()
    }

    // it will evaluate bcode instructions and collect information
    // about the code, using it to analyze and partially evaluate the
    // code to generate a type checked version or just C++
    pub fn eval(&mut self) {
        todo!()
    }
}
