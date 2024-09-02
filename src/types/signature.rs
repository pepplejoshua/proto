#![allow(dead_code)]
#![allow(unused_variables)]

use crate::parser::ast::Expr;
use crate::source::source::{SourceFile, SourceRef};
use std::{
    borrow::BorrowMut,
    collections::{BTreeMap, HashMap},
    rc::Rc,
    sync::atomic::AtomicUsize,
};

// the id for a type
pub type TypeId = usize;

// this is the concrete type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeDef {
    Type,
    Signed {
        size: u8,
        is_int: bool,
    },
    Unsigned {
        size: u8,
        is_uint: bool,
    },
    Float {
        size: u8,
    },
    Str,
    Char,
    Void,
    Bool,
    Func {
        params: Vec<TypeId>,
        ret: TypeId,
    },
    Method {
        params: Vec<TypeId>,
        ret: TypeId,
        is_const: bool,
    },
    StaticArray {
        sub_ty: TypeId,
        size: usize,
    },
    Slice {
        sub_ty: TypeId,
    },
    Optional {
        sub_ty: TypeId,
    },
    Struct {
        name: String,
        fields: BTreeMap<String, TypeId>,
        funcs: BTreeMap<String, TypeId>,
    },
    Pointer {
        sub_ty: TypeId,
    },
    ErrorType,
}

// an instance of a type definition
pub struct TypeInst {
    pub id: TypeId,
    pub scope_id: usize,
    pub loc: Rc<SourceRef>,
}

pub fn make_inst(id: TypeId, scope_id: usize, loc: Rc<SourceRef>) -> TypeInst {
    TypeInst { id, scope_id, loc }
}

pub enum ScopeType {
    Persistent,
    Ephemeral,
}

pub struct TypeScope {
    scope_ty: ScopeType,
    parent: Option<Rc<TypeScope>>,
    types: HashMap<TypeDef, TypeId>,
    definitions: Vec<(TypeDef, Option<Rc<SourceRef>>)>,
    next_type_id: AtomicUsize,
    named_definitions: HashMap<String, TypeId>,
    scope_id: usize,
}

impl TypeScope {
    pub fn new(scope_id: usize, scope_ty: ScopeType) -> TypeScope {
        TypeScope {
            scope_ty,
            parent: None,
            types: HashMap::new(),
            definitions: vec![],
            next_type_id: AtomicUsize::new(0),
            named_definitions: HashMap::new(),
            scope_id,
        }
    }

    pub fn add_new_type_def(
        &mut self,
        ty_def: TypeDef,
        def_location: Option<Rc<SourceRef>>,
    ) -> (TypeId, usize) {
        if let Some(&id) = self.types.get(&ty_def) {
            (id, self.scope_id)
        } else {
            let id = self
                .next_type_id
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            self.types.insert(ty_def.clone(), id);
            self.definitions.push((ty_def.clone(), def_location));
            (id, self.scope_id)
        }
    }

    pub fn add_new_named_type_def(&mut self, ty_def: TypeDef, name: &str) -> (TypeId, usize) {
        if let Some(&type_id) = self.named_definitions.get(name) {
            let (ex_type_def, _) = &self.definitions[type_id];
            if &ty_def != ex_type_def {}

            todo!()
        } else {
            todo!()
        }
    }

    pub fn type_id_exists(&self, id: TypeId) -> bool {
        self.definitions.get(id).is_some()
    }

    pub fn get_type_def_using_id(&self, id: TypeId) -> Option<&(TypeDef, Option<Rc<SourceRef>>)> {
        self.definitions.get(id)
    }

    pub fn name_exists_shallow(&self, name: &str) -> Option<TypeId> {
        if let Some(id) = self.named_definitions.get(name) {
            return Some(*id);
        }
        None
    }

    pub fn name_exists(&self, name: &str) -> Option<TypeId> {
        if let Some(id) = self.named_definitions.get(name) {
            return Some(*id);
        }

        let mut maybe_scope = &self.parent;

        while let Some(scope) = maybe_scope {
            if let Some(id) = scope.named_definitions.get(name) {
                return Some(*id);
            } else {
                maybe_scope = &scope.parent;
            }
        }
        None
    }
}

pub struct TypeTable {
    cur_scope_id: usize,
    next_scope_id: AtomicUsize,
    all_scopes: Vec<TypeScope>,
}

impl TypeTable {
    pub fn new() -> TypeTable {
        return TypeTable {
            cur_scope_id: 0,
            next_scope_id: AtomicUsize::new(1),
            all_scopes: vec![TypeScope::new(0, ScopeType::Persistent)],
        };
    }

    pub fn add_new_type_def(
        &mut self,
        ty_def: TypeDef,
        def_location: Option<Rc<SourceRef>>,
    ) -> (TypeId, usize) {
        self.all_scopes[self.cur_scope_id].add_new_type_def(ty_def, def_location)
    }

    pub fn add_new_named_type_def(&mut self, ty_def: TypeDef, name: &str) -> (TypeId, usize) {
        self.all_scopes[self.cur_scope_id].add_new_named_type_def(ty_def, name)
    }

    pub fn type_id_exists(&self, id: TypeId) -> bool {
        self.all_scopes[self.cur_scope_id].type_id_exists(id)
    }

    pub fn get_type_def_using_id(&self, id: TypeId) -> Option<&(TypeDef, Option<Rc<SourceRef>>)> {
        self.all_scopes[self.cur_scope_id].get_type_def_using_id(id)
    }

    pub fn name_exists_shallow(&self, name: &str) -> Option<TypeId> {
        self.all_scopes[self.cur_scope_id].name_exists_shallow(name)
    }

    pub fn name_exists(&self, name: &str) -> Option<TypeId> {
        self.all_scopes[self.cur_scope_id].name_exists(name)
    }
}

#[derive(Debug, Clone)]
pub enum Ty {
    Type {
        loc: Rc<SourceRef>,
    },
    Signed {
        size: u8,
        is_int: bool,
        loc: Rc<SourceRef>,
    },
    Unsigned {
        size: u8,
        is_uint: bool,
        loc: Rc<SourceRef>,
    },
    Float {
        size: u8,
        loc: Rc<SourceRef>,
    },
    Str {
        loc: Rc<SourceRef>,
    },
    Char {
        loc: Rc<SourceRef>,
    },
    Void {
        loc: Rc<SourceRef>,
    },
    Bool {
        loc: Rc<SourceRef>,
    },
    Func {
        params: Vec<Rc<Ty>>,
        ret: Rc<Ty>,
        loc: Rc<SourceRef>,
        is_const: bool,
    },
    StaticArray {
        sub_ty: Rc<Ty>,
        size: Expr,
        loc: Rc<SourceRef>,
    },
    Slice {
        sub_ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    Optional {
        sub_ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    Struct {
        fields: HashMap<String, Rc<Ty>>,
        static_funcs: HashMap<String, Rc<Ty>>,
        methods: HashMap<String, Rc<Ty>>,
        loc: Rc<SourceRef>,
    },
    NamedType {
        loc: Rc<SourceRef>,
    },
    AccessMemberType {
        target: Rc<Ty>,
        mem: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    TypeFunc {
        func: Rc<Ty>,
        args: Vec<Expr>,
        loc: Rc<SourceRef>,
    },
    Pointer {
        sub_ty: Rc<Ty>,
        loc: Rc<SourceRef>,
    },
    Tuple {
        sub_tys: Vec<Rc<Ty>>,
        loc: Rc<SourceRef>,
    },
    ErrorType {
        loc: Rc<SourceRef>,
    },
}

impl Ty {
    pub fn is_float_ty(&self) -> bool {
        match self {
            Ty::Float { .. } => true,
            _ => false,
        }
    }

    pub fn is_num_ty(&self) -> bool {
        match self {
            Ty::Signed { .. } | Ty::Unsigned { .. } => true,
            _ => false,
        }
    }

    pub fn is_void(&self) -> bool {
        match self {
            Ty::Void { .. } => true,
            _ => false,
        }
    }

    pub fn is_signed_ty(&self) -> bool {
        match self {
            Ty::Signed { .. } => true,
            _ => false,
        }
    }

    pub fn is_unsigned_ty(&self) -> bool {
        match self {
            Ty::Unsigned { .. } => true,
            _ => false,
        }
    }

    pub fn is_error_ty(&self) -> bool {
        match self {
            Ty::ErrorType { .. } => true,
            _ => false,
        }
    }

    pub fn is_indexable(&self) -> bool {
        match self {
            Ty::Str { .. } | Ty::StaticArray { .. } | Ty::Slice { .. } => true,
            _ => false,
        }
    }

    pub fn is_sliceable(&self) -> bool {
        match self {
            Ty::StaticArray { .. } | Ty::Slice { .. } => true,
            _ => false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        match self {
            Ty::Pointer { .. } => true,
            _ => false,
        }
    }

    pub fn get_loc(&self) -> Rc<SourceRef> {
        match self {
            Ty::Type { loc }
            | Ty::Signed { loc, .. }
            | Ty::Unsigned { loc, .. }
            | Ty::Str { loc, .. }
            | Ty::Char { loc }
            | Ty::Void { loc }
            | Ty::Bool { loc }
            | Ty::Func { loc, .. }
            | Ty::AccessMemberType { loc, .. }
            | Ty::TypeFunc { loc, .. }
            | Ty::StaticArray { loc, .. }
            | Ty::Slice { loc, .. }
            | Ty::Optional { loc, .. }
            | Ty::Struct { loc, .. }
            | Ty::NamedType { loc, .. }
            | Ty::Pointer { loc, .. }
            | Ty::Float { loc, .. }
            | Ty::Tuple { loc, .. }
            | Ty::ErrorType { loc } => loc.clone(),
        }
    }

    pub fn set_loc(&mut self, n_loc: Rc<SourceRef>) {
        match self {
            Ty::Type { loc }
            | Ty::Signed { loc, .. }
            | Ty::Unsigned { loc, .. }
            | Ty::Str { loc, .. }
            | Ty::Char { loc }
            | Ty::Void { loc }
            | Ty::Bool { loc }
            | Ty::Func { loc, .. }
            | Ty::AccessMemberType { loc, .. }
            | Ty::TypeFunc { loc, .. }
            | Ty::StaticArray { loc, .. }
            | Ty::Slice { loc, .. }
            | Ty::Struct { loc, .. }
            | Ty::NamedType { loc, .. }
            | Ty::Optional { loc, .. }
            | Ty::Pointer { loc, .. }
            | Ty::Float { loc, .. }
            | Ty::Tuple { loc, .. }
            | Ty::ErrorType { loc } => {
                let b_loc = loc.borrow_mut();
                *b_loc = n_loc;
            }
        }
    }

    pub fn as_str(&self, src: &SourceFile) -> String {
        match self {
            Ty::Type { .. } => "type".to_string(),
            Ty::Signed { size, is_int, .. } => {
                if *is_int {
                    "int".into()
                } else {
                    format!("i{size}")
                }
            }
            Ty::Unsigned { size, is_uint, .. } => {
                if *is_uint {
                    "uint".into()
                } else {
                    format!("u{size}")
                }
            }
            Ty::Float { size, loc } => {
                format!("f{size}")
            }
            Ty::Str { .. } => "str".into(),
            Ty::Char { .. } => "char".into(),
            Ty::Void { .. } => "void".into(),
            Ty::Bool { .. } => "bool".into(),
            Ty::AccessMemberType { target, mem, .. } => {
                format!("{}.{}", target.as_str(src), mem.as_str(src))
            }
            Ty::TypeFunc { func, args, .. } => {
                format!(
                    "{}({})",
                    func.as_str(src),
                    args.iter()
                        .map(|ty| { ty.as_str(src) })
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Ty::Func { params, ret, .. } => {
                format!(
                    "\\({}) {}",
                    params
                        .iter()
                        .map(|p| { p.as_str(src) })
                        .collect::<Vec<String>>()
                        .join(", "),
                    ret.as_str(src)
                )
            }
            Ty::StaticArray { sub_ty, size, .. } => {
                let size_s = size.as_str(src);
                format!("[{size_s}]{}", sub_ty.as_str(src))
            }
            Ty::Slice { sub_ty, .. } => format!("[]{}", sub_ty.as_str(src)),
            Ty::Optional { sub_ty, .. } => format!("?{}", sub_ty.as_str(src)),
            Ty::NamedType { loc } => src.text[loc.flat_start..loc.flat_end].to_string(),
            Ty::Struct { .. } => "type".to_string(),
            Ty::Pointer { sub_ty, .. } => format!("*{}", sub_ty.as_str(src)),
            Ty::Tuple { sub_tys, .. } => {
                format!(
                    "({})",
                    sub_tys
                        .iter()
                        .map(|ty| { ty.as_str(src) })
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Ty::ErrorType { .. } => "err!".into(),
        }
    }

    pub fn clone_loc(&self, loc: Rc<SourceRef>) -> Rc<Ty> {
        let mut ty = self.clone();
        ty.set_loc(loc);
        Rc::new(ty)
    }

    pub fn get_int_ty(loc: Rc<SourceRef>) -> Rc<Ty> {
        Rc::new(Ty::Signed {
            size: Ty::get_platform_size(),
            is_int: true,
            loc,
        })
    }

    pub fn get_uint_ty(loc: Rc<SourceRef>) -> Rc<Ty> {
        Rc::new(Ty::Unsigned {
            size: Ty::get_platform_size(),
            is_uint: true,
            loc,
        })
    }

    pub fn get_platform_size() -> u8 {
        if std::mem::size_of::<usize>() == 8 {
            64
        } else {
            32
        }
    }
}
