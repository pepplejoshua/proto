#![allow(unused)]
use crate::{
    parser::{
        ast::{FnParam, Ins, ModulePath, ModulePathPart},
        type_signature::Ty,
        Parser,
    },
    source::source::SourceFile,
};
use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    rc::Rc,
    thread::current,
};

use super::CompileMode;

/// Represents a symbol in a module
#[derive(Debug)]
enum ModuleSymbol {
    Variable {
        name: String,
        ty: Rc<Ty>,
        is_mutable: bool,
        is_public: bool,
    },
    Function {
        name: String,
        params: Vec<FnParam>,
        return_ty: Rc<Ty>,
        is_public: bool,
    },
    Type {
        name: String,
        ty: Rc<Ty>,
        is_public: bool,
    },
}

/// Represents a module's compilation state
struct Module {
    path: PathBuf,
    instructions: Vec<Ins>,
    symbols: HashMap<String, ModuleSymbol>,
    is_compiled: bool,
}

/// Represents the state and context of a single compilation
pub struct CompileContext {
    /// Whether we are compiling a simple script or a project
    mode: CompileMode,

    /// Root directory for resolving imports
    /// - In script mode: directory containing the script
    /// - in project mode: project root directory (containing project.pr)
    root_dir: PathBuf,

    /// Entry point file
    /// - In script mode: the script file
    /// - In project mode: src/main.pr
    entry_point: PathBuf,

    /// Track loaded modules
    modules: HashMap<PathBuf, Module>,

    /// Track module dependencies
    dependencies: HashMap<PathBuf, HashSet<PathBuf>>,
}

impl CompileContext {
    /// Create a new compilation context for script mode
    pub fn new_script(script_path: PathBuf) -> Result<Self, CompileError> {
        // convert to absolute path
        let script_path = script_path
            .canonicalize()
            .map_err(|_| CompileError::FileNotFound(script_path.clone()))?;

        // get the directory containing the script
        let root_dir = script_path
            .parent()
            .ok_or_else(|| CompileError::InvalidPath(script_path.clone()))?
            .to_path_buf();

        Ok(Self {
            mode: CompileMode::Script {
                path: script_path.clone(),
            },
            root_dir,
            entry_point: script_path,
            modules: HashMap::new(),
            dependencies: HashMap::new(),
        })
    }

    /// Create a new compilation context for project mode
    pub fn new_project(project_dir: PathBuf, run: bool) -> Result<Self, CompileError> {
        // convert to absolute path
        let project_dir = project_dir
            .canonicalize()
            .map_err(|_| CompileError::FileNotFound(project_dir.clone()))?;

        // check for project.pr
        let project_file = project_dir.join("project.pr");
        if !project_file.exists() {
            return Err(CompileError::MissingProjectFile(project_dir));
        }

        // check for src/main.pr
        let entry_point = project_dir.join("src").join("main.pr");
        if !entry_point.exists() {
            return Err(CompileError::MissingEntryPoint(entry_point));
        }

        Ok(Self {
            mode: CompileMode::Project {
                root_dir: project_dir.clone(),
                run,
            },
            root_dir: project_dir,
            entry_point,
            modules: HashMap::new(),
            dependencies: HashMap::new(),
        })
    }

    fn resolve_module_path(
        &self,
        use_path: &ModulePath,
        from_file: &PathBuf,
    ) -> Result<PathBuf, CompileError> {
        match &self.mode {
            CompileMode::Script { .. } => {
                // In script mode:
                // - No 'root' keyword allowed
                // - All paths are relative to the script's location
                if let Some(&ModulePathPart::Root) = use_path.parts.first() {
                    return Err(CompileError::InvalidModulePath(
                        from_file.clone(),
                        "'root' keyword is only valid in project mode".into(),
                    ));
                }
                self.resolve_script_import(use_path, from_file)
            }
            CompileMode::Project { root_dir, run } => {
                // In project mode:
                // - 'root' means project root
                // - Relative paths are relative to current module
                // - Non-root paths are relative to src/
                self.resolve_project_import(use_path, from_file)
            }
        }
    }

    fn resolve_script_import(
        &self,
        use_path: &ModulePath,
        from_file: &PathBuf,
    ) -> Result<PathBuf, CompileError> {
        // start from the directory containing the importing file
        let mut current_dir = from_file
            .parent()
            .ok_or_else(|| CompileError::InvalidPath(from_file.clone()))?
            .to_path_buf();

        // process path parts
        for part in &use_path.parts {
            match part {
                ModulePathPart::Root => unreachable!("Use of root checked in resolve_module_path"),
                ModulePathPart::Parent => {
                    current_dir = current_dir
                        .parent()
                        .ok_or_else(|| CompileError::InvalidPath(current_dir.clone()))?
                        .to_path_buf();
                }
                ModulePathPart::Current => {
                    // stay in current directory
                }
                ModulePathPart::Name(name) => current_dir.push(name),
            }
        }

        // add .pr extension
        current_dir.set_extension(".pr");

        if !current_dir.exists() {
            return Err(CompileError::FileNotFound(current_dir));
        }

        Ok(current_dir)
    }

    fn resolve_project_import(
        &self,
        use_path: &ModulePath,
        from_file: &PathBuf,
    ) -> Result<PathBuf, CompileError> {
        // first determine starting directory
        let mut current_dir = match use_path.parts.first() {
            // if path starts with 'root', start from src/ directory
            Some(ModulePathPart::Root) => self.root_dir.join("src"),
            // for relative paths, start from importing file's directory
            _ => {
                // verify importing file is within src/
                let src_dir = self.root_dir.join("src");
                if !from_file.starts_with(&src_dir) {
                    return Err(CompileError::InvalidPath(
                        "All modules must be within src directory".into(),
                    ));
                }

                from_file
                    .parent()
                    .ok_or_else(|| CompileError::InvalidPath(from_file.clone()))?
                    .to_path_buf()
            }
        };

        // process remaining parts
        for part in use_path.parts.iter().skip(
            if matches!(use_path.parts.first(), Some(ModulePathPart::Root)) {
                1
            } else {
                0
            },
        ) {
            match part {
                ModulePathPart::Root => {
                    return Err(CompileError::InvalidModulePath(
                        current_dir.clone(),
                        "'root' can only appear at start of path".into(),
                    ));
                }
                ModulePathPart::Parent => {
                    // get parent directory
                    let parent = current_dir
                        .parent()
                        .ok_or_else(|| CompileError::InvalidPath(current_dir.clone()))?
                        .to_path_buf();

                    // check if going up would escape src/
                    if !parent.starts_with(&self.root_dir.join("src")) {
                        return Err(CompileError::InvalidPath(
                            "Cannot use '..' to escape src directory".into(),
                        ));
                    }

                    current_dir = parent;
                }
                ModulePathPart::Current => {
                    // stay in current directory
                }
                ModulePathPart::Name(name) => current_dir.push(name),
            }
        }

        // add .pr extension
        current_dir.set_extension("pr");

        // final verification that we're still in src/
        if !current_dir.starts_with(&self.root_dir.join("src")) {
            return Err(CompileError::InvalidPath(
                "Resolved path must be within src directory".into(),
            ));
        }

        // check file exists
        if !current_dir.exists() {
            return Err(CompileError::FileNotFound(current_dir));
        }

        Ok(current_dir)
    }

    fn load_and_parse_file(&mut self, path: &PathBuf) -> Result<(), CompileError> {
        // don't load if already loaded
        if self.modules.contains_key(path) {
            return Ok(());
        }

        let source = SourceFile::new(path.to_str().unwrap().to_string());
        let lexer = crate::lexer::lexer::Lexer::new(source);
        let mut parser = Parser::new(lexer);

        let instructions = parser.parse_file();

        // create new module with parsed instructions
        let module = Module {
            path: path.clone(),
            instructions,
            symbols: HashMap::new(),
            is_compiled: false,
        };

        // track dependencies from 'use' statements
        let mut deps = HashSet::new();
        for use_decl in &parser.use_declarations {
            if let Ins::DeclUse { items, .. } = use_decl {
                for item in items {
                    let dep_path = self.resolve_module_path(&item.path, path)?;
                    deps.insert(dep_path.clone());

                    // recursively load dependencies
                    self.load_and_parse_file(&dep_path)?;
                }
            }
        }

        self.modules.insert(path.clone(), module);
        self.dependencies.insert(path.clone(), deps);

        Ok(())
    }

    fn generate_compilation_order(&self) -> Result<Vec<PathBuf>, CompileError> {
        let mut order = Vec::new();
        let mut visited = HashSet::new();
        let mut path = Vec::new(); // current path for cycle detection

        // helper closure for depth-first search
        fn visit(
            ctx: &CompileContext,
            current: &PathBuf,
            visited: &mut HashSet<PathBuf>,
            path: &mut Vec<PathBuf>,
            order: &mut Vec<PathBuf>,
        ) -> Result<(), CompileError> {
            // check for cycle
            if path.contains(current) {
                path.push(current.clone());
                return Err(CompileError::CircularDependency(path.clone()));
            }

            // skip if already visted
            if visited.contains(current) {
                return Ok(());
            }

            // add to current path
            path.push(current.clone());

            // visit all dependencies first
            if let Some(deps) = ctx.dependencies.get(current) {
                for dep in deps {
                    visit(ctx, dep, visited, path, order)?;
                }
            }

            // mark as visited and add to order
            visited.insert(current.clone());
            order.push(current.clone());

            // remove from current path as we backtrack
            path.pop();

            Ok(())
        }

        // start from entry point
        let entry_point = self.entry_point.clone();
        visit(self, &entry_point, &mut visited, &mut path, &mut order)?;

        Ok(order)
    }

    /// Start compilation process
    pub fn compile(&mut self) -> Result<(), CompileError> {
        match &self.mode {
            CompileMode::Script { .. } => self.compile_script(),
            CompileMode::Project { .. } => self.compile_project(),
        }
    }

    fn compile_script(&mut self) -> Result<(), CompileError> {
        // load and parse the main script
        self.load_and_parse_file(&self.entry_point.clone())?;

        // generate compilation order (also checks for cycles)
        let compilation_order = self.generate_compilation_order()?;

        // process modules in order
        for module_path in compilation_order {
            self.compile_module(&module_path)?;
        }

        Ok(())
    }

    fn compile_module(&mut self, module_path: &PathBuf) -> Result<(), CompileError> {
        let module = self.modules.get_mut(module_path).ok_or_else(|| {
            CompileError::LoadError(module_path.clone(), "Module not loaded".into())
        })?;

        // collect module's symbols
        for ins in &module.instructions {
            match ins {
                Ins::DeclVariable {
                    name,
                    ty,
                    is_mutable,
                    is_public,
                    ..
                } => {
                    let var_name = name.as_str();
                    let symbol = ModuleSymbol::Variable {
                        name: var_name.clone(),
                        ty: ty.clone().unwrap(), // TODO: check for explicit type
                        is_mutable: *is_mutable,
                        is_public: *is_public,
                    };
                    module.symbols.insert(var_name, symbol);
                }
                Ins::DeclFunc {
                    name,
                    params,
                    ret_ty,
                    is_public,
                    ..
                } => {
                    let func_name = name.as_str();
                    let symbol = ModuleSymbol::Function {
                        name: func_name.clone(),
                        params: params.clone(),
                        return_ty: ret_ty.clone(),
                        is_public: *is_public,
                    };
                    module.symbols.insert(func_name, symbol);
                }
                Ins::DeclTypeAlias {
                    name,
                    ty,
                    is_public,
                    ..
                } => {
                    let alias = name.as_str();
                    let symbol = ModuleSymbol::Type {
                        name: alias.clone(),
                        ty: ty.clone(),
                        is_public: *is_public,
                    };
                    module.symbols.insert(alias, symbol);
                }
                _ => {}
            }
        }

        module.is_compiled = true;
        Ok(())
    }

    fn compile_project(&mut self) -> Result<(), CompileError> {
        todo!()
    }
}

#[derive(Debug)]
pub enum CompileError {
    FileNotFound(PathBuf),
    InvalidPath(PathBuf),
    InvalidModulePath(PathBuf, String),
    MissingProjectFile(PathBuf),
    MissingEntryPoint(PathBuf),
    CircularDependency(Vec<PathBuf>),
    LoadError(PathBuf, String),
}
