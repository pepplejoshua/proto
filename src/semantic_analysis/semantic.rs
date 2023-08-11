use super::sym_table::{SemanticAnalysisError, SymbolTable};
use crate::{
    pir::ir::{KeyValueBindings, PIRExpr, PIRIns, PIRModule, PIRModulePass},
    semantic_analysis::{sym_table::Symbol, types::PIRTypes},
};

#[allow(dead_code)]
pub struct SemanticAnalyzr<'a> {
    module: &'a PIRModule,
    symbol_table: SymbolTable,
    pub errors: Vec<SemanticAnalysisError>,
    new_module: PIRModule,
}

impl<'a> SemanticAnalyzr<'a> {
    pub fn extract(self) -> Result<PIRModule, Vec<SemanticAnalysisError>> {
        if self.errors.is_empty() {
            Ok(self.new_module)
        } else {
            Err(self.errors)
        }
    }
}
/*
update file import syntax to use new syntax similar to js import syntax
e.g: import [foo, bar] from "baz"; // baz is a file in the same directory with foo and bar as public symbols
it will only allow top level imports, so no nested imports like before

you can reference internal symbols of imports using the :: operator
e.g: import [foo, bar] from "baz"; // baz is a file in the same directory with foo and bar as public symbols
foo::bar::baz(); // baz is a function defined in bar, which is defined in foo
use foo::bar; // this exposes bar as a symbol in the current scope for use
*/

/*
I need to be able to accumulate only relevant type information about a file.

a.pr:
use b::code;

fn main() void {
    code::foo();
}

b.pr:
pub mod code {
    pub fn foo() void {
        println("hello world");
    }
}

{
    'b.pr': {
        'foo': {
            path_to_info: ['b.pr', 'code', 'foo'],
            'type': 'fn() void',
            'is_public': true,
            'src': SourceRef { start: 0, end: 0 },
        }
        'code': {
            path_to_info: ['b.pr', 'code'],
            'type': 'mod',
            'is_public': true,
            'src': SourceRef { start: 0, end: 0 },
            'contains': ['foo'],
        }
    }
    'a.pr': {
        'main': {
            path_to_info: ['a.pr', 'main'],
            'type': 'fn() void',
            'is_public': false,
            'src': SourceRef { start: 0, end: 0 },
        }
    }
}

when b.pr is analyzed, it should return information about public symbols.
*/

/*
processing a single file, a.pr where a.pr contains:

fn main() void {
    let x = 5;
    let y = 10;
    let z i8 = y;
}

since a.pr is the entry point, it will return the following information:
{
    'a.pr': {
        'main': {
            path_to_info: ['a.pr', 'main'],
            'type': 'fn() void', // will be an actual type object
            'is_public': false,
            'src': SourceRef { start: 0, end: 0 },
        }
    }
}

and the source should be fully type annotated as:
fn main() void {
    let x i8 = 5;
    let y i8 = 10;
    let z i8 = y;
}
*/
#[allow(unused_variables)]
impl<'a> PIRModulePass<'a, Option<usize>, usize, (), (), SemanticAnalysisError>
    for SemanticAnalyzr<'a>
{
    fn process_ins(&mut self, ins: &usize) -> Result<Option<usize>, SemanticAnalysisError> {
        let module = self.module;
        let ins_node = &mut module.ins_pool.get(&ins);

        match ins_node {
            PIRIns::CodeBlock { instructions, .. } => {
                // create a new scope
                self.symbol_table.enter_scope();

                // process all instructions in the block
                for ins in instructions {
                    let res = self.process_ins(ins);
                    if let Err(e) = res {
                        self.errors.push(e);
                    }
                }

                // pop the scope
                self.symbol_table.exit_scope();

                // update the new_module
                self.new_module.ins_pool.pool.push(ins_node.clone());
                Ok(None)
            }
            PIRIns::FunctionDef {
                name,
                params,
                return_type,
                body,
                is_public,
                src,
            } => {
                // make sure name is not already defined in the current scope
                // TODO: get the SourceRef of the original definition of the symbol
                // and include it in the error message to improve error messages
                if self.symbol_table.shallow_sym_exists(&name.as_str()) {
                    return Err(SemanticAnalysisError::RedefinitionOfSymbol(src.clone()));
                }

                // make sure return type exists
                let return_type = self
                    .symbol_table
                    .get_type_loc(&return_type.as_str(), return_type.get_source_ref().unwrap())?;

                // make sure all params have valid types
                let mut param_signature = Vec::new();
                for param in params.into_iter() {
                    let param = self.module.expr_pool.get(param);
                    if let PIRExpr::Id(param_name, param_type, _) = param {
                        let param_type = param_type.unwrap();
                        let param_type_loc = self.symbol_table.get_type_loc(
                            &param_type.as_str(),
                            param_type.get_source_ref().unwrap(),
                        )?;
                        let param_sym = Symbol {
                            identifier: param_name.as_str(),
                            definition_loc: None,
                            depth: self.symbol_table.depth + 1,
                            associated_type: Some(param_type_loc),
                            been_initialized: true,
                            is_mutable: true, // TODO: when params get attributes, update this accordingly
                        };
                        self.symbol_table.register_sym(param_sym);
                        param_signature.push(param_type_loc);
                    } else {
                        unreachable!("params should only be PIRExpr::Id");
                    }
                }

                // add function to symbol table before processing its body
                // so that it can be used recursively and even if processing the body fails
                // the function can still be referenced reliably during analysis
                let function_sig = PIRTypes::Function {
                    name: name.as_str(),
                    param_signature,
                    return_type,
                    definition_loc: Some(*ins),
                    depth: self.symbol_table.depth,
                };
                let sig_loc = self.symbol_table.register_type(function_sig);

                let function_sym = Symbol {
                    identifier: name.as_str(),
                    definition_loc: Some(*ins),
                    depth: self.symbol_table.depth,
                    associated_type: Some(sig_loc),
                    been_initialized: false,
                    is_mutable: false,
                };
                self.symbol_table.register_sym(function_sym);

                // add params to new_module
                for param in params {
                    let param = self.module.expr_pool.get(param);
                    self.new_module.expr_pool.pool.push(param.clone());
                }

                // process the body of the function
                // TODO: make sure the return type of the function matches the type of the body
                self.process_ins(body)?;

                // add function to new_module
                self.new_module.ins_pool.pool.push(ins_node.clone());
                Ok(None)
            }
            PIRIns::VariableDecl(name_t, ref mut type_o, init_o, src) => {
                // make sure name is not already defined in the current scope
                // TODO: get the SourceRef of the original definition of the symbol
                // and include it in the error message to improve error messages
                if self.symbol_table.shallow_sym_exists(&name_t.as_str()) {
                    return Err(SemanticAnalysisError::RedefinitionOfSymbol(src.clone()));
                }

                // make sure type exists
                let mut expected_var_type_loc = None;
                if let Some(type_name) = type_o {
                    let get_type_loc = self
                        .symbol_table
                        .get_type_loc(&type_name.as_str(), type_name.get_source_ref().unwrap())?;
                    expected_var_type_loc = Some(get_type_loc);
                }

                // type check init_o
                if let Some(expr) = init_o {
                    // typecheck the expression, and make sure it matches the type
                    // of the variable, if it has a type
                    let expr_type_loc = self.process_expr(&expr)?;
                    if let Some(var_type_loc) = expected_var_type_loc {
                        // get SourceRef of the type of the variable
                        // and the SourceRef of the init expression
                        let var_type_src = type_o.clone().unwrap().get_source_ref().unwrap();
                        let actual_expr = self.module.expr_pool.get(&expr);
                        let expr_src = actual_expr.source_ref();

                        // compare the types in the symbol table
                        self.symbol_table.loc_compare_types(
                            &var_type_loc,
                            var_type_src,
                            &expr_type_loc,
                            expr_src,
                        )?;

                        let new_sym = Symbol {
                            identifier: name_t.as_str(),
                            definition_loc: Some(*ins),
                            depth: self.symbol_table.depth,
                            associated_type: Some(expr_type_loc),
                            been_initialized: true,
                            is_mutable: true,
                        };

                        // since the types match, we can set the type of the variable
                        // to the type of the expression
                        self.symbol_table.register_sym(new_sym);
                        self.new_module.ins_pool.pool.push(ins_node.clone());
                    } else {
                        // if the variable doesn't have a type, then we can just
                        // set the type of the variable to the type of the expression
                        let new_sym = Symbol {
                            identifier: name_t.as_str(),
                            definition_loc: Some(*ins),
                            depth: self.symbol_table.depth,
                            associated_type: Some(expr_type_loc),
                            been_initialized: true,
                            is_mutable: true,
                        };
                        self.symbol_table.register_sym(new_sym);
                        self.new_module.ins_pool.pool.push(PIRIns::VariableDecl(
                            name_t.clone(),
                            Some(self.symbol_table.make_type_reference(expr_type_loc)),
                            init_o.clone(),
                            src.clone(),
                        ));
                    }
                } else if let Some(var_type_loc) = expected_var_type_loc {
                    // if the variable doesn't have an initializer
                    // set type provided
                    let new_sym = Symbol {
                        identifier: name_t.as_str(),
                        definition_loc: Some(*ins),
                        depth: self.symbol_table.depth,
                        associated_type: Some(var_type_loc),
                        been_initialized: false,
                        is_mutable: true,
                    };
                    self.symbol_table.register_sym(new_sym);
                    self.new_module.ins_pool.pool.push(ins_node.clone());
                } else {
                    unreachable!("No init expression or type provided for Variable Decl");
                }
                Ok(None)
            }
            _ => {
                self.new_module.ins_pool.pool.push(ins_node.clone());
                Ok(None)
            }
        }
    }

    fn process_expr(&mut self, expr: &usize) -> Result<usize, SemanticAnalysisError> {
        let module = self.module;
        let expr_node = &module.expr_pool.get(&expr);

        match expr_node {
            PIRExpr::Id(name, _, _) => {
                // whenever we use process_expr to handle an Id, maybe_type will be empty.
                // maybe_type will only be used when checking a function parameter, or
                // a struct field, which are handled in process_ins and
                // process_pairs respectively, so we can ignore the type in this case.

                // make sure name exists in the symbol table
                if !self.symbol_table.sym_exists(&name.as_str()) {
                    return Err(SemanticAnalysisError::UndefinedSymbol(
                        name.as_str(),
                        name.get_source_ref(),
                    ));
                }

                // get the location of the type of the symbol and return
                let def_sym = self.symbol_table.get_sym(&name.as_str()).unwrap();
                if def_sym.been_initialized {
                    // then we can unwrap the type
                    let id_type = def_sym.associated_type.unwrap();
                    // update new_module
                    self.new_module.expr_pool.pool.push(expr_node.clone());
                    return Ok(id_type);
                } else {
                    // if the symbol has not been initialized, throw an error
                    return Err(SemanticAnalysisError::UseOfSymbolBeforeInitialization(
                        name.as_str(),
                        name.get_source_ref(),
                    ));
                }
            }
            PIRExpr::Number(node, value_type, _)
            | PIRExpr::StringLiteral(node, value_type, _)
            | PIRExpr::CharacterLiteral(node, value_type, _)
            | PIRExpr::Boolean(node, value_type, _) => {
                // ASSUME that the type of number exists
                let value_type = value_type.clone().unwrap();
                let num_type_loc = self
                    .symbol_table
                    .get_type_loc(&value_type.as_str(), node.get_source_ref())
                    .unwrap();
                // update new_module
                self.new_module.expr_pool.pool.push(expr_node.clone());
                return Ok(num_type_loc);
            }
            _ => {}
        }
        todo!("Unimplemented expr type")
    }

    fn process_pairs(&mut self, kv: &KeyValueBindings) -> Result<(), SemanticAnalysisError> {
        // go through fields, check the type of the field
        // and verify it exists in the symbol table
        for (name_with_type, optional_init) in kv.pairs.iter() {
            // validate that the type of the field exists
            let field = self.module.expr_pool.get(&name_with_type);
            if let PIRExpr::Id(name, field_type, _) = field {
                // check if the type exists
                if let Some(field_type) = field_type {
                    if !self.symbol_table.type_exists(&field_type.as_str()) {
                        return Err(SemanticAnalysisError::UndefinedType(
                            field_type.as_str(),
                            name.get_source_ref(),
                        ));
                    }
                }
            }
        }

        todo!("Implement updating new_module");
    }

    fn process(&mut self) -> Result<(), SemanticAnalysisError> {
        let module = self.module;
        let top_level = &module.top_level;

        for ins_ref in top_level {
            let res = self.process_ins(ins_ref);
            if let Err(err) = res {
                self.errors.push(err);
            }
        }

        // set the top level instructions inside new_module
        self.new_module.top_level = module.top_level.clone();

        Ok(())
    }

    fn new(module: &'a PIRModule) -> Self {
        Self {
            module,
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),
            new_module: PIRModule::empty(),
        }
    }

    fn get_module(&mut self) -> &'a PIRModule {
        self.module
    }
}
