use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use crate::{
    frontend::{
        ast::{DependencyPath, PathAction},
        source::SourceRef,
    },
    pir::ir::{KeyValueBindings, PIRIns, PIRModule, PIRModulePass},
};

#[allow(dead_code)]
pub struct DependencyResolvr<'a> {
    module: Option<&'a PIRModule>,
    file_path: String,
    resolution_errors: Vec<(String, SourceRef)>,
}

// expect to first process the module and get a Vec<usize> as the result
// we can then use these indices to get the corresponding ins from the module
// for each of these dependencies, we will:
// - process the actions until we get to a file path. The remainder of the actions will
//  be the path to the dependency.
impl<'a> DependencyResolvr<'a> {
    pub fn resolve(
        &mut self,
        dependencies: Vec<usize>,
    ) -> HashMap<usize, Vec<(PathBuf, DependencyPath)>> {
        let module = self.get_module();
        let ins_pool = &module.ins_pool.pool;

        // for each dependency, we will:
        // - get the Instruction (UseDependency)
        // - each UseDependency has a list of DependencyPaths:
        // - each of the DependencyPaths has a list of actions which will be used to get
        //   the path
        // - we will process the actions (while updating depenedency path) until we:
        //   * get to a file path. Stop.
        //   * get to a folder. Continue.
        //   * path is invalid. Error.
        // - once we have a valid file path, whatever is left of the actions
        //   will be the path to the dependency
        // - cluster the dependencies by their paths and return that structure

        let mut res: HashMap<usize, Vec<(PathBuf, DependencyPath)>> = HashMap::new();

        for index in dependencies {
            let dep = ins_pool.get(index).unwrap();
            if let PIRIns::UseDependency { paths, src: _ } = dep {
                for path in paths {
                    let resolved_res = self.evaluate_dependency_path(path);
                    match resolved_res {
                        Ok((res_path, rem_actions)) => {
                            // add to res
                            let res_vec = res.entry(index).or_insert(Vec::new());
                            res_vec.push((res_path, rem_actions));
                        }
                        Err(msg) => {
                            self.resolution_errors.push((msg, path.source_ref()));
                        }
                    }
                }
            }
        }

        return res;
    }

    fn evaluate_dependency_path(
        &mut self,
        dep: &DependencyPath,
    ) -> Result<(PathBuf, DependencyPath), String> {
        let mut start_dir = PathBuf::from(&self.file_path.clone());
        start_dir.pop();
        let mut search_dep_path = start_dir;
        let mut skip_to = 0;

        for action in &dep.actions {
            match action {
                PathAction::ToParentDir(_) => {
                    search_dep_path.pop();
                }
                PathAction::ImportAll(_) => {
                    break;
                }
                PathAction::SearchFor(path_section) => {
                    // append path_section to search_dep_path
                    search_dep_path.push(path_section.as_str());
                }
                PathAction::SearchCoreModulesFor(_) => todo!(),
                PathAction::SearchProjectRootFor(_) => todo!(),
                PathAction::SearchCurrentFileFor(_) => {
                    search_dep_path = PathBuf::from(&self.file_path);
                    search_dep_path.set_extension("pr");
                    break;
                }
                PathAction::NameLastItemAs(_) => todo!(),
            }

            // check that updated path exists
            let (path_exists, is_file, is_dir) = self.path_exists(&search_dep_path);
            if !path_exists {
                // report error
                let msg = format!("Unable to resolve dependency path: {}", dep.as_str());
                return Err(msg);
            }

            // update skip_to by 1, with a max of actions.len()
            skip_to += 1;
            if skip_to >= dep.actions.len() {
                break;
            }

            if is_dir {
                // continue
                continue;
            }

            if is_file {
                // stop
                search_dep_path.set_extension("pr");
                break;
            } else {
            }
        }

        // we have to verify that the path is a file
        let (path_exists, is_file, _) = self.path_exists(&search_dep_path);
        if !path_exists {
            // report error
            let msg = format!("Unable to resolve dependency path: {}", dep.as_str());
            return Err(msg);
        }

        if !is_file {
            // report error
            let msg = format!("Dependency path must contain a file: {}", dep.as_str());
            return Err(msg);
        }

        search_dep_path.set_extension("pr");

        // we have a valid file path
        // we need to collect the remainder of the actions into a Vector of PathActions
        let mut remainder = Vec::new();
        for action in &dep.actions[skip_to..] {
            remainder.push(action.clone());
        }
        let remainder = DependencyPath { actions: remainder };
        Ok((search_dep_path, remainder))
    }

    // checks if path exists and also if it is a file
    fn path_exists(&self, path: &PathBuf) -> (bool, bool, bool) {
        let mut file_path = path.clone();
        file_path.set_extension("pr");
        let file_path = Path::new(&file_path);
        let file_exists = file_path.exists();
        let dir_exists = path.exists();
        (dir_exists || file_exists, file_exists, dir_exists)
    }
}

impl<'a> PIRModulePass<'a, (), (), (), Vec<usize>, ()> for DependencyResolvr<'a> {
    fn process_ins(&mut self, _: &usize) -> Result<(), ()> {
        Ok(())
    }

    fn process_expr(&mut self, _: &usize) -> Result<(), ()> {
        Ok(())
    }

    fn process_pairs(&mut self, _: &KeyValueBindings) -> Result<(), ()> {
        Ok(())
    }

    fn process(&mut self) -> Result<Vec<usize>, ()> {
        let module = self.get_module();
        let ins_pool = &module.ins_pool.pool;
        let mut dependencies = Vec::new();

        for (index, ins) in ins_pool.iter().enumerate() {
            if matches!(ins, PIRIns::UseDependency { paths: _, src: _ }) {
                dependencies.push(index);
            }
        }

        Ok(dependencies)
    }

    fn new(module: &'a PIRModule) -> Self {
        Self {
            module: Some(module),
            file_path: module.path.clone(),
            resolution_errors: Vec::new(),
        }
    }

    fn get_module(&mut self) -> &'a PIRModule {
        self.module.unwrap()
    }
}
