use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use crate::{
    frontend::{
        ast::{DependencyPath, PathAction},
        errors::DependencyResolvrError,
        source::SourceRef,
    },
    pir::ir::{ExprRef, InsRef, KeyValueBindings, PIRIns, PIRModule, PIRModulePass},
};

#[derive(Clone)]
#[allow(dead_code)]
pub struct DependencyResolvr<'a> {
    module: Option<&'a PIRModule>,
    file_path: String,
    pub errors: Vec<DependencyResolvrError>,
}

// expect to first process the module and get a Vec<usize> as the result
// we can then use these indices to get the corresponding ins from the module
// for each of these dependencies, we will:
// - process the actions until we get to a file path. The remainder of the actions will
//  be the path to the dependency.
impl<'a> DependencyResolvr<'a> {
    fn evaluate_dependency_path(
        &mut self,
        dep: &DependencyPath,
        src_ref: &SourceRef,
    ) -> Result<(PathBuf, DependencyPath), DependencyResolvrError> {
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
                return Err(DependencyResolvrError::UnableToResolvePath(src_ref.clone()));
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
            return Err(DependencyResolvrError::UnableToResolvePath(src_ref.clone()));
        }

        if !is_file {
            // report error
            return Err(DependencyResolvrError::DependencyPathMustContainFile(
                src_ref.clone(),
            ));
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

type DependencyBundle = HashMap<usize, Vec<(PathBuf, DependencyPath)>>;
impl<'a> PIRModulePass<'a, (), (), (), DependencyBundle, ()> for DependencyResolvr<'a> {
    fn process_ins(&mut self, _: &InsRef) -> Result<(), ()> {
        Ok(())
    }

    fn process_expr(&mut self, _: &ExprRef) -> Result<(), ()> {
        Ok(())
    }

    fn process_pairs(&mut self, _: &KeyValueBindings) -> Result<(), ()> {
        Ok(())
    }

    fn process(&mut self) -> Result<DependencyBundle, ()> {
        let module = self.get_module();
        let ins_pool = &module.ins_pool.pool;
        let mut res: HashMap<usize, Vec<(PathBuf, DependencyPath)>> = HashMap::new();

        for (index, ins) in ins_pool.iter().enumerate() {
            if let PIRIns::UseDependency { paths, src } = ins {
                for path in paths {
                    let resolved_res = self.evaluate_dependency_path(path, src);
                    match resolved_res {
                        Ok((res_path, rem_actions)) => {
                            // add to res
                            let res_vec = res.entry(index).or_insert(Vec::new());
                            res_vec.push((res_path, rem_actions));
                        }
                        Err(err) => {
                            self.errors.push(err);
                        }
                    }
                }
            }
        }

        Ok(res)
    }

    fn new(module: &'a PIRModule) -> Self {
        Self {
            module: Some(module),
            file_path: module.path.clone(),
            errors: Vec::new(),
        }
    }

    fn get_module(&mut self) -> &'a PIRModule {
        self.module.unwrap()
    }
}
