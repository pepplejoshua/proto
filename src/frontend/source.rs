use std::{fs::File, io::Read};

#[allow(dead_code)]
pub struct SourceFile {
    pub path: String,
    pub text: String,
}

#[allow(dead_code)]
impl SourceFile {
    // new source file from a path
    pub fn new(path: String) -> SourceFile {
        SourceFile {
            path,
            text: String::new(),
        }
    }

    fn read_file(&mut self) {
        let file_not_found = format!("file not found: {}", self.path);
        let read_error = format!("something went wrong reading the file: {}", self.path);
        let mut file = File::open(&self.path).expect(&file_not_found);
        file.read_to_string(&mut self.text).expect(&read_error);
    }
}
