#[allow(dead_code)]
pub enum ProtoError {
    FileNotFound { path: String },
    ReadError { path: String },
}

#[allow(dead_code)]
impl ProtoError {
    pub fn report_error(&self) {
        match self {
            ProtoError::FileNotFound { path } => {
                println!("file not found: {path}");
            }
            ProtoError::ReadError { path } => {
                println!("something went wrong reading the file: {path}");
            }
        }
    }
}
