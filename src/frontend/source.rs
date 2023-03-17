use serde::{Deserialize, Serialize};
use std::{fs::File, io::Read};

#[allow(dead_code)]
#[derive(Debug, Serialize, Deserialize, Default)]
pub struct SourceFile {
    pub path: String,
    pub text: String,
    pub flat_index: usize,
    pub col: usize,
    pub line: usize,
}

#[allow(dead_code)]
impl SourceFile {
    // new source file from a path
    pub fn new(path: String) -> SourceFile {
        let mut src_file = SourceFile {
            path,
            text: String::new(),
            flat_index: 0,
            col: 0,
            line: 0,
        };
        src_file.read_file();
        src_file
    }

    // get a reference to the current position in the source file
    pub fn get_ref(&self) -> SourceRef {
        SourceRef {
            file: self.path.clone(),
            start_line: self.line,
            start_col: self.col,
            end_line: self.line,
            end_col: self.col,
            flat_start: self.flat_index,
            flat_end: self.flat_index,
        }
    }

    // jump to a specific position in the source file from a reference
    pub fn jump_to(&mut self, src_ref: &SourceRef) {
        self.flat_index = src_ref.flat_start;
        self.line = src_ref.start_line;
        self.col = src_ref.start_col;
    }

    // return the next character in the source file and advance the fields
    // accordingly:
    // - flat_index: increase by 1 (as long as we read a character). If we return EOF, flat_index
    //               will be the length of the text (and will not increase anymore)
    // - col: increase by 1 (as long as we read a character). If we return EOF, col will be the
    //        length of the last line (and will not increase anymore)
    // - line: increase by 1 if we read a '\n' character. If we return EOF, line will be the
    //         number of lines in the text (and will not increase anymore)
    pub fn next_char(&mut self) -> char {
        // if we are on the EOF character
        if self.flat_index >= self.text.len() {
            return '\0';
        }

        // if we are on the last character in the file
        if self.flat_index + 1 == self.text.len() {
            self.flat_index += 1;
            self.col += 1;
            return '\0';
        }

        // if previous character was a newline character
        if self.text.chars().nth(self.flat_index).unwrap() == '\n' {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
        self.flat_index += 1;
        let c = self.text.chars().nth(self.flat_index).unwrap();
        c
    }

    // peek next character without advancing the fields
    pub fn peek_char(&self) -> char {
        if self.flat_index + 1 >= self.text.len() {
            return '\0';
        }
        self.text.chars().nth(self.flat_index + 1).unwrap()
    }

    // current character without advancing the fields
    pub fn cur_char(&self) -> char {
        if self.flat_index >= self.text.len() {
            return '\0';
        }
        self.text.chars().nth(self.flat_index).unwrap()
    }

    fn read_file(&mut self) {
        let file_not_found = format!("{}: file not found.", self.path);
        let mut file = File::open(&self.path).expect(&file_not_found);
        let read_error = format!(
            "{}: something went wrong trying to read the file.",
            self.path
        );
        file.read_to_string(&mut self.text).expect(&read_error);
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct SourceRef {
    pub file: String,      // file path
    pub start_line: usize, // start line
    pub start_col: usize,  // start column
    pub end_line: usize,   // end line
    pub end_col: usize,    // end column
    pub flat_start: usize, // start index in the flat text
    pub flat_end: usize,   // end index in the flat text
}

#[allow(dead_code)]
impl SourceRef {
    pub fn new(
        file: String,
        start_line: usize,
        start_col: usize,
        end_line: usize,
        end_col: usize,
        flat_start: usize,
        flat_end: usize,
    ) -> SourceRef {
        SourceRef {
            file,
            start_line,
            start_col,
            end_line,
            end_col,
            flat_start,
            flat_end,
        }
    }

    // combine 2 SourceRefs to create a new one that:
    // - has the same file
    // - has the first occuring start_line and start_col
    // - has the last occuring end_line and end_col
    // - has the first occuring flat_start
    // - has the last occuring flat_end
    pub fn combine(&self, other: SourceRef) -> SourceRef {
        let start_line = if self.start_line < other.start_line {
            self.start_line
        } else {
            other.start_line
        };
        let start_col = if self.start_col < other.start_col {
            self.start_col
        } else {
            other.start_col
        };
        let end_line = if self.end_line > other.end_line {
            self.end_line
        } else {
            other.end_line
        };
        let end_col = if self.end_col > other.end_col {
            self.end_col
        } else {
            other.end_col
        };
        let flat_start = if self.flat_start < other.flat_start {
            self.flat_start
        } else {
            other.flat_start
        };
        let flat_end = if self.flat_end > other.flat_end {
            self.flat_end
        } else {
            other.flat_end
        };
        SourceRef::new(
            self.file.clone(),
            start_line,
            start_col,
            end_line,
            end_col,
            flat_start,
            flat_end,
        )
    }
}
