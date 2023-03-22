use serde::{Deserialize, Serialize};
use std::{fs::File, io::Read};

use super::errors::{LexError, ParseError};

#[allow(dead_code)]
#[derive(Debug, Serialize, Deserialize, Default, Clone)]
pub struct SourceFile {
    pub path: String,
    pub text: String,
    pub flat_index: usize,
    pub col: usize,
    pub line: usize,
    pub lines: Vec<String>,
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
            lines: vec![],
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
        let mut text = String::new();
        file.read_to_string(&mut text).expect(&read_error);
        let t_lines = text.split('\n').collect::<Vec<_>>();
        for line in t_lines {
            self.lines.push(line.into());
        }

        self.text = text;
    }
}

#[allow(dead_code)]
#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
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

    pub fn as_str(&self) -> String {
        format!(
            "[start_line:{} start_col:{} end_line:{} end_col:{} flat_start:{} flat_end:{}]",
            self.start_line,
            self.start_col,
            self.end_line,
            self.end_col,
            self.flat_start,
            self.flat_end
        )
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

#[allow(dead_code)]
pub struct SourceReporter {
    src: SourceFile,
}

use crate::pastel::pastel;

#[allow(dead_code)]
impl SourceReporter {
    pub fn new(src: SourceFile) -> SourceReporter {
        SourceReporter { src }
    }

    pub fn report_lexer_error(&self, le: &LexError) {
        match le {
            LexError::InvalidCharacter(src) => {
                let msg = "Character is invalid.".to_string();
                self.report_with_ref(src, msg, None);
            }
            LexError::CannotMakeSignedNumber(src) => {
                let msg = "Number too large to fit any signed integer type.".to_string();
                self.report_with_ref(src, msg, None);
            }
            LexError::CannotMakeUnsignedNumber(src) => {
                let msg = "Number too large to fit any unsigned integer type.".to_string();
                self.report_with_ref(src, msg, None);
            }
        }
    }

    pub fn report_parse_error(&self, pe: ParseError) {
        match pe {
            ParseError::Expected(msg, src, tip) => {
                self.report_with_ref(&src, "Expected ".to_owned() + &msg, tip)
            }
            ParseError::ConstantDeclarationNeedsInitValue(src) => {
                let msg = "Constant declaration needs an initialization value.".to_string();
                let tip = "Constants are set on creation and cannot be modified after.".to_string();
                self.report_with_ref(&src, msg, Some(tip));
            }
            ParseError::CannotParseAnExpression(src) => {
                let msg = "An expression was required at this point of the program but couldn't find any.".to_string();
                self.report_with_ref(&src, msg, None);
            }
            ParseError::TooManyFnArgs(src) => {
                let msg = "Function calls only allow 256 arguments.".to_string();
                let tip = "Consider splitting this function into multiple functions that separate the work.".to_string();
                self.report_with_ref(&src, msg, Some(tip));
            }
            ParseError::MalformedDeclaration(tip, src) => {
                let msg = "Malformed declaration.".to_string();
                self.report_with_ref(&src, msg, Some(tip))
            }
            ParseError::MisuseOfPubKeyword(tip, src) => {
                let msg = "Pub keyword can only be used with function and module declarations."
                    .to_string();
                self.report_with_ref(&src, msg, Some(tip))
            }
            ParseError::TooManyFnParams(src) => {
                let msg = "Function definitions only allow 256 parameters.".to_string();
                let tip = "Consider splitting this function into multiple functions that separate the work.".to_string();
                self.report_with_ref(&src, msg, Some(tip));
            }
            ParseError::NoVariableAtTopLevel(src, tip) => {
                let msg = "Variable declarations are not allowed at the top level of a module."
                    .to_string();
                self.report_with_ref(&src, msg, tip);
            }
        }
    }

    fn report_with_ref(&self, src: &SourceRef, msg: String, tip: Option<String>) {
        let err_col = "d_red";
        let target_col = "l_cyan";
        let tip_col = "l_yellow";
        let line_col = "l_green";
        let mut output = String::new();

        // add provided msg
        output.push_str(&format!("*[_, {err_col}:d_black]{msg}[/]\n"));

        // add file name
        let f_name = &self.src.path;
        output.push_str(&format!(
            "   *[_, l_white:d_black]File '{f_name}'[/] *[*, {line_col}]{}[/]:*[*, {tip_col}]{}[/]\n",
            src.start_line + 1,
            src.start_col + 1
        ));

        // add actual target lines
        // - add first line of target area
        let f_line = self.src.lines[src.start_line].clone();
        let pre_slice = &f_line[..src.start_col];
        let f_target_slice = &f_line[src.start_col..];
        output.push_str(&format!(
            "       *[d_white:d_black]{}[/] | *[d_white:d_black]{pre_slice}[/]*[*, _, {target_col}:d_black]{f_target_slice}[/]\n",
            src.start_line + 1,
        ));

        // add any lines between
        for line_no in src.start_line + 1..src.end_line {
            let target_line = self.src.lines[line_no].clone();
            output.push_str(&format!(
                "       *[d_white:d_black]{}[/] | *[*, _, {target_col}:d_black]{target_line}[/]\n",
                line_no + 1,
            ));
        }

        // - add last line of target area (if it is not the same as first line)
        if src.end_line != src.start_line {
            let l_line = &self.src.lines[src.end_line].clone();
            let l_target_slice = &l_line[..src.end_col];
            let post_slice = &l_line[src.end_col..];
            output.push_str(&format!(
                "       *[d_white:d_black]{}[/] | *[*, _, {target_col}:d_black]{l_target_slice}[/]*[d_white:d_black]{post_slice}[/]\n",
                src.end_line + 1,
            ));
        }

        if let Some(tip_text) = tip {
            output.push_str(&format!(
                "\n*[*, _, {tip_col}:d_black]Note:[/] *[*, l_white:d_black]{tip_text}[/]\n"
            ));
        } else {
            output.push('\n');
        }
        println!("{}", pastel(&output));
    }
}
