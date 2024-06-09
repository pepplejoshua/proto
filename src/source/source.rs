use serde::{Deserialize, Serialize};
use std::{fs::File, io::Read};

#[allow(dead_code)]
#[derive(Debug, Serialize, Deserialize, Default, Clone)]
pub struct SourceFile {
    pub path: String,
    pub text: String,
    pub flat_index: usize,
    pub col: usize,
    pub line: usize,
    pub lines: Vec<String>,
    pub longest_line_count: usize,
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
            longest_line_count: 0,
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

    // check if we are at end of file
    pub fn is_eof(&self) -> bool {
        self.flat_index >= self.text.len()
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
#[derive(serde::Deserialize, serde::Serialize, Debug, Clone, Hash, PartialEq, Eq)]
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
    pub fn dud() -> SourceRef {
        SourceRef {
            file: String::new(),
            start_line: 0,
            start_col: 0,
            end_line: 0,
            end_col: 0,
            flat_start: 0,
            flat_end: 0,
        }
    }

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

    pub fn combine(&self, other: SourceRef) -> SourceRef {
        let (start_line, start_col) = if self.start_line < other.start_line {
            (self.start_line, self.start_col)
        } else if self.start_line == other.start_line {
            if self.start_col < other.start_col {
                (self.start_line, self.start_col)
            } else {
                (other.start_line, other.start_col)
            }
        } else {
            (other.start_line, other.start_col)
        };
        let (end_line, end_col) = if self.end_line > other.end_line {
            (self.end_line, self.end_col)
        } else if self.end_line == other.end_line {
            if self.end_col > other.end_col {
                (self.end_line, self.end_col)
            } else {
                (other.end_line, other.end_col)
            }
        } else {
            (other.end_line, other.end_col)
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
#[derive(Debug, Clone)]
pub struct SourceReporter {
    src: SourceFile,
}

use crate::pastel::pastel;

use super::errors::{CheckerError, LexError, ParseError, ParseWarning};

#[allow(dead_code)]
impl SourceReporter {
    pub fn new(src: SourceFile) -> SourceReporter {
        SourceReporter { src }
    }

    pub fn report_lexer_error(&self, le: &LexError) {
        match le {
            LexError::InvalidCharacter(src) => {
                let msg = "Character is invalid.".to_string();
                self.report_with_ref(src, msg, None, false);
            }
            LexError::CannotMakeSignedNumber(src) => {
                let msg = "Number too large to fit any signed integer type.".to_string();
                self.report_with_ref(src, msg, None, false);
            }
            LexError::CannotMakeUnsignedNumber(src) => {
                let msg = "Number too large to fit any unsigned integer type.".to_string();
                self.report_with_ref(src, msg, None, false);
            }
            LexError::EmptyCharacterLiteral(src) => {
                let msg = "Empty character literal.".to_string();
                let mut tip = "Character literals must contain a single character.\n".to_string();
                tip.push_str("E.g: 'a' is a character, while '' is not.");
                self.report_with_ref(src, msg, Some(tip), false);
            }
            LexError::UnterminatedCharacterLiteral(src) => {
                let msg = "Unterminated character literal.".to_string();
                let mut tip = "Character literals must be terminated with a '.\n".to_string();
                tip.push_str("E.g: 'a' is a character, while 'a is not.");
                self.report_with_ref(src, msg, Some(tip), false);
            }
            LexError::UnterminatedStringLiteral(src) => {
                let msg = "Unterminated string literal.".to_string();
                let mut tip = "String literals must be terminated with a \".\n".to_string();
                tip.push_str(
                    "E.g: '\"hello world\"' is a string literal, while '\"hello world' is not.",
                );
                self.report_with_ref(src, msg, Some(tip), false);
            }
        }
    }

    pub fn report_parser_warning(&self, pw: ParseWarning) {
        self.report_with_ref(&pw.src, pw.msg, None, true);
    }

    pub fn report_parser_error(&self, pe: ParseError) {
        match pe {
            ParseError::Expected(msg, src, tip) => {
                self.report_with_ref(&src, "Expected ".to_string() + &msg, tip, false);
            }
            ParseError::ConstantDeclarationNeedsTypeOrInitValue(src) => {
                let msg = "Constant declaration needs an initialization type or value.".to_string();
                let tip =
                    "The compiler cannot yet back-propagate the type of a constant declaration."
                        .to_string();
                self.report_with_ref(&src, msg, Some(tip), false);
            }
            ParseError::CannotParseAnExpression(src) => {
                let msg = "An expression was required at this point of the program but couldn't find any.".to_string();
                self.report_with_ref(&src, msg, None, false);
            }
            ParseError::CannotParseAType(src) => {
                let msg = "A type was required at this point of the program but couldn't find any."
                    .to_string();
                self.report_with_ref(&src, msg, None, false);
            }
            ParseError::MalformedDeclaration(tip, src) => {
                let msg = "Malformed declaration.".to_string();
                self.report_with_ref(&src, msg, Some(tip), false);
            }
            ParseError::MisuseOfPubKeyword(src) => {
                let msg = "Misuse of 'pub' keyword.".to_string();
                let tip = "'pub' keyword can only be used with function, constant and module declarations."
                    .to_string();
                self.report_with_ref(&src, msg, Some(tip), false);
            }
            ParseError::NoVariableAtCurrentScope(src) => {
                let mut msg = "Variable declarations are not allowed:\n".to_string();
                msg.push_str(
                    "*  at the top level of module (It is allowed at the top level of the file).\n",
                );
                msg.push_str("*  inside a type extension.\n");
                let tip =
                    "Consider if this can be declared as a constant (use let instead of mut)."
                        .into();
                self.report_with_ref(&src, msg, Some(tip), false);
            }
            ParseError::UnterminatedCodeBlock(src, tip) => {
                let msg = "Code block was not terminated.".to_string();
                self.report_with_ref(&src, msg, tip, false);
            }
            ParseError::NoCodeBlockAllowedInCurrentContext(src) => {
                let msg = "Code block is not allowed in this context.".to_string();
                let mut tip = "Code blocks are not allowed:".to_string();
                tip.push_str("*  Within type extensions\n");
                tip.push_str("*  Within modules declarations\n");
                tip.push_str("*  At the top level of a file.\n");
                self.report_with_ref(&src, msg, Some(tip), false);
            }
            ParseError::ReturnInstructionOutsideFunction(src) => {
                let msg = "A return instruction can only be used in a function.".to_string();
                self.report_with_ref(&src, msg, None, false);
            }
            ParseError::NoLoopAtTopLevel(src) => {
                let msg = "Loop found at top level".to_string();
                let tip = "Loops are only allowed within functions.".to_string();
                self.report_with_ref(&src, msg, Some(tip), false);
            }
            ParseError::NoBreakOutsideLoop(src) => {
                let msg = "A break instruction can only be used inside a loop.".to_string();
                self.report_with_ref(&src, msg, None, false);
            }
            ParseError::NoContinueOutsideLoop(src) => {
                let msg = "A continue instruction can only be used inside a loop.".to_string();
                self.report_with_ref(&src, msg, None, false);
            }
            ParseError::TooManyErrors(src) => {
                let msg = "Too many errors during parsing. Stopping.".to_string();
                let tip =
                    "Errors might be cascading. Try fixing some error and recompiling.".to_string();
                self.report_with_ref(&src, msg, Some(tip), false);
            }
        }
    }

    pub fn report_checker_error(&self, ce: CheckerError) {
        match ce {
            CheckerError::InvalidType { loc, type_name } => {
                let msg = format!("Invalid type: '{}'.", type_name);
                self.report_with_ref(&loc, msg, None, false);
            }
            CheckerError::TypeMismatch {
                loc,
                expected,
                found,
            } => {
                let msg = format!(
                    "Type mismatch. Expected a value of type: `{}` but found a value of type: `{}`",
                    expected, found
                );
                self.report_with_ref(&loc, msg, None, false);
            }
            CheckerError::NumberTypeDefaultInferenceFailed { loc, number } => {
                let msg = format!("Failed to convert Number<'{}'> to i32.", number);
                self.report_with_ref(&loc, msg, None, false);
            }
            CheckerError::NumberTypeInferenceFailed {
                loc,
                number,
                given_type,
            } => {
                let msg = format!(
                    "Number '{}' is not compatible with the given type '{}'.",
                    number, given_type
                );
                self.report_with_ref(&loc, msg, None, false);
            }
            CheckerError::ReferenceToUndefinedName { loc, var_name } => {
                let msg = format!("Reference to an undefined name: '{}'.", var_name);
                self.report_with_ref(&loc, msg, None, false);
            }
            CheckerError::InvalidUseOfBinaryOperator {
                loc,
                op,
                left,
                right,
            } => {
                let msg = format!(
                    "Invalid use of binary operator '{}'. It cannot be applied to values of types '{}' and '{}'.",
                    op, left, right
                );
                self.report_with_ref(&loc, msg, None, false);
            }
            CheckerError::InvalidUseOfUnaryOperator {
                loc,
                op,
                operand,
                tip,
            } => {
                let msg = format!(
                    "Invalid use of unary operator '{}'. It cannot be applied to a value of type '{}'.",
                    op, operand
                );
                self.report_with_ref(&loc, msg, tip, false);
            }
            CheckerError::TooManyErrors => {
                let msg = format!("Too many errors during semantic analysis. Stopping.");
                let tip =
                    "Errors might be cascading. Try fixing some error and recompiling.".to_string();
                self.report_with_ref(&SourceRef::dud(), msg, Some(tip), false);
            }
            CheckerError::NameAlreadyDefined { loc, name } => {
                let msg = format!("'{name}' is already defined.");
                self.report_with_ref(&loc, msg, None, false);
            }
            CheckerError::UseOfUninitializedVariable { loc, name } => {
                let msg = format!("Use of uninitialized variable: '{}'.", name);
                self.report_with_ref(&loc, msg, None, false);
            }
            CheckerError::UseOfErroredVariableOrConstant {
                is_const,
                loc,
                name,
            } => {
                let msg = format!(
                    "Use of errored {} '{}'.",
                    if is_const { "constant" } else { "variable" },
                    name
                );
                self.report_with_ref(&loc, msg, None, false);
            }
            CheckerError::MismatchingReturnType {
                exp,
                given,
                loc_given,
            } => {
                let msg = format!("Function expects a return type of '{exp}' but a value of type '{given}' was returned.");
                self.report_with_ref(&loc_given, msg, None, false);
            }
        }
    }

    pub fn show_info(&self, msg: String) {
        let output = format!("*[_, *, l_green:d_black]info:[/] *[*, l_white:d_black]{msg}[/]");
        println!("{}", pastel(&output));
    }

    pub fn show_error(&self, msg: String) {
        let output = format!("*[_, *, l_red:d_black]Error:[/] *[*, l_white:d_black]{msg}[/]");
        println!("{}", pastel(&output));
    }

    fn report_with_ref(&self, src: &SourceRef, msg: String, tip: Option<String>, is_warning: bool) {
        let err_col = "d_red";
        let tip_col = "l_yellow";
        let line_col = if is_warning { tip_col } else { "l_green" };
        let mut output = String::new();

        // add provided msg
        output.push_str(&format!("*[_, {err_col}:d_black]{msg}[/]\n"));

        // add file name
        let f_name = &self.src.path;
        output.push_str(&format!(
                "   *[_, l_white:d_black]File '{f_name}:[/]*[*, {line_col}]{}[/]:*[*, {tip_col}]{}[/]'\n",
                src.start_line + 1,
                src.start_col + 1
            ));

        if src.start_line == src.end_line {
            let line = self.src.lines[src.start_line].clone();
            let pre_slice = &line[..src.start_col];
            let end_col = if src.end_col >= line.len() {
                line.len() - 1
            } else {
                src.end_col - 1
            };
            // make sure end_col is not same as start_col

            let target_slice = &line[src.start_col..=end_col];
            let post_slice = if end_col + 1 < line.len() {
                &line[end_col + 1..]
            } else {
                ""
            };
            output.push_str(&format!(
                    "       *[d_white:l_black]{}[/] | *[d_white:d_black]{pre_slice}[/]*[*, {err_col}:d_black]{target_slice}[/]*[d_white:d_black]{post_slice}[/]",
                    src.start_line + 1,
                ));
        } else {
            // add actual target lines
            // - add first line of target area
            let f_line = self.src.lines[src.start_line].clone();
            let pre_slice = &f_line[..src.start_col];
            let f_target_slice = &f_line[src.start_col..];
            output.push_str(&format!(
                    "       *[d_white:l_black]{}[/] | *[d_white:d_black]{pre_slice}[/]*[*, {err_col}:d_black]{f_target_slice}[/]\n",
                    src.start_line + 1,
                ));

            // add any lines between
            for line_no in src.start_line + 1..src.end_line {
                let target_line = self.src.lines[line_no].clone();
                output.push_str(&format!(
                    "       *[d_white:l_black]{}[/] | *[*, {err_col}:d_black]{target_line}[/]\n",
                    line_no + 1,
                ));
            }

            // - add last line of target area (if it is not the same as first line)
            let l_line = &self.src.lines[src.end_line].clone();
            let mut end_col = src.end_col;
            if src.end_col >= l_line.len() {
                end_col = l_line.len();
            }
            let l_target_slice = &l_line[..end_col];
            let post_slice = &l_line[end_col..];
            output.push_str(&format!(
                    "       *[d_white:l_black]{}[/] | *[*, {err_col}:d_black]{l_target_slice}[/]*[d_white:d_black]{post_slice}[/]\n",
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
