#![allow(dead_code)]

use std::rc::Rc;

use super::source::SourceRef;

#[derive(Debug, Clone)]
pub enum LexError {
    InvalidCharacter(Rc<SourceRef>),
    CannotMakeSignedNumber(Rc<SourceRef>),
    CannotMakeUnsignedNumber(Rc<SourceRef>),
    EmptyCharacterLiteral(Rc<SourceRef>),
    UnterminatedCharacterLiteral(Rc<SourceRef>),
    UnterminatedStringLiteral(Rc<SourceRef>),
    DecimalLiteralWithMultipleDecimalPoints(Rc<SourceRef>),
}

#[derive(Debug, Clone)]
pub enum ParseError {
    Expected(String, Rc<SourceRef>, Option<String>),
    ConstantDeclarationNeedsTypeOrInitValue(Rc<SourceRef>),
    CannotParseAnExpression(Rc<SourceRef>),
    MalformedDeclaration(String, Rc<SourceRef>),
    UnterminatedCodeBlock(Rc<SourceRef>, Option<String>),
    MalformedPubDeclaration { src: Rc<SourceRef> },
    ParsedInstructionIsNotAllowedAtThisLevel { level: String, src: Rc<SourceRef> },
    TooManyErrors(Rc<SourceRef>),
}

#[derive(Debug)]
pub enum CompileError {
    FileError(String, String), // (path, error message)
    LexicalErrors(Vec<crate::source::errors::LexError>),
    ParsingErrors(Vec<crate::source::errors::ParseError>),
    TypeError(String),      // Will expand this later
    CodegenError(String),   // Will expand this later
    ExecutionError(String), // Will expand this later
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::FileError(path, msg) => {
                write!(f, "Error reading file '{}': {}", path, msg)
            }
            CompileError::LexicalErrors(errors) => {
                writeln!(f, "Lexical errors:")?;
                for err in errors {
                    writeln!(f, "  {:?}", err)?;
                }
                Ok(())
            }
            CompileError::ParsingErrors(errors) => {
                writeln!(f, "Parsing errors:")?;
                for err in errors {
                    writeln!(f, "  {:?}", err)?;
                }
                Ok(())
            }
            CompileError::TypeError(msg) => write!(f, "Type error: {}", msg),
            CompileError::CodegenError(msg) => write!(f, "Code generation error: {}", msg),
            CompileError::ExecutionError(msg) => write!(f, "Execution error: {}", msg),
        }
    }
}

impl std::error::Error for CompileError {}

// Will add more error conversions as needed
impl From<std::io::Error> for CompileError {
    fn from(err: std::io::Error) -> Self {
        CompileError::FileError("".to_string(), err.to_string())
    }
}

// Display implementations for LexError and ParseError
impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexError::InvalidCharacter(src) => write!(f, "Invalid character at {}", src.as_str()),
            LexError::CannotMakeSignedNumber(src) => {
                write!(f, "Cannot make signed number at {}", src.as_str())
            }
            LexError::CannotMakeUnsignedNumber(src) => {
                write!(f, "Cannot make unsigned number at {}", src.as_str())
            }
            LexError::EmptyCharacterLiteral(src) => {
                write!(f, "Empty character literal at {}", src.as_str())
            }
            LexError::UnterminatedCharacterLiteral(src) => {
                write!(f, "Unterminated character literal at {}", src.as_str())
            }
            LexError::UnterminatedStringLiteral(src) => {
                write!(f, "Unterminated string literal at {}", src.as_str())
            }
            LexError::DecimalLiteralWithMultipleDecimalPoints(src) => {
                write!(
                    f,
                    "Decimal literal with multiple decimal points at {}",
                    src.as_str()
                )
            }
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Expected(expected, src, found) => {
                write!(
                    f,
                    "Expected {} at {}, found {:?}",
                    expected,
                    src.as_str(),
                    found
                )
            }
            ParseError::ConstantDeclarationNeedsTypeOrInitValue(src) => write!(
                f,
                "Constant declaration needs type or init value at {}",
                src.as_str()
            ),
            ParseError::CannotParseAnExpression(src) => {
                write!(f, "Cannot parse expression at {}", src.as_str())
            }
            ParseError::MalformedDeclaration(msg, src) => {
                write!(f, "Malformed declaration: {} at {}", msg, src.as_str())
            }
            ParseError::UnterminatedCodeBlock(src, ctx) => {
                write!(
                    f,
                    "Unterminated code block at {} (context: {:?})",
                    src.as_str(),
                    ctx
                )
            }
            ParseError::MalformedPubDeclaration { src } => {
                write!(f, "Malformed pub declaration at {}", src.as_str())
            }
            ParseError::ParsedInstructionIsNotAllowedAtThisLevel { level, src } => {
                write!(
                    f,
                    "{} is not allowed at this level at {}",
                    level,
                    src.as_str()
                )
            }
            ParseError::TooManyErrors(src) => write!(f, "Too many errors at {}", src.as_str()),
        }
    }
}
