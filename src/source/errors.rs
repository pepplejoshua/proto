#![allow(dead_code)]

use super::source::SourceRef;

#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
pub enum LexError {
    InvalidCharacter(SourceRef),
    CannotMakeSignedNumber(SourceRef),
    CannotMakeUnsignedNumber(SourceRef),
    EmptyCharacterLiteral(SourceRef),
    UnterminatedCharacterLiteral(SourceRef),
    UnterminatedStringLiteral(SourceRef),
}

#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
pub enum ParseError {
    Expected(String, SourceRef, Option<String>),
    ConstantDeclarationNeedsTypeOrInitValue(SourceRef),
    CannotParseAnExpression(SourceRef),
    CannotParseAType(SourceRef),
    MalformedDeclaration(String, SourceRef),
    NoVariableAtCurrentScope(SourceRef),
    NoCodeBlockAllowedInCurrentContext(SourceRef),
    NoLoopAtTopLevel(SourceRef),
    NoBreakOutsideLoop(SourceRef),
    NoContinueOutsideLoop(SourceRef),
    MisuseOfPubKeyword(SourceRef),
    UnterminatedCodeBlock(SourceRef, Option<String>),
    ReturnInstructionOutsideFunction(SourceRef),
    TooManyErrors(SourceRef),
}

impl ParseError {
    pub fn get_error_src(&self) -> SourceRef {
        match self {
            ParseError::Expected(_, src, _) => src.clone(),
            ParseError::ConstantDeclarationNeedsTypeOrInitValue(src) => src.clone(),
            ParseError::CannotParseAnExpression(src) => src.clone(),
            ParseError::CannotParseAType(src) => src.clone(),
            ParseError::MalformedDeclaration(_, src) => src.clone(),
            ParseError::NoCodeBlockAllowedInCurrentContext(src) => src.clone(),
            ParseError::NoLoopAtTopLevel(src) => src.clone(),
            ParseError::NoBreakOutsideLoop(src) => src.clone(),
            ParseError::NoContinueOutsideLoop(src) => src.clone(),
            ParseError::MisuseOfPubKeyword(src) => src.clone(),
            ParseError::UnterminatedCodeBlock(src, _) => src.clone(),
            ParseError::ReturnInstructionOutsideFunction(src) => src.clone(),
            ParseError::TooManyErrors(src) => src.clone(),
            ParseError::NoVariableAtCurrentScope(src) => src.clone(),
        }
    }
}

#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
pub struct ParseWarning {
    pub msg: String,
    pub src: SourceRef,
}

impl ParseWarning {
    pub fn get_warning_src(&self) -> SourceRef {
        self.src.clone()
    }
}
