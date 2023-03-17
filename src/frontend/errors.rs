use super::source::SourceRef;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum LexerError {
    InvalidCharacter(SourceRef),
    CannotMakeSignedNumber(SourceRef),
    CannotMakeUnsignedNumber(SourceRef),
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum ParserError {
    Expected(String, SourceRef, Option<String>),
    ConstantDeclarationNeedsInitValue(SourceRef),
    CannotParseAnExpression(SourceRef),
    TooManyFnArgs(SourceRef),
}
