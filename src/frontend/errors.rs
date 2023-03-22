use super::source::SourceRef;

#[allow(dead_code)]
#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
pub enum LexError {
    InvalidCharacter(SourceRef),
    CannotMakeSignedNumber(SourceRef),
    CannotMakeUnsignedNumber(SourceRef),
}

#[allow(dead_code)]
#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
pub enum ParseError {
    Expected(String, SourceRef, Option<String>),
    ConstantDeclarationNeedsInitValue(SourceRef),
    CannotParseAnExpression(SourceRef),
    TooManyFnArgs(SourceRef),
    TooManyFnParams(SourceRef),
    MalformedDeclaration(String, SourceRef),
    NoVariableAtTopLevel(SourceRef, Option<String>),
    MisuseOfPubKeyword(String, SourceRef),
}
