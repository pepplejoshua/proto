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
    NoVariableAtTopLevel(SourceRef),
    NoCodeBlockAtTopLevel(SourceRef),
    MisuseOfPubKeyword(SourceRef),
    UnterminatedCodeBlock(SourceRef, Option<String>),
}

// for:
// - CannotParseAnExpression: provide more ParseScopes to allow this work better.
//   I can use the context to provide a better skip_to() token
//   Perhaps, it can halt on this error?
