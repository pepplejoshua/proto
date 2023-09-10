use super::source::SourceRef;

#[allow(dead_code)]
#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
pub enum LexError {
    InvalidCharacter(SourceRef),
    CannotMakeSignedNumber(SourceRef),
    CannotMakeUnsignedNumber(SourceRef),
    EmptyCharacterLiteral(SourceRef),
    UnterminatedCharacterLiteral(SourceRef),
    UnterminatedStringLiteral(SourceRef),
}

#[allow(dead_code)]
#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
pub enum ParseError {
    Expected(String, SourceRef, Option<String>),
    ConstantDeclarationNeedsTypeOrInitValue(SourceRef),
    CannotParseAnExpression(SourceRef),
    TooManyFnArgs(SourceRef),
    TooManyFnParams(SourceRef),
    MalformedDeclaration(String, SourceRef),
    NoVariableAtCurrentScope(SourceRef),
    NoCodeBlockAllowedInCurrentContext(SourceRef),
    NoLoopAtTopLevel(SourceRef),
    NoBreakOutsideLoop(SourceRef),
    NoContinueOutsideLoop(SourceRef),
    MisuseOfPubKeyword(SourceRef),
    UnterminatedCodeBlock(SourceRef, Option<String>),
    ReturnInstructionOutsideFunction(SourceRef),
    UnknownCompilerDirective(SourceRef),
    TooManyErrors(SourceRef),
    CannotParseType(SourceRef, Option<String>),
    TooManyTypes(SourceRef),
}
