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
    ConstantDeclarationNeedsInitValue(SourceRef),
    CannotParseAnExpression(SourceRef),
    TooManyFnArgs(SourceRef),
    TooManyFnParams(SourceRef),
    MalformedDeclaration(String, SourceRef),
    NoVariableAtTopLevel(SourceRef),
    NoCodeBlockAtTopLevel(SourceRef),
    NoLoopAtTopLevel(SourceRef),
    NoBreakOutsideLoop(SourceRef),
    NoContinueOutsideLoop(SourceRef),
    MisuseOfPubKeyword(SourceRef),
    UnterminatedCodeBlock(SourceRef, Option<String>),
    ReturnInstructionOutsideFunction(SourceRef),
    UnusualTokenInUsePath(SourceRef),
    UnterminatedUsePath(SourceRef),
    UnknownCompilerDirective(SourceRef),
    TooManyErrors(SourceRef),
}

#[allow(dead_code)]
#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
pub enum NameResolutionError {
    SymbolAlreadyExists(SourceRef),
    UndefinedSymbol(SourceRef),
}
