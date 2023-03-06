use super::source::SourceRef;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum LexerError {
    InvalidCharacter(SourceRef),
    CannotMakeSignedNumber(SourceRef),
    CannotMakeUnsignedNumber(SourceRef),
}
