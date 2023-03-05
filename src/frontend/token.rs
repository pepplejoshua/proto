use super::source::SourceRef;

pub enum Token {
    // keywords
    Fn(SourceRef),
    Let(SourceRef),
    Mut(SourceRef),
    If(SourceRef),
    Else(SourceRef),
    Loop(SourceRef),
    While(SourceRef),
    Void(SourceRef),
    True(SourceRef),
    False(SourceRef),
    Character(SourceRef, char),

    // operators
    Plus(SourceRef),
    Minus(SourceRef),
    Star(SourceRef),
    Slash(SourceRef),
    Modulo(SourceRef),

    // comparison
    Equal(SourceRef),
    NotEqual(SourceRef),
    Less(SourceRef),
    LessEqual(SourceRef),
    Greater(SourceRef),
    GreaterEqual(SourceRef),

    // assignment
    Assign(SourceRef),

    // logical
    And(SourceRef),
    Or(SourceRef),
    Not(SourceRef),

    // delimiters
    LParen(SourceRef),
    RParen(SourceRef),
    LBrace(SourceRef),
    RBrace(SourceRef),
    LBracket(SourceRef),
    RBracket(SourceRef),
    Semicolon(SourceRef),
    Colon(SourceRef),
    Comma(SourceRef),

    // literals
    Identifier(String, SourceRef),
    SignedI8(i8, SourceRef),
    UnsignedI8(u8, SourceRef),
    SignedI64(i64, SourceRef),
    UnsignedI64(u64, SourceRef),
    SignedSize(isize, SourceRef),
    UnsignedSize(usize, SourceRef),

    // primitive types
    I8(SourceRef),
    I16(SourceRef),
    I32(SourceRef),
    I64(SourceRef),
    Isize(SourceRef),
    U8(SourceRef),
    U16(SourceRef),
    U32(SourceRef),
    U64(SourceRef),
    Usize(SourceRef),
    Bool(SourceRef),
    Char(SourceRef),

    // misc
    Eof(SourceRef),
}

impl Token {
    pub fn get_source_ref(&self) -> &SourceRef {
        match self {
            Token::Fn(src) => src,
            Token::Let(src) => src,
            Token::Mut(src) => src,
            Token::If(src) => src,
            Token::Else(src) => src,
            Token::Loop(src) => src,
            Token::While(src) => src,
            Token::Plus(src) => src,
            Token::Minus(src) => src,
            Token::Star(src) => src,
            Token::Slash(src) => src,
            Token::Modulo(src) => src,
            Token::Equal(src) => src,
            Token::NotEqual(src) => src,
            Token::Less(src) => src,
            Token::LessEqual(src) => src,
            Token::Greater(src) => src,
            Token::GreaterEqual(src) => src,
            Token::Assign(src) => src,
            Token::And(src) => src,
            Token::Or(src) => src,
            Token::Not(src) => src,
            Token::LParen(src) => src,
            Token::RParen(src) => src,
            Token::LBrace(src) => src,
            Token::RBrace(src) => src,
            Token::LBracket(src) => src,
            Token::RBracket(src) => src,
            Token::Semicolon(src) => src,
            Token::Colon(src) => src,
            Token::Comma(src) => src,
            Token::SignedI8(_, src) => src,
            Token::UnsignedI8(_, src) => src,
            Token::SignedI64(_, src) => src,
            Token::UnsignedI64(_, src) => src,
            Token::Eof(src) => src,
            Token::Void(src) => src,
            Token::True(src) => src,
            Token::False(src) => src,
            Token::Character(src, _) => src,
            Token::Identifier(_, src) => src,
            Token::SignedSize(_, src) => src,
            Token::UnsignedSize(_, src) => src,
            Token::I8(src) => src,
            Token::U8(src) => src,
            Token::I64(src) => src,
            Token::U64(src) => src,
            Token::Isize(src) => src,
            Token::Usize(src) => src,
            Token::Bool(src) => src,
            Token::Char(src) => src,
        }
    }
}
