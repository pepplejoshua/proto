use super::{source::SourceRef, types::Type};

#[allow(dead_code)]
#[derive(serde::Deserialize, serde::Serialize, Debug, Clone)]
pub enum Token {
    // keywords
    Fn(SourceRef),
    Let(SourceRef),
    Mut(SourceRef),
    If(SourceRef),
    Else(SourceRef),
    Loop(SourceRef),
    While(SourceRef),
    Break(SourceRef),
    Continue(SourceRef),
    Void(SourceRef),
    True(SourceRef),
    False(SourceRef),
    Use(SourceRef),
    Pub(SourceRef),
    Mod(SourceRef),
    Return(SourceRef),

    // operators
    Plus(SourceRef),
    Minus(SourceRef),
    Star(SourceRef),
    Slash(SourceRef),
    Modulo(SourceRef),

    // special operators
    As(SourceRef),

    // special characters
    Caret(SourceRef),
    Exclamation(SourceRef),
    Dollar(SourceRef),
    At(SourceRef),

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
    LCurly(SourceRef),
    RCurly(SourceRef),
    LBracket(SourceRef),
    RBracket(SourceRef),
    Semicolon(SourceRef),
    Colon(SourceRef),
    Scope(SourceRef),
    Comma(SourceRef),
    Dot(SourceRef),

    // literals
    Identifier(String, SourceRef),
    I8Literal(i8, SourceRef),
    I16Literal(i16, SourceRef),
    I32Literal(i32, SourceRef),
    I64Literal(i64, SourceRef),
    IsizeLiteral(isize, SourceRef),
    U8Literal(u8, SourceRef),
    U16Literal(u16, SourceRef),
    U32Literal(u32, SourceRef),
    U64Literal(u64, SourceRef),
    UsizeLiteral(usize, SourceRef),
    CharLiteral(SourceRef, char),
    StringLiteral(SourceRef, String),
    SingleLineComment(SourceRef, String),

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
    Str(SourceRef),

    // misc
    Eof(SourceRef),
}

#[allow(dead_code)]
impl Token {
    pub fn get_source_ref(&self) -> SourceRef {
        match self {
            Token::SingleLineComment(src, _) => src.clone(),
            Token::Fn(src) => src.clone(),
            Token::Let(src) => src.clone(),
            Token::Mut(src) => src.clone(),
            Token::If(src) => src.clone(),
            Token::Else(src) => src.clone(),
            Token::Loop(src) => src.clone(),
            Token::While(src) => src.clone(),
            Token::Plus(src) => src.clone(),
            Token::Minus(src) => src.clone(),
            Token::Star(src) => src.clone(),
            Token::Slash(src) => src.clone(),
            Token::Modulo(src) => src.clone(),
            Token::Equal(src) => src.clone(),
            Token::NotEqual(src) => src.clone(),
            Token::Less(src) => src.clone(),
            Token::LessEqual(src) => src.clone(),
            Token::Greater(src) => src.clone(),
            Token::GreaterEqual(src) => src.clone(),
            Token::Assign(src) => src.clone(),
            Token::And(src) => src.clone(),
            Token::Or(src) => src.clone(),
            Token::Not(src) => src.clone(),
            Token::LParen(src) => src.clone(),
            Token::RParen(src) => src.clone(),
            Token::LCurly(src) => src.clone(),
            Token::RCurly(src) => src.clone(),
            Token::LBracket(src) => src.clone(),
            Token::RBracket(src) => src.clone(),
            Token::Semicolon(src) => src.clone(),
            Token::Colon(src) => src.clone(),
            Token::Comma(src) => src.clone(),
            Token::Eof(src) => src.clone(),
            Token::Void(src) => src.clone(),
            Token::True(src) => src.clone(),
            Token::False(src) => src.clone(),
            Token::CharLiteral(src, _) => src.clone(),
            Token::Identifier(_, src) => src.clone(),
            Token::I8(src) => src.clone(),
            Token::I16(src) => src.clone(),
            Token::I32(src) => src.clone(),
            Token::I64(src) => src.clone(),
            Token::Isize(src) => src.clone(),
            Token::U8(src) => src.clone(),
            Token::U16(src) => src.clone(),
            Token::U32(src) => src.clone(),
            Token::U64(src) => src.clone(),
            Token::Usize(src) => src.clone(),
            Token::Bool(src) => src.clone(),
            Token::Char(src) => src.clone(),
            Token::Str(src) => src.clone(),
            Token::I8Literal(_, src) => src.clone(),
            Token::I16Literal(_, src) => src.clone(),
            Token::I32Literal(_, src) => src.clone(),
            Token::I64Literal(_, src) => src.clone(),
            Token::IsizeLiteral(_, src) => src.clone(),
            Token::U8Literal(_, src) => src.clone(),
            Token::U16Literal(_, src) => src.clone(),
            Token::U32Literal(_, src) => src.clone(),
            Token::U64Literal(_, src) => src.clone(),
            Token::UsizeLiteral(_, src) => src.clone(),
            Token::Dot(src) => src.clone(),
            Token::Use(src) => src.clone(),
            Token::Break(src) => src.clone(),
            Token::Continue(src) => src.clone(),
            Token::Pub(src) => src.clone(),
            Token::Mod(src) => src.clone(),
            Token::Return(src) => src.clone(),
            Token::Scope(src) => src.clone(),
            Token::Caret(src) => src.clone(),
            Token::Exclamation(src) => src.clone(),
            Token::Dollar(src) => src.clone(),
            Token::At(src) => src.clone(),
            Token::As(src) => src.clone(),
            Token::StringLiteral(src, _) => src.clone(),
        }
    }

    pub fn is_terminator(&self) -> bool {
        matches!(self, Token::Semicolon(_) | Token::RCurly(_) | Token::Eof(_))
    }

    pub fn begins_instruction(&self) -> bool {
        matches!(
            self,
            Token::Let(_)
                | Token::LCurly(_)
                | Token::Loop(_)
                | Token::While(_)
                | Token::Use(_)
                | Token::Mod(_)
                | Token::Pub(_)
                | Token::At(_)
                | Token::SingleLineComment(_, _)
        )
    }

    pub fn is_type_token(&self) -> bool {
        matches!(
            self,
            Token::I8(_)
                | Token::I16(_)
                | Token::I32(_)
                | Token::I64(_)
                | Token::Isize(_)
                | Token::U8(_)
                | Token::U16(_)
                | Token::U32(_)
                | Token::U64(_)
                | Token::Usize(_)
                | Token::Bool(_)
                | Token::Char(_)
                | Token::Void(_)
                | Token::Str(_)
        )
    }

    pub fn to_type(&self) -> Type {
        match self {
            Token::I8(_) => Type::I8,
            Token::I16(_) => Type::I16,
            Token::I32(_) => Type::I32,
            Token::I64(_) => Type::I64,
            Token::Isize(_) => Type::ISize,
            Token::U8(_) => Type::U8,
            Token::U16(_) => Type::U16,
            Token::U32(_) => Type::U32,
            Token::U64(_) => Type::U64,
            Token::Usize(_) => Type::USize,
            Token::Bool(_) => Type::Bool,
            Token::Char(_) => Type::Char,
            Token::Void(_) => Type::Void,
            Token::Str(_) => Type::Str,
            _ => unreachable!("to_type() called on unexpected Token, {self:?}"),
        }
    }

    pub fn as_str(&self) -> String {
        match self {
            Token::Fn(_) => "fn".into(),
            Token::Let(_) => "let".into(),
            Token::Mut(_) => "mut".into(),
            Token::If(_) => "if".into(),
            Token::Else(_) => "else".into(),
            Token::Loop(_) => "loop".into(),
            Token::While(_) => "while".into(),
            Token::Break(_) => "break".into(),
            Token::Continue(_) => "continue".into(),
            Token::Void(_) => "void".into(),
            Token::True(_) => "true".into(),
            Token::False(_) => "false".into(),
            Token::CharLiteral(_, c) => format!("'{c}'"),
            Token::Use(_) => "use".into(),
            Token::Plus(_) => "+".into(),
            Token::Minus(_) => "-".into(),
            Token::Star(_) => "*".into(),
            Token::Slash(_) => "/".into(),
            Token::Modulo(_) => "%".into(),
            Token::Equal(_) => "==".into(),
            Token::NotEqual(_) => "!=".into(),
            Token::Less(_) => "<".into(),
            Token::LessEqual(_) => "<=".into(),
            Token::Greater(_) => ">".into(),
            Token::GreaterEqual(_) => ">=".into(),
            Token::Assign(_) => "=".into(),
            Token::And(_) => "and".into(),
            Token::Or(_) => "or".into(),
            Token::Not(_) => "not".into(),
            Token::LParen(_) => "(".into(),
            Token::RParen(_) => ")".into(),
            Token::LCurly(_) => "{".into(),
            Token::RCurly(_) => "}".into(),
            Token::LBracket(_) => "[".into(),
            Token::RBracket(_) => "]".into(),
            Token::Semicolon(_) => ";".into(),
            Token::Colon(_) => ":".into(),
            Token::Comma(_) => ",".into(),
            Token::Dot(_) => ".".into(),
            Token::Identifier(name, _) => name.clone(),
            Token::I8Literal(num, _) => num.to_string(),
            Token::I16Literal(num, _) => num.to_string(),
            Token::I32Literal(num, _) => num.to_string(),
            Token::I64Literal(num, _) => num.to_string(),
            Token::IsizeLiteral(num, _) => num.to_string(),
            Token::U8Literal(num, _) => num.to_string(),
            Token::U16Literal(num, _) => num.to_string(),
            Token::U32Literal(num, _) => num.to_string(),
            Token::U64Literal(num, _) => num.to_string(),
            Token::UsizeLiteral(num, _) => num.to_string(),
            Token::I8(_) => "i8".into(),
            Token::I16(_) => "i16".into(),
            Token::I32(_) => "i32".into(),
            Token::I64(_) => "i64".into(),
            Token::Isize(_) => "isize".into(),
            Token::U8(_) => "u8".into(),
            Token::U16(_) => "u16".into(),
            Token::U32(_) => "u32".into(),
            Token::U64(_) => "u64".into(),
            Token::Usize(_) => "usize".into(),
            Token::Bool(_) => "bool".into(),
            Token::Char(_) => "char".into(),
            Token::Str(_) => "str".into(),
            Token::Eof(_) => "\0".into(),
            Token::Pub(_) => "pub".into(),
            Token::Mod(_) => "mod".into(),
            Token::Return(_) => "return".into(),
            Token::Scope(_) => "::".into(),
            Token::Caret(_) => "^".into(),
            Token::Exclamation(_) => "!".into(),
            Token::Dollar(_) => "$".into(),
            Token::At(_) => "@".into(),
            Token::As(_) => "as".into(),
            Token::StringLiteral(_, src) => format!("\"{src}\""),
            Token::SingleLineComment(_, src) => src.clone(),
        }
    }
}
