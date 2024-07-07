use std::rc::Rc;

use crate::source::source::SourceRef;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Token {
    // keywords
    Fn(Rc<SourceRef>),
    Let(Rc<SourceRef>),
    Mut(Rc<SourceRef>),
    If(Rc<SourceRef>),
    Else(Rc<SourceRef>),
    For(Rc<SourceRef>),
    In(Rc<SourceRef>),
    Break(Rc<SourceRef>),
    Continue(Rc<SourceRef>),
    True(Rc<SourceRef>),
    False(Rc<SourceRef>),
    Pub(Rc<SourceRef>),
    Return(Rc<SourceRef>),
    Mod(Rc<SourceRef>),
    Struct(Rc<SourceRef>),
    Print(Rc<SourceRef>),
    Println(Rc<SourceRef>),
    Some(Rc<SourceRef>),
    None(Rc<SourceRef>),
    Defer(Rc<SourceRef>),

    // operators
    Plus(Rc<SourceRef>),
    Minus(Rc<SourceRef>),
    Star(Rc<SourceRef>),
    Slash(Rc<SourceRef>),
    Modulo(Rc<SourceRef>),

    // special characters
    At(Rc<SourceRef>),

    // comparison
    Equal(Rc<SourceRef>),
    NotEqual(Rc<SourceRef>),
    Less(Rc<SourceRef>),
    LessEqual(Rc<SourceRef>),
    Greater(Rc<SourceRef>),
    GreaterEqual(Rc<SourceRef>),

    // assignment
    Assign(Rc<SourceRef>),

    // logical
    And(Rc<SourceRef>),
    Or(Rc<SourceRef>),
    Not(Rc<SourceRef>),

    // delimiters
    LParen(Rc<SourceRef>),
    RParen(Rc<SourceRef>),
    LCurly(Rc<SourceRef>),
    RCurly(Rc<SourceRef>),
    LBracket(Rc<SourceRef>),
    RBracket(Rc<SourceRef>),
    Semicolon(Rc<SourceRef>),
    Colon(Rc<SourceRef>),
    Comma(Rc<SourceRef>),
    Dot(Rc<SourceRef>),
    BackTick(Rc<SourceRef>),

    // literals
    NumberLiteral(String, Rc<SourceRef>),
    Identifier(String, Rc<SourceRef>),
    CharLiteral(Rc<SourceRef>, char),
    SingleLineStringLiteral(Rc<SourceRef>, String),
    InterpStrLiteral(Rc<SourceRef>, String),
    MultiLineStringFragment(Rc<SourceRef>, String),
    SingleLineComment(Rc<SourceRef>, String),

    // type tags
    I8(Rc<SourceRef>),
    I16(Rc<SourceRef>),
    I32(Rc<SourceRef>),
    I64(Rc<SourceRef>),
    Int(Rc<SourceRef>),
    U8(Rc<SourceRef>),
    U16(Rc<SourceRef>),
    U32(Rc<SourceRef>),
    U64(Rc<SourceRef>),
    UInt(Rc<SourceRef>),
    Bool(Rc<SourceRef>),
    Void(Rc<SourceRef>),
    Char(Rc<SourceRef>),
    Str(Rc<SourceRef>),
    Type(Rc<SourceRef>),

    // misc
    Underscore(Rc<SourceRef>),
    QuestionMark(Rc<SourceRef>),
    Eof(Rc<SourceRef>),
}

#[allow(dead_code)]
impl Token {
    pub fn get_source_ref(&self) -> Rc<SourceRef> {
        match self {
            Token::SingleLineComment(src, _)
            | Token::Fn(src)
            | Token::Let(src)
            | Token::Mut(src)
            | Token::If(src)
            | Token::Else(src)
            | Token::For(src)
            | Token::Plus(src)
            | Token::Minus(src)
            | Token::Star(src)
            | Token::Slash(src)
            | Token::Modulo(src)
            | Token::Equal(src)
            | Token::NotEqual(src)
            | Token::Less(src)
            | Token::LessEqual(src)
            | Token::Greater(src)
            | Token::GreaterEqual(src)
            | Token::Assign(src)
            | Token::And(src)
            | Token::Or(src)
            | Token::Not(src)
            | Token::LParen(src)
            | Token::RParen(src)
            | Token::LCurly(src)
            | Token::RCurly(src)
            | Token::LBracket(src)
            | Token::RBracket(src)
            | Token::Semicolon(src)
            | Token::Comma(src)
            | Token::Eof(src)
            | Token::Void(src)
            | Token::True(src)
            | Token::False(src)
            | Token::CharLiteral(src, _)
            | Token::Identifier(_, src)
            | Token::Dot(src)
            | Token::Break(src)
            | Token::Continue(src)
            | Token::Pub(src)
            | Token::Return(src)
            | Token::At(src)
            | Token::Mod(src)
            | Token::SingleLineStringLiteral(src, _)
            | Token::MultiLineStringFragment(src, _)
            | Token::NumberLiteral(_, src)
            | Token::Struct(src)
            | Token::I8(src)
            | Token::I16(src)
            | Token::I32(src)
            | Token::I64(src)
            | Token::Int(src)
            | Token::U8(src)
            | Token::U16(src)
            | Token::U32(src)
            | Token::U64(src)
            | Token::UInt(src)
            | Token::Bool(src)
            | Token::Char(src)
            | Token::Str(src)
            | Token::Type(src)
            | Token::Colon(src)
            | Token::Underscore(src)
            | Token::QuestionMark(src)
            | Token::Print(src)
            | Token::BackTick(src)
            | Token::Some(src)
            | Token::None(src)
            | Token::InterpStrLiteral(src, _)
            | Token::Defer(src)
            | Token::In(src)
            | Token::Println(src) => src.clone(),
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
                | Token::For(_)
                | Token::Pub(_)
                | Token::Return(_)
                | Token::Break(_)
                | Token::Continue(_)
                | Token::If(_)
                | Token::Struct(_)
                | Token::At(_)
                | Token::SingleLineComment(_, _)
                | Token::Fn(_)
        )
    }

    pub fn as_str(&self) -> String {
        match self {
            Token::Fn(_) => "fn".into(),
            Token::Let(_) => "let".into(),
            Token::Mut(_) => "mut".into(),
            Token::If(_) => "if".into(),
            Token::Else(_) => "else".into(),
            Token::For(_) => "for".into(),
            Token::Break(_) => "break".into(),
            Token::Continue(_) => "continue".into(),
            Token::Void(_) => "void".into(),
            Token::True(_) => "true".into(),
            Token::False(_) => "false".into(),
            Token::Mod(_) => "mod".into(),
            Token::CharLiteral(_, c) => format!("'{c}'"),
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
            Token::And(_) => "&&".into(),
            Token::Or(_) => "||".into(),
            Token::Not(_) => "!".into(),
            Token::LParen(_) => "(".into(),
            Token::RParen(_) => ")".into(),
            Token::LCurly(_) => "{".into(),
            Token::RCurly(_) => "}".into(),
            Token::LBracket(_) => "[".into(),
            Token::RBracket(_) => "]".into(),
            Token::Semicolon(_) => ";".into(),
            Token::Comma(_) => ",".into(),
            Token::Dot(_) => ".".into(),
            Token::Identifier(name, _) => name.clone(),
            Token::NumberLiteral(num, _) => num.to_string(),
            Token::I8(_) => "i8".into(),
            Token::I16(_) => "i16".into(),
            Token::I32(_) => "i32".into(),
            Token::I64(_) => "i64".into(),
            Token::Int(_) => "int".into(),
            Token::U8(_) => "u8".into(),
            Token::U16(_) => "u16".into(),
            Token::U32(_) => "u32".into(),
            Token::U64(_) => "u64".into(),
            Token::UInt(_) => "uint".into(),
            Token::Bool(_) => "bool".into(),
            Token::Char(_) => "char".into(),
            Token::Str(_) => "str".into(),
            Token::Eof(_) => "\0".into(),
            Token::Pub(_) => "pub".into(),
            Token::Return(_) => "return".into(),
            Token::At(_) => "@".into(),
            Token::SingleLineComment(_, src) => src.clone(),
            Token::Type(_) => "type".into(),
            Token::Struct(_) => "struct".into(),
            Token::Colon(_) => ":".into(),
            Token::SingleLineStringLiteral(_, content) => format!("\"{content}\""),
            Token::InterpStrLiteral(_, content) => content.clone(),
            Token::MultiLineStringFragment(_, mls) => mls.clone(),
            Token::Underscore(_) => '_'.to_string(),
            Token::QuestionMark(_) => '?'.to_string(),
            Token::Print(_) => "print".to_string(),
            Token::Println(_) => "println".to_string(),
            Token::BackTick(_) => '`'.to_string(),
            Token::Some(_) => "some".to_string(),
            Token::Defer(_) => "defer".to_string(),
            Token::None(_) => "none".to_string(),
            Token::In(_) => "in".to_string(),
        }
    }
}
