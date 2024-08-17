use std::rc::Rc;

use crate::source::source::{SourceFile, SourceRef};

#[allow(dead_code, unused)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenType {
    // keywords
    Fn,
    If,
    Else,
    For,
    In,
    Break,
    Continue,
    True,
    False,
    Return,
    Struct,
    Print,
    Println,
    None,
    Defer,
    Const,

    // operators
    BackSlash,
    Plus,
    Minus,
    Star,
    Slash,
    Modulo,
    PlusAssign,
    MinusAssign,
    StarAssign,
    SlashAssign,
    ModuloAssign,
    Ampersand,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Assign,
    And,
    Or,
    Not,

    // special characters
    At,

    // delimiters
    LParen,
    RParen,
    LCurly,
    RCurly,
    LBracket,
    RBracket,
    Semicolon,
    Colon,
    Comma,
    Dot,
    BackTick,

    // complex tokens
    Integer,
    Decimal,
    Identifier,
    Character,
    String,
    InterpolatedStringFragment,
    Comment,

    // type tags
    I8,
    I16,
    I32,
    I64,
    Int,
    U8,
    U16,
    U32,
    U64,
    UInt,
    F32,
    F64,
    Bool,
    Void,
    Char,
    Str,
    Type,

    // misc
    Underscore,
    QuestionMark,
    Eof,
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct X {
    ty: TokenType,
    loc: Rc<SourceRef>,
}

#[allow(unused)]
impl X {
    pub fn as_str(self: &X, src: &SourceFile) -> String {
        match self.ty {
            TokenType::Fn => "fn".to_string(),
            TokenType::If => "if".to_string(),
            TokenType::Else => "else".to_string(),
            TokenType::For => "for".to_string(),
            TokenType::In => "in".to_string(),
            TokenType::Break => "break".to_string(),
            TokenType::Continue => "continue".to_string(),
            TokenType::True => "true".to_string(),
            TokenType::False => "false".to_string(),
            TokenType::Return => "return".to_string(),
            TokenType::Struct => "struct".to_string(),
            TokenType::Print => "print".to_string(),
            TokenType::Println => "println".to_string(),
            TokenType::None => "none".to_string(),
            TokenType::Defer => "defer".to_string(),
            TokenType::Const => "const".to_string(),
            TokenType::BackSlash => "\\".to_string(),
            TokenType::Plus => "+".to_string(),
            TokenType::Minus => "-".to_string(),
            TokenType::Star => "*".to_string(),
            TokenType::Slash => "/".to_string(),
            TokenType::Modulo => "%".to_string(),
            TokenType::PlusAssign => "+=".to_string(),
            TokenType::MinusAssign => "-=".to_string(),
            TokenType::StarAssign => "*=".to_string(),
            TokenType::SlashAssign => "/=".to_string(),
            TokenType::ModuloAssign => "%=".to_string(),
            TokenType::Ampersand => "&".to_string(),
            TokenType::Equal => "==".to_string(),
            TokenType::NotEqual => "!=".to_string(),
            TokenType::Less => "<".to_string(),
            TokenType::LessEqual => "<=".to_string(),
            TokenType::Greater => ">".to_string(),
            TokenType::GreaterEqual => ">=".to_string(),
            TokenType::Assign => "=".to_string(),
            TokenType::And => "and".to_string(),
            TokenType::Or => "or".to_string(),
            TokenType::Not => "!".to_string(),
            TokenType::At => "@".to_string(),
            TokenType::LParen => "(".to_string(),
            TokenType::RParen => ")".to_string(),
            TokenType::LCurly => "{".to_string(),
            TokenType::RCurly => "}".to_string(),
            TokenType::LBracket => "[".to_string(),
            TokenType::RBracket => "]".to_string(),
            TokenType::Semicolon => ";".to_string(),
            TokenType::Colon => ":".to_string(),
            TokenType::Comma => ",".to_string(),
            TokenType::Dot => ".".to_string(),
            TokenType::BackTick => "`".to_string(),
            TokenType::Integer
            | TokenType::Decimal
            | TokenType::Identifier
            | TokenType::InterpolatedStringFragment
            | TokenType::Comment
            | TokenType::String
            | TokenType::Character => {
                let snippet = &src.text[self.loc.flat_start..self.loc.flat_end];
                snippet.to_string()
            }
            TokenType::I8 => "i8".to_string(),
            TokenType::I16 => "i16".to_string(),
            TokenType::I32 => "i32".to_string(),
            TokenType::I64 => "i64".to_string(),
            TokenType::Int => "int".to_string(),
            TokenType::U8 => "u8".to_string(),
            TokenType::U16 => "u16".to_string(),
            TokenType::U32 => "u32".to_string(),
            TokenType::U64 => "u64".to_string(),
            TokenType::UInt => "uint".to_string(),
            TokenType::F32 => "f32".to_string(),
            TokenType::F64 => "f64".to_string(),
            TokenType::Bool => "bool".to_string(),
            TokenType::Void => "void".to_string(),
            TokenType::Char => "char".to_string(),
            TokenType::Str => "str".to_string(),
            TokenType::Type => "type".to_string(),
            TokenType::Underscore => "_".to_string(),
            TokenType::QuestionMark => "?".to_string(),
            TokenType::Eof => "\0".to_string(),
        }
    }
}

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
    Const(Rc<SourceRef>),
    Trait(Rc<SourceRef>),
    Impl(Rc<SourceRef>),

    // operators
    BackSlash(Rc<SourceRef>),
    Plus(Rc<SourceRef>),
    Minus(Rc<SourceRef>),
    Star(Rc<SourceRef>),
    Slash(Rc<SourceRef>),
    Modulo(Rc<SourceRef>),
    PlusAssign(Rc<SourceRef>),
    MinusAssign(Rc<SourceRef>),
    StarAssign(Rc<SourceRef>),
    SlashAssign(Rc<SourceRef>),
    ModuloAssign(Rc<SourceRef>),
    Ampersand(Rc<SourceRef>),

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
    DecimalLiteral(String, Rc<SourceRef>),
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
    F32(Rc<SourceRef>),
    F64(Rc<SourceRef>),
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
            | Token::Const(src)
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
            | Token::DecimalLiteral(_, src)
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
            | Token::F32(src)
            | Token::F64(src)
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
            | Token::PlusAssign(src)
            | Token::MinusAssign(src)
            | Token::StarAssign(src)
            | Token::SlashAssign(src)
            | Token::ModuloAssign(src)
            | Token::BackSlash(src)
            | Token::Ampersand(src)
            | Token::Trait(src)
            | Token::Impl(src)
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
                | Token::Trait(_)
                | Token::Const(_)
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
            Token::DecimalLiteral(dec, _) => dec.to_string(),
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
            Token::F32(_) => "f32".into(),
            Token::F64(_) => "f64".into(),
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
            Token::PlusAssign(_) => "+=".to_string(),
            Token::MinusAssign(_) => "-=".to_string(),
            Token::StarAssign(_) => "*=".to_string(),
            Token::SlashAssign(_) => "/=".to_string(),
            Token::ModuloAssign(_) => "%=".to_string(),
            Token::BackSlash(_) => "\\".to_string(),
            Token::Ampersand(_) => "&".to_string(),
            Token::Const(_) => "const".to_string(),
            Token::Trait(_) => "trait".to_string(),
            Token::Impl(_) => "impl".to_string(),
        }
    }
}
