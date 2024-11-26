use std::rc::Rc;

use crate::source::source::{SourceFile, SourceRef};

#[allow(dead_code, unused)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenType {
    // keywords
    Pub,
    Fn,
    Var,
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
    Some,
    None,
    Defer,
    Const,
    Comptime,
    Use,
    Module,
    As,
    Root,

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
    DoubleColon,
    ColonAssign,
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
    U8,
    U16,
    U32,
    U64,
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
pub struct SrcToken {
    pub ty: TokenType,
    pub loc: Rc<SourceRef>,
}

#[allow(unused)]
impl SrcToken {
    pub fn get_source_ref(self: &SrcToken) -> Rc<SourceRef> {
        return self.loc.clone();
    }

    pub fn as_str(self: &SrcToken, src: &SourceFile) -> String {
        match self.ty {
            TokenType::As => "as".to_string(),
            TokenType::Module => "module".to_string(),
            TokenType::Use => "use".to_string(),
            TokenType::Pub => "pub".to_string(),
            TokenType::Comptime => "comptime".to_string(),
            TokenType::Fn => "fn".to_string(),
            TokenType::Const => "const".to_string(),
            TokenType::Var => "var".to_string(),
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
            TokenType::Some => "some".to_string(),
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
            TokenType::DoubleColon => "::".to_string(),
            TokenType::ColonAssign => ":=".to_string(),
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
            TokenType::U8 => "u8".to_string(),
            TokenType::U16 => "u16".to_string(),
            TokenType::U32 => "u32".to_string(),
            TokenType::U64 => "u64".to_string(),
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
