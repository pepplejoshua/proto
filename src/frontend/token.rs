use super::source::SourceRef;

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
    Pub(SourceRef),
    Return(SourceRef),
    Comp(SourceRef),
    Struct(SourceRef),

    // operators
    Plus(SourceRef),
    Minus(SourceRef),
    Star(SourceRef),
    Slash(SourceRef),
    Modulo(SourceRef),

    // special characters
    Exclamation(SourceRef),
    QuestionMark(SourceRef),
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
    Comma(SourceRef),
    Dot(SourceRef),

    // literals
    Identifier(String, SourceRef),
    Integer(String, SourceRef),
    CharLiteral(SourceRef, char),
    SingleLineStringLiteral(SourceRef, String),
    MultiLineStringFragment(SourceRef, String),
    SingleLineComment(SourceRef, String),

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
            Token::Comma(src) => src.clone(),
            Token::Eof(src) => src.clone(),
            Token::Void(src) => src.clone(),
            Token::True(src) => src.clone(),
            Token::False(src) => src.clone(),
            Token::CharLiteral(src, _) => src.clone(),
            Token::Identifier(_, src) => src.clone(),
            Token::Dot(src) => src.clone(),
            Token::Break(src) => src.clone(),
            Token::Continue(src) => src.clone(),
            Token::Pub(src) => src.clone(),
            Token::Return(src) => src.clone(),
            Token::Exclamation(src) => src.clone(),
            Token::At(src) => src.clone(),
            Token::SingleLineStringLiteral(src, _) => src.clone(),
            Token::MultiLineStringFragment(src, _) => src.clone(),
            Token::Integer(_, src) => src.clone(),
            Token::Comp(src) => src.clone(),
            Token::Struct(src) => src.clone(),
            Token::QuestionMark(src) => src.clone(),
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
            Token::Loop(_) => "loop".into(),
            Token::While(_) => "while".into(),
            Token::Break(_) => "break".into(),
            Token::Continue(_) => "continue".into(),
            Token::Void(_) => "void".into(),
            Token::True(_) => "true".into(),
            Token::False(_) => "false".into(),
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
            Token::Comma(_) => ",".into(),
            Token::Dot(_) => ".".into(),
            Token::Identifier(name, _) => name.clone(),
            Token::Integer(num, _) => num.to_string(),
            Token::Eof(_) => "\0".into(),
            Token::Pub(_) => "pub".into(),
            Token::Return(_) => "return".into(),
            Token::Exclamation(_) => "!".into(),
            Token::At(_) => "@".into(),
            Token::SingleLineComment(_, src) => src.clone(),
            Token::SingleLineStringLiteral(_, content) => format!("\"{content}\""),
            Token::MultiLineStringFragment(_, content) => format!("||{content}"),
            Token::Comp(_) => "comp".into(),
            Token::Struct(_) => "struct".into(),
            Token::QuestionMark(_) => "?".into(),
        }
    }
}
