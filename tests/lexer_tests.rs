use proto::{
    common::{LexContext, ProtoErr},
    lexer::{lex_next, TokenKind},
};
use std::fs;

#[test]
fn test_lexer_tokens() -> Result<(), ProtoErr> {
    let path = "./samples/test_sources/lexer/valid/all_tokens.pr";
    let mut ctx = LexContext::new(fs::read_to_string(path).unwrap());

    loop {
        if lex_next(&mut ctx)?.kind == TokenKind::End {
            break;
        }
    }

    Ok(())
}

#[test]
fn test_lexing_types() -> Result<(), ProtoErr> {
    let path = "./samples/test_sources/lexer/valid/type_tokens.pr";
    let src = fs::read_to_string(path).unwrap();
    let mut ctx = LexContext::new(src);
    let token_kinds = vec![
        TokenKind::Typei64,
        TokenKind::TypeChar,
        TokenKind::TypeStr,
        TokenKind::TypeBool,
    ];

    for expected in token_kinds {
        let token = lex_next(&mut ctx)?;
        assert_eq!(expected, token.kind);
    }

    assert_eq!(lex_next(&mut ctx)?.kind, TokenKind::End);
    Ok(())
}

#[test]
fn test_lexing_grouping_tokens() -> Result<(), ProtoErr> {
    let path = "./samples/test_sources/lexer/valid/grouping_tokens.pr";
    let src = fs::read_to_string(path).unwrap();
    let mut ctx = LexContext::new(src);
    let token_kinds = vec![
        TokenKind::OpenParen,
        TokenKind::CloseParen,
        TokenKind::OpenCurly,
        TokenKind::CloseCurly,
        TokenKind::OpenBracket,
        TokenKind::CloseBracket,
    ];

    for expected in token_kinds {
        let token = lex_next(&mut ctx)?;
        assert_eq!(expected, token.kind);
    }

    assert_eq!(lex_next(&mut ctx)?.kind, TokenKind::End);
    Ok(())
}

#[test]
fn test_lexing_keywords() -> Result<(), ProtoErr> {
    let path = "./samples/test_sources/lexer/valid/keyword_tokens.pr";
    let src = fs::read_to_string(path).unwrap();
    let mut ctx = LexContext::new(src);
    let token_kinds = vec![
        TokenKind::Let,
        TokenKind::Mut,
        TokenKind::Struct,
        TokenKind::Fn,
        TokenKind::For,
        TokenKind::If,
        TokenKind::Loop,
        TokenKind::While,
        TokenKind::Else,
        TokenKind::Break,
        TokenKind::Continue,
        TokenKind::Return,
        TokenKind::Not,
    ];

    for expected in token_kinds {
        let token = lex_next(&mut ctx)?;
        assert_eq!(expected, token.kind);
    }

    assert_eq!(lex_next(&mut ctx)?.kind, TokenKind::End);
    Ok(())
}

#[test]
fn test_lexing_valid_identifiers() -> Result<(), ProtoErr> {
    let path = "./samples/test_sources/lexer/valid/identifier_tokens.pr";
    let src = fs::read_to_string(path).unwrap();
    let mut ctx = LexContext::new(src);

    let strings = vec![
        "someidentifier",
        "another_identifier",
        "a",
        "c",
        "_d",
        "_identifier",
        "a1b2c3d4e5f6",
    ];

    for ident in strings {
        let token = lex_next(&mut ctx)?;
        assert_eq!(TokenKind::Identifier(ident.into()), token.kind);
    }

    assert_eq!(lex_next(&mut ctx)?.kind, TokenKind::End);
    Ok(())
}

#[test]
fn test_lexing_valid_string_literals() -> Result<(), ProtoErr> {
    let path = "./samples/test_sources/lexer/valid/string_literals.pr";
    let src = fs::read_to_string(path).unwrap();
    let mut ctx = LexContext::new(src);

    let strings = vec![
        "\"some string\"",
        "\"another string\"",
        "\"a\"",
        "\"eiou\"",
        "\"quote ' in string '\"",
        "\"escaped \\\" string\"",
        "\"another \\\"escaped \\\"string, yayy\\\"\"",
    ];

    for ident in strings {
        let token = lex_next(&mut ctx)?;
        assert_eq!(TokenKind::String(ident.into()), token.kind);
    }

    assert_eq!(lex_next(&mut ctx)?.kind, TokenKind::End);
    Ok(())
}

#[test]
fn test_lexing_single_operators() -> Result<(), ProtoErr> {
    let path = "./samples/test_sources/lexer/valid/single_operators.pr";
    let src = fs::read_to_string(path).unwrap();
    let mut ctx = LexContext::new(src);
    let token_kinds = vec![
        TokenKind::Equals,
        TokenKind::Plus,
        TokenKind::Minus,
        TokenKind::Star,
        TokenKind::Slash,
        TokenKind::Dot,
        TokenKind::Modulo,
        TokenKind::Comma,
        TokenKind::Colon,
        TokenKind::SemiColon,
        TokenKind::GreaterThan,
        TokenKind::LessThan,
        TokenKind::QuestionMark,
        TokenKind::Not,
    ];

    for expected in token_kinds {
        let token = lex_next(&mut ctx)?;
        assert_eq!(expected, token.kind);
    }

    assert_eq!(lex_next(&mut ctx)?.kind, TokenKind::End);
    Ok(())
}

#[test]
fn test_lexing_double_operators() -> Result<(), ProtoErr> {
    let path = "./samples/test_sources/lexer/valid/double_operators.pr";
    let src = fs::read_to_string(path).unwrap();
    let mut ctx = LexContext::new(src);
    let token_kinds = vec![
        TokenKind::LessThanOrEqual,
        TokenKind::GreaterThanOrEqual,
        TokenKind::And,
        TokenKind::Or,
        TokenKind::ComparisonEquals,
        TokenKind::ComparisonNotEquals,
    ];

    for expected in token_kinds {
        let token = lex_next(&mut ctx)?;
        assert_eq!(expected, token.kind);
    }

    assert_eq!(lex_next(&mut ctx)?.kind, TokenKind::End);
    Ok(())
}

#[test]
fn test_lexing_literal_tokens() -> Result<(), ProtoErr> {
    let path = "./samples/test_sources/lexer/valid/literal_tokens.pr";
    let src = fs::read_to_string(path).unwrap();
    let mut ctx = LexContext::new(src);

    // handle numbers first
    let numbers = vec![12345, 32100, 9223372036854775807];

    for num in numbers {
        let token = lex_next(&mut ctx)?;
        assert_eq!(TokenKind::Integer(num), token.kind);
    }

    // handle characters
    let chars = vec!['a', 'b', 'c', 'd', 'e', '"'];

    for ch in chars {
        let token = lex_next(&mut ctx)?;
        assert_eq!(TokenKind::Char(ch), token.kind);
    }

    // handle both boolean values
    assert_eq!(lex_next(&mut ctx)?.kind, TokenKind::Boolean(false));
    assert_eq!(lex_next(&mut ctx)?.kind, TokenKind::Boolean(true));

    assert_eq!(lex_next(&mut ctx)?.kind, TokenKind::End);
    Ok(())
}

#[test]
fn test_lexer_skipping_comments() -> Result<(), ProtoErr> {
    let path = "./samples/test_sources/lexer/valid/comments.pr";
    let src = fs::read_to_string(path).unwrap();
    let mut ctx = LexContext::new(src);

    let end = lex_next(&mut ctx)?;

    assert_eq!(end.kind, TokenKind::End);
    assert_eq!(end.line, 6);
    assert_eq!(end.column, 1);
    Ok(())
}
