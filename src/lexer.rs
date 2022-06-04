use crate::common::{LexContext, ProtoErr};

#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum TokenKind {
    // Complex
    Identifier(String),
    String(String),
    Integer(i64),
    Boolean(bool),
    Char(char),

    // Singles
    Let,
    Mut,
    Equals,
    Plus,
    Minus,
    Star,
    Slash,
    Dot,
    Modulo,
    Comma,
    Colon,
    SemiColon,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    OpenBracket,
    CloseBracket,
    Record,
    Fn,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    And,
    Or,
    QuestionMark,
    For,
    If,
    While,
    Loop,
    Else,
    Break,
    Continue,
    Return,

    // Typei64,
    // TypeChar,
    // TypeStr,
    // TypeBool,
    End,
}

// Instead of cloning/copying the source to create
// a heaped string, we just keep track of where it is
// within the string instead (reduces overall allocations)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
    pub column: usize,
    pub line: usize,
}

impl Token {
    pub fn new(span: Span, kind: TokenKind, column: usize, line: usize) -> Self {
        Self {
            span,
            kind,
            column,
            line,
        }
    }
}

fn peek(ctx: &mut LexContext, by: usize) -> char {
    if ctx.current_ip + by >= ctx.source.len() {
        return '\0';
    }
    ctx.source.as_bytes()[ctx.current_ip + by].into()
}

fn is_at_end(ctx: &mut LexContext) -> bool {
    ctx.current_ip >= ctx.source.len()
}

fn current_char(ctx: &mut LexContext) -> char {
    peek(ctx, 0)
}

fn advance(ctx: &mut LexContext) {
    ctx.current_ip += 1;
    ctx.column += 1;
}

fn advance_line(ctx: &mut LexContext) {
    ctx.current_ip += 1;
    ctx.line += 1;
    ctx.column = 1;
}

fn skip_whitespace(ctx: &mut LexContext) {
    'outer: while !is_at_end(ctx) {
        match current_char(ctx) {
            ' ' | '\t' | '\r' => advance(ctx),
            '\n' => advance_line(ctx),

            // Comments
            '/' => {
                // single line comments
                if peek(ctx, 1) == '/' {
                    advance(ctx);
                    while !is_at_end(ctx) && current_char(ctx) != '\n' {
                        advance(ctx);
                    }
                } else if peek(ctx, 1) == '*' {
                    //multiline comment
                    advance(ctx); // advance to *
                    advance(ctx); // advance past *
                    while !is_at_end(ctx) {
                        let cur = current_char(ctx);
                        match cur {
                            '*' => {
                                if peek(ctx, 1) == '/' {
                                    // we have reached the end of a multiline comment
                                    advance(ctx); // advance to /
                                    advance(ctx); // advance past /
                                    break;
                                }
                            }
                            '\n' => advance_line(ctx),
                            _ => advance(ctx),
                        }
                    }
                } else {
                    return;
                }
            }
            _ => break 'outer,
        }
    }
}

fn find_keyword(ctx: &LexContext, span: Span) -> Option<TokenKind> {
    let lexeme = &ctx.source[span.start..span.end];

    match lexeme {
        "let" => Some(TokenKind::Let),
        "mut" => Some(TokenKind::Mut),
        "fn" => Some(TokenKind::Fn),
        "record" => Some(TokenKind::Record),
        "for" => Some(TokenKind::For),
        "if" => Some(TokenKind::If),
        "else" => Some(TokenKind::Else),
        "while" => Some(TokenKind::While),
        "loop" => Some(TokenKind::Loop),
        "return" => Some(TokenKind::Return),
        "break" => Some(TokenKind::Break),
        "continue" => Some(TokenKind::Continue),
        "true" | "false" => Some(TokenKind::Boolean(lexeme.parse().unwrap())),
        _ => None,
    }
}

fn find_single(ctx: &mut LexContext) -> Option<TokenKind> {
    let single = current_char(ctx);

    match single {
        '+' => Some(TokenKind::Plus),
        '-' => Some(TokenKind::Minus),
        '*' => Some(TokenKind::Star),
        '/' => Some(TokenKind::Slash),
        '=' => Some(TokenKind::Equals),
        '%' => Some(TokenKind::Modulo),

        '.' => Some(TokenKind::Dot),
        ',' => Some(TokenKind::Comma),
        ';' => Some(TokenKind::SemiColon),

        '{' => Some(TokenKind::OpenCurly),
        '}' => Some(TokenKind::CloseCurly),
        '(' => Some(TokenKind::OpenParen),
        ')' => Some(TokenKind::CloseParen),
        ':' => Some(TokenKind::Colon),
        '[' => Some(TokenKind::OpenBracket),
        ']' => Some(TokenKind::CloseBracket),
        '?' => Some(TokenKind::QuestionMark),

        // check if these are double operands
        '<' => {
            if peek(ctx, 1) != '=' {
                Some(TokenKind::LessThan)
            } else {
                None
            }
        }
        '>' => {
            if peek(ctx, 1) != '=' {
                Some(TokenKind::GreaterThan)
            } else {
                None
            }
        }
        _ => None,
    }
}

fn find_double(ctx: &mut LexContext) -> Option<TokenKind> {
    let cur = current_char(ctx);

    match cur {
        '<' => {
            if peek(ctx, 1) == '=' {
                advance(ctx);
                Some(TokenKind::LessThanOrEqual)
            } else {
                None
            }
        }
        '>' => {
            if peek(ctx, 1) == '=' {
                advance(ctx);
                Some(TokenKind::GreaterThanOrEqual)
            } else {
                None
            }
        }
        '&' => {
            if peek(ctx, 1) == '&' {
                advance(ctx);
                Some(TokenKind::And)
            } else {
                None
            }
        }
        '|' => {
            if peek(ctx, 1) == '|' {
                advance(ctx);
                Some(TokenKind::Or)
            } else {
                None
            }
        }
        _ => None,
    }
}

fn is_identifier(current: char) -> bool {
    char::is_alphabetic(current) || current == '_'
}

fn make_token(ctx: &LexContext, span: Span, kind: TokenKind, column: usize) -> Token {
    Token::new(span, kind, column, ctx.line)
}

fn identifier(ctx: &mut LexContext) -> Token {
    let start = ctx.current_ip;
    let start_col = ctx.column;

    while !is_at_end(ctx) && is_identifier(current_char(ctx)) {
        advance(ctx);
    }

    let span = Span {
        start,
        end: ctx.current_ip,
    };
    let keyword = find_keyword(ctx, span);

    make_token(
        ctx,
        span,
        if let Some(word) = keyword {
            word
        } else {
            TokenKind::Identifier(ctx.source[start..ctx.current_ip].into())
        },
        start_col,
    )
}

fn number(ctx: &mut LexContext) -> Result<Token, ProtoErr> {
    let start = ctx.current_ip;
    let start_col = ctx.column;
    // let mut is_float = false;

    while ctx.current_ip < ctx.source.len()
        && char::is_digit(ctx.source.as_bytes()[ctx.current_ip] as char, 10)
    {
        advance(ctx);

        // useful for when we add in Floats or Doubles
        // if ctx.source.as_bytes()[ctx.current_ip] == b'.' {
        //     if is_float {
        //         return Err(ProtoErr {
        //             msg: String::from("Multiple decimals found in floating point"),
        //             token: None,
        //         });
        //     }

        //     is_float = true;
        // }
    }

    let lexeme = &ctx.source[start..ctx.current_ip];

    Ok(make_token(
        ctx,
        Span {
            start,
            end: ctx.current_ip,
        },
        TokenKind::Integer(lexeme.parse().unwrap()),
        start_col,
    ))
}

fn string(ctx: &mut LexContext) -> Result<Token, ProtoErr> {
    let start = ctx.current_ip;
    let start_col = ctx.column;

    advance(ctx);

    while !is_at_end(ctx) && current_char(ctx) != '"' {
        advance(ctx);
    }

    // Unterminated string
    if is_at_end(ctx) {
        return Err(ProtoErr {
            msg: String::from("Unterminated string"),
            token: None,
        });
    }

    advance(ctx);

    Ok(make_token(
        ctx,
        Span {
            start,
            end: ctx.current_ip,
        },
        TokenKind::String(ctx.source[start..ctx.current_ip].into()),
        start_col,
    ))
}

fn character(ctx: &mut LexContext) -> Result<Token, ProtoErr> {
    let start = ctx.current_ip;
    let start_col = ctx.column;

    // if advancing once or twice will be in EOF
    if ctx.current_ip + 1 >= ctx.source.len() || ctx.current_ip + 2 >= ctx.source.len() {
        return Err(ProtoErr {
            msg: String::from("Unterminated character"),
            token: None,
        });
    }
    advance(ctx); // advance past ' to actual character
    let ch = current_char(ctx);
    advance(ctx); // advance to closing '

    // current character should be the closing '
    if current_char(ctx) != '\'' {
        return Err(ProtoErr {
            msg: String::from("A character should be of length 1"),
            token: None,
        });
    }

    advance(ctx);
    Ok(make_token(
        ctx,
        Span {
            start,
            end: ctx.current_ip,
        },
        TokenKind::Char(ch),
        start_col,
    ))
}

pub fn lex_next(ctx: &mut LexContext) -> Result<Token, ProtoErr> {
    skip_whitespace(ctx);

    // End of the stream
    if is_at_end(ctx) {
        return Ok(make_token(
            ctx,
            Span {
                start: ctx.current_ip,
                end: ctx.current_ip,
            },
            TokenKind::End,
            ctx.column,
        ));
    }

    let current = current_char(ctx);

    if is_identifier(current) {
        return Ok(identifier(ctx));
    }

    if char::is_digit(current, 10) {
        return number(ctx);
    }

    if current == '"' {
        return string(ctx);
    }

    if current == '\'' {
        return character(ctx);
    }

    if let Some(kind) = find_single(ctx) {
        advance(ctx);

        return Ok(make_token(
            ctx,
            Span {
                start: ctx.current_ip - 1,
                end: ctx.current_ip,
            },
            kind,
            ctx.column - 1,
        ));
    } else if let Some(kind) = find_double(ctx) {
        // it was a double operand
        advance(ctx);

        return Ok(make_token(
            ctx,
            Span {
                start: ctx.current_ip - 2,
                end: ctx.current_ip,
            },
            kind,
            ctx.column - 2,
        ));
    }

    // Unknown item found
    Err(ProtoErr {
        msg: format!(
            "Unknown character found: '{}'",
            ctx.source.as_bytes()[ctx.current_ip]
        ),
        token: None,
    })
}
