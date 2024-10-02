use super::token::{SrcToken, TokenType};
use crate::source::{errors::LexError, source::SourceFile};

#[allow(dead_code)]
pub struct Lexer {
    pub src: SourceFile,
    last_lexed: Option<SrcToken>,
    queue: Vec<SrcToken>,
}

#[allow(dead_code)]
impl Lexer {
    pub fn new(src: SourceFile) -> Lexer {
        Lexer {
            src,
            last_lexed: None,
            queue: vec![],
        }
    }

    // skip all whitespace characters
    fn skip_whitespace(&mut self) {
        loop {
            let c = self.src.cur_char();
            if c.is_whitespace() {
                self.src.next_char();
                continue;
            } else {
                break;
            }
        }
    }

    fn lex_comment(&mut self) -> Result<SrcToken, LexError> {
        let mut span = self.src.get_ref();
        // skip both '/' characters
        self.src.next_char();
        self.src.next_char();

        while !self.src.is_eof() {
            let cur = self.src.cur_char();
            span = span.combine(self.src.get_ref());
            self.src.next_char();
            if cur != '\n' {
                continue;
            } else {
                break;
            }
        }

        Ok(SrcToken {
            ty: TokenType::Comment,
            loc: span,
        })
    }

    // try to lex the next possible token
    pub fn next_token(&mut self) -> Result<SrcToken, LexError> {
        if !self.queue.is_empty() {
            let tok = self.queue.remove(0);
            self.last_lexed = Some(tok.clone());
            return Ok(tok);
        }

        while self.src.cur_char().is_whitespace() {
            // skip all preceding whitespace
            self.skip_whitespace();
        }

        // get the current character
        let c = self.src.cur_char();
        let c_ref = self.src.get_ref();

        // if we reached the end of the file, return EOF
        if c == '\0' {
            return Ok(SrcToken {
                ty: TokenType::Eof,
                loc: self.src.get_ref(),
            });
        }
        let maybe_token = match c {
            '`' => self.lex_interpolated_string(),
            // operators
            '+' | '-' | '*' | '/' | '%' | '!' | '=' | '<' | '>' | '(' | ')' | '{' | '}' | '['
            | ']' | ',' | '.' | ':' | ';' | '@' | '&' | '?' | '\\' => self.lex_operator(),
            // numbers
            _ if c.is_ascii_digit() => self.lex_number(),
            // identifiers | keywords
            _ if c.is_alphabetic() || c == '_' => self.lex_potential_identifier(),
            // ' is used to start characters
            '\'' => self.lex_char(),
            // " is used to start strings
            '"' => self.lex_string(),
            // invalid character
            _ => {
                self.src.next_char(); // try to keep lexing
                let err = Err(LexError::InvalidCharacter(
                    c_ref.combine(self.src.get_ref()),
                ));
                err
            }
        };

        match &maybe_token {
            Ok(t) => self.last_lexed = Some(t.clone()),
            Err(_) => self.last_lexed = None,
        }
        maybe_token
    }

    fn lex_interpolated_string(&mut self) -> Result<SrcToken, LexError> {
        let mut span = self.src.get_ref();
        self.src.next_char(); // consume initial backtick
        span = span.combine(self.src.get_ref());
        let bt = SrcToken {
            ty: TokenType::BackTick,
            loc: span.clone(),
        };
        let mut parts = vec![];

        let mut buf_span = self.src.get_ref();
        while !self.src.is_eof() {
            let c = self.src.cur_char();

            match c {
                '`' => {
                    span = self.src.get_ref();
                    self.src.next_char();
                    span = span.combine(self.src.get_ref());
                    if buf_span.flat_end - buf_span.flat_start > 1 {
                        parts.push(SrcToken {
                            ty: TokenType::InterpolatedStringFragment,
                            loc: buf_span.clone(),
                        });
                    }
                    parts.push(SrcToken {
                        ty: TokenType::BackTick,
                        loc: span.clone(),
                    });
                    break;
                }
                '{' => {
                    if self.src.peek_char() == '{' {
                        // we can escape the { character and add it to the buf
                        self.src.next_char();
                        self.src.next_char();
                        buf_span = buf_span.combine(self.src.get_ref());
                        continue;
                    }

                    // store whatever we have in buf and reset it
                    span = span.combine(self.src.get_ref());
                    if buf_span.flat_end - buf_span.flat_start > 1 {
                        parts.push(SrcToken {
                            ty: TokenType::InterpolatedStringFragment,
                            loc: buf_span.clone(),
                        });
                    }

                    let mut lcurly_span = self.src.get_ref();
                    self.src.next_char();
                    lcurly_span = lcurly_span.combine(self.src.get_ref());
                    let lcurly = SrcToken {
                        ty: TokenType::LCurly,
                        loc: lcurly_span,
                    };
                    parts.push(lcurly);

                    // from this point till we see a }, we will call next_token() to read
                    // regular token to us, instead of a string
                    let mut saw_rcurly = false;
                    let mut num_of_lcurly = 1;
                    while !self.src.is_eof() {
                        if let Ok(tok) = self.next_token() {
                            span = span.combine(tok.get_source_ref());
                            let saw_lcurly = matches!(tok.ty, TokenType::LCurly);
                            if saw_lcurly {
                                // we have nested interpolated strings with code sections
                                num_of_lcurly += 1;
                            }
                            saw_rcurly = matches!(tok.ty, TokenType::RCurly);
                            parts.push(tok);
                            if saw_rcurly {
                                if num_of_lcurly == 1 {
                                    // we are closing the original {
                                    break;
                                }
                                num_of_lcurly -= 1;
                            }
                        } else {
                            break;
                        }
                    }

                    if !saw_rcurly {
                        return Err(LexError::UnterminatedStringLiteral(span));
                    }
                    buf_span = self.src.get_ref();
                }
                _ => {
                    self.src.next_char();
                    buf_span = buf_span.combine(self.src.get_ref());
                }
            }
        }

        if self.src.is_eof() {
            return Err(LexError::UnterminatedStringLiteral(span));
        }

        // let span =
        self.queue = parts;
        Ok(bt)
    }

    // lex a potential identifier
    // it might turn out to be either a:
    // - keyword
    // - identifier
    fn lex_potential_identifier(&mut self) -> Result<SrcToken, LexError> {
        let mut id = String::new();
        let c_ref = self.src.get_ref();
        let cur = self.src.cur_char();
        id.push(cur);

        // read all characters that are alphanumeric or '_'
        let mut end_ref;
        loop {
            let c = self.src.next_char();
            end_ref = self.src.get_ref();
            if c.is_alphanumeric() || c == '_' {
                id.push(c);
            } else {
                break;
            }
        }

        let combined_ref = c_ref.combine(end_ref);
        let id = &self.src.text[combined_ref.flat_start..combined_ref.flat_end];
        // check if the identifier is a keyword
        match id {
            "pub" => Ok(SrcToken {
                ty: TokenType::Pub,
                loc: combined_ref,
            }),
            "fn" => Ok(SrcToken {
                ty: TokenType::Fn,
                loc: combined_ref,
            }),
            "if" => Ok(SrcToken {
                ty: TokenType::If,
                loc: combined_ref,
            }),
            "else" => Ok(SrcToken {
                ty: TokenType::Else,
                loc: combined_ref,
            }),
            "for" => Ok(SrcToken {
                ty: TokenType::For,
                loc: combined_ref,
            }),
            "in" => Ok(SrcToken {
                ty: TokenType::In,
                loc: combined_ref,
            }),
            "true" => Ok(SrcToken {
                ty: TokenType::True,
                loc: combined_ref,
            }),
            "false" => Ok(SrcToken {
                ty: TokenType::False,
                loc: combined_ref,
            }),
            "break" => Ok(SrcToken {
                ty: TokenType::Break,
                loc: combined_ref,
            }),
            "continue" => Ok(SrcToken {
                ty: TokenType::Continue,
                loc: combined_ref,
            }),
            "return" => Ok(SrcToken {
                ty: TokenType::Return,
                loc: combined_ref,
            }),
            "const" => Ok(SrcToken {
                ty: TokenType::Const,
                loc: combined_ref,
            }),
            "print" => Ok(SrcToken {
                ty: TokenType::Print,
                loc: combined_ref,
            }),
            "println" => Ok(SrcToken {
                ty: TokenType::Println,
                loc: combined_ref,
            }),
            "none" => Ok(SrcToken {
                ty: TokenType::None,
                loc: combined_ref,
            }),
            "some" => Ok(SrcToken {
                ty: TokenType::Some,
                loc: combined_ref,
            }),
            "defer" => Ok(SrcToken {
                ty: TokenType::Defer,
                loc: combined_ref,
            }),
            "i8" => Ok(SrcToken {
                ty: TokenType::I8,
                loc: combined_ref,
            }),
            "i16" => Ok(SrcToken {
                ty: TokenType::I16,
                loc: combined_ref,
            }),
            "i32" => Ok(SrcToken {
                ty: TokenType::I32,
                loc: combined_ref,
            }),
            "i64" => Ok(SrcToken {
                ty: TokenType::I64,
                loc: combined_ref,
            }),
            "int" => Ok(SrcToken {
                ty: TokenType::Int,
                loc: combined_ref,
            }),
            "u8" => Ok(SrcToken {
                ty: TokenType::U8,
                loc: combined_ref,
            }),
            "u16" => Ok(SrcToken {
                ty: TokenType::U16,
                loc: combined_ref,
            }),
            "u32" => Ok(SrcToken {
                ty: TokenType::U32,
                loc: combined_ref,
            }),
            "u64" => Ok(SrcToken {
                ty: TokenType::U64,
                loc: combined_ref,
            }),
            "uint" => Ok(SrcToken {
                ty: TokenType::UInt,
                loc: combined_ref,
            }),
            "f32" => Ok(SrcToken {
                ty: TokenType::F32,
                loc: combined_ref,
            }),
            "f64" => Ok(SrcToken {
                ty: TokenType::F64,
                loc: combined_ref,
            }),
            "bool" => Ok(SrcToken {
                ty: TokenType::Bool,
                loc: combined_ref,
            }),
            "char" => Ok(SrcToken {
                ty: TokenType::Char,
                loc: combined_ref,
            }),
            "str" => Ok(SrcToken {
                ty: TokenType::Str,
                loc: combined_ref,
            }),
            "type" => Ok(SrcToken {
                ty: TokenType::Type,
                loc: combined_ref,
            }),
            "void" => Ok(SrcToken {
                ty: TokenType::Void,
                loc: combined_ref,
            }),
            "struct" => Ok(SrcToken {
                ty: TokenType::Struct,
                loc: combined_ref,
            }),
            "and" => Ok(SrcToken {
                ty: TokenType::And,
                loc: combined_ref,
            }),
            "or" => Ok(SrcToken {
                ty: TokenType::Or,
                loc: combined_ref,
            }),
            "comptime" => Ok(SrcToken {
                ty: TokenType::Comptime,
                loc: combined_ref,
            }),
            "_" => Ok(SrcToken {
                ty: TokenType::Underscore,
                loc: combined_ref,
            }),
            _ => Ok(SrcToken {
                ty: TokenType::Identifier,
                loc: combined_ref,
            }),
        }
    }

    // lex an operator
    fn lex_operator(&mut self) -> Result<SrcToken, LexError> {
        let cur_ref = self.src.get_ref();
        let cur = self.src.cur_char();

        match cur {
            '+' => {
                let c = self.src.peek_char();
                if c == '=' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(SrcToken {
                        ty: TokenType::PlusAssign,
                        loc: cur_ref.combine(self.src.get_ref()),
                    });
                }
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::Plus,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            '-' => {
                let c = self.src.peek_char();
                if c == '=' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(SrcToken {
                        ty: TokenType::MinusAssign,
                        loc: cur_ref.combine(self.src.get_ref()),
                    });
                }
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::Minus,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            '*' => {
                let c = self.src.peek_char();
                if c == '=' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(SrcToken {
                        ty: TokenType::StarAssign,
                        loc: cur_ref.combine(self.src.get_ref()),
                    });
                }
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::Star,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            '/' => {
                // if the next character is a '/', lex a comment
                let c = self.src.peek_char();
                if c == '/' {
                    return self.lex_comment();
                }
                if c == '=' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(SrcToken {
                        ty: TokenType::SlashAssign,
                        loc: cur_ref.combine(self.src.get_ref()),
                    });
                }
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::Slash,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            '%' => {
                let c = self.src.peek_char();
                if c == '=' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(SrcToken {
                        ty: TokenType::ModuloAssign,
                        loc: cur_ref.combine(self.src.get_ref()),
                    });
                }
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::Modulo,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }

            '@' => {
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::At,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            '!' => {
                let c = self.src.peek_char();
                if c == '=' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(SrcToken {
                        ty: TokenType::NotEqual,
                        loc: cur_ref.combine(self.src.get_ref()),
                    });
                }
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::Not,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            '&' => {
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::Ampersand,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            '=' => {
                let c = self.src.peek_char();
                if c == '=' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(SrcToken {
                        ty: TokenType::Equal,
                        loc: cur_ref.combine(self.src.get_ref()),
                    });
                }
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::Assign,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            '<' => {
                let c = self.src.peek_char();
                if c == '=' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(SrcToken {
                        ty: TokenType::LessEqual,
                        loc: cur_ref.combine(self.src.get_ref()),
                    });
                }
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::Less,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            '>' => {
                let c = self.src.peek_char();
                if c == '=' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(SrcToken {
                        ty: TokenType::GreaterEqual,
                        loc: cur_ref.combine(self.src.get_ref()),
                    });
                }
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::Greater,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            ':' => {
                let c = self.src.peek_char();
                if c == ':' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(SrcToken {
                        ty: TokenType::DoubleColon,
                        loc: cur_ref.combine(self.src.get_ref()),
                    });
                } else if c == '=' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(SrcToken {
                        ty: TokenType::ColonAssign,
                        loc: cur_ref.combine(self.src.get_ref()),
                    });
                }
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::Colon,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            '(' => {
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::LParen,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            ')' => {
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::RParen,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            '{' => {
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::LCurly,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            '}' => {
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::RCurly,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            '[' => {
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::LBracket,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            ']' => {
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::RBracket,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            ',' => {
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::Comma,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            '.' => {
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::Dot,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            ';' => {
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::Semicolon,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            '?' => {
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::QuestionMark,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            '\\' => {
                self.src.next_char();
                return Ok(SrcToken {
                    ty: TokenType::BackSlash,
                    loc: cur_ref.combine(self.src.get_ref()),
                });
            }
            _ => unreachable!("invalid operator character: '{}' at {:?}", cur, cur_ref),
        }
    }

    fn lex_string(&mut self) -> Result<SrcToken, LexError> {
        let mut span = self.src.get_ref();

        // read all characters until terminating "
        // if a new line is encountered, return an error
        // if a \ is encountered, read the next character
        while !self.src.is_eof() {
            let c = self.src.next_char();
            span = span.combine(self.src.get_ref());
            if c == '"' || c == '\n' {
                break;
            } else if c == '\\' {
                // if the next character is a ", add it to the content
                self.src.next_char();
                span = span.combine(self.src.get_ref());
            }
        }

        // if the string is not terminated, return an error
        if self.src.is_eof() || self.src.cur_char() == '\n' {
            Err(LexError::UnterminatedStringLiteral(span))
        } else {
            self.src.next_char();
            span = span.combine(self.src.get_ref());
            Ok(SrcToken {
                ty: TokenType::String,
                loc: span,
            })
        }
    }

    // try to lex a character
    fn lex_char(&mut self) -> Result<SrcToken, LexError> {
        let mut span = self.src.get_ref();
        // read single character
        let c = self.src.next_char();
        span = span.combine(self.src.get_ref());

        // if the next character is a ', return an error
        if c == '\'' {
            return Err(LexError::EmptyCharacterLiteral(span));
        } else if c == '\\' {
            // if the next character is a ', add it to the content
            span = span.combine(self.src.get_ref());
        }

        // if the next character is not a ', return an error
        let c = self.src.next_char();
        span = span.combine(self.src.get_ref());
        if c != '\'' {
            return Err(LexError::UnterminatedCharacterLiteral(span));
        }

        self.src.next_char();
        span = span.combine(self.src.get_ref());
        Ok(SrcToken {
            ty: TokenType::Character,
            loc: span,
        })
    }

    fn lex_number(&mut self) -> Result<SrcToken, LexError> {
        let mut number = String::new();
        let cur_ref = self.src.get_ref();
        let cur = self.src.cur_char();
        number.push(cur);

        // read all characters that are digits
        let mut end_ref;
        let mut decimal_point_count = 0;
        loop {
            let c = self.src.next_char();
            end_ref = self.src.get_ref();
            if c.is_ascii_digit() {
                number.push(c);
            } else if c == '_' {
                // ignore underscores
                continue;
            } else if c == '.' {
                decimal_point_count += 1;
                number.push(c);
            } else {
                break;
            }
        }

        // TODO: there is bug where a user can enter a decimal with no mantissa
        // provided: `12323.`

        let combined_ref = cur_ref.combine(end_ref);
        if decimal_point_count == 0 {
            return Ok(SrcToken {
                ty: TokenType::Integer,
                loc: combined_ref,
            });
        } else if decimal_point_count == 1 {
            return Ok(SrcToken {
                ty: TokenType::Decimal,
                loc: combined_ref,
            });
        } else {
            Err(LexError::DecimalLiteralWithMultipleDecimalPoints(
                combined_ref,
            ))
        }
    }
}

#[cfg(test)]
#[allow(dead_code)]
#[derive(serde::Deserialize, serde::Serialize, Debug)]
struct LexerTestResult {
    stringified_tokens: Vec<String>,
}

#[cfg(test)]
#[test]
fn test_lexer() {
    insta::glob!("lexer_inputs/*.pr", |path| {
        // build the SourceFile from the proto file
        let path = path.to_str().unwrap().to_string();
        let src: SourceFile = SourceFile::new(path);

        // build the lexer
        let mut lexer = Lexer::new(src);

        let mut res = LexerTestResult {
            stringified_tokens: Vec::new(),
        };
        loop {
            let token = lexer.next_token();
            if token.is_err() {
                panic!("error lexing: {token:?}");
            }
            let token = token.unwrap();
            if let TokenType::Eof = token.ty {
                break;
            }
            res.stringified_tokens.push(format!(
                "{} at {}",
                token.as_str(&lexer.src),
                token.get_source_ref().as_str()
            ));
        }
        insta::assert_yaml_snapshot!(res)
    });
}
