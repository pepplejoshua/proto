use super::errors::LexerError;
use super::source::{SourceFile, SourceRef};
use super::token::Token;

pub struct Lexer {
    src: SourceFile,
}

impl Lexer {
    pub fn new(src: SourceFile) -> Lexer {
        Lexer { src }
    }

    // skip all preceding whitespace
    fn skip_whitespace(&mut self) {
        while let c = self.src.next_char() {
            if c.is_whitespace() {
                continue;
            } else {
                break;
            }
        }
    }

    // try to lex the next possible token
    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        // skip all whitespace
        self.skip_whitespace();

        // get the next c_ref and character
        let c_ref = self.src.get_ref();
        let c = self.src.next_char();

        // if we reached the end of the file, return EOF
        if c == '\0' {
            return Ok(Token::Eof(self.src.get_ref()));
        }

        match c {
            // operators
            '+' | '-' | '*' | '/' | '%' | '!' | '=' | '<' | '>' => self.lex_operator(c, c_ref),
            _ if c.is_digit(10) => self.lex_number(c, c_ref),
            _ if c.is_alphabetic() => self.lex_potential_identifier(c, c_ref),
            _ => Err(LexerError::InvalidCharacter(c, c_ref)),
        }
    }

    // lex a potential identifier
    // it might turn out to be either a:
    // - keyword
    // - identifier
    fn lex_potential_identifier(
        &mut self,
        cur: char,
        cur_ref: SourceRef,
    ) -> Result<Token, LexerError> {
        let mut id = String::new();
        id.push(cur);

        // read all characters that are alphanumeric or '_'
        while let c = self.src.peek_char() {
            if c.is_alphanumeric() || c == '_' {
                id.push(self.src.next_char());
            } else {
                break;
            }
        }

        // check if the identifier is a keyword
        match id.as_str() {
            "fn" => Ok(Token::Fn(self.src.get_ref())),

            _ => Ok(Token::Identifier(id, self.src.get_ref())),
        }
    }

    // lex an operator
    fn lex_operator(&mut self, cur: char, cur_ref: SourceRef) -> Result<Token, LexerError> {
        // detect a signed number
        if cur == '-' {
            // if the next character is a digit, lex a signed number
            if let c = self.src.peek_char() {
                if c.is_digit(10) {
                    return self.lex_signed_number(cur, cur_ref);
                } else {
                    // otherwise, return a minus operator
                    return Ok(Token::Minus(cur_ref));
                }
            }
        }

        match cur {
            // arithmetic operators
            '+' => Ok(Token::Plus(cur_ref)),
            '-' => Ok(Token::Minus(cur_ref)),
            '*' => Ok(Token::Star(cur_ref)),
            '/' => Ok(Token::Slash(cur_ref)),
            '%' => Ok(Token::Modulo(cur_ref)),

            // logical operators
            '!' => {
                // if the next character is a '=', return a NotEqual operator
                if let c = self.src.peek_char() {
                    if c == '=' {
                        return Ok(Token::NotEqual(cur_ref));
                    }
                }
                // otherwise, return a Not operator
                Ok(Token::Not(cur_ref))
            }
            '=' => {
                // if the next character is a '=', return a Equal operator
                if let c = self.src.peek_char() {
                    if c == '=' {
                        return Ok(Token::Equal(cur_ref));
                    }
                }
                // otherwise, return a Assign operator
                Ok(Token::Assign(cur_ref))
            }
            '<' => {
                // if the next character is a '=', return a LessEqual operator
                if let c = self.src.peek_char() {
                    if c == '=' {
                        return Ok(Token::LessEqual(cur_ref));
                    }
                }
                // otherwise, return a Less operator
                Ok(Token::Less(cur_ref))
            }
            '>' => {
                // if the next character is a '=', return a GreaterEqual operator
                if let c = self.src.peek_char() {
                    if c == '=' {
                        return Ok(Token::GreaterEqual(cur_ref));
                    }
                }
                // otherwise, return a Greater operator
                Ok(Token::Greater(cur_ref))
            }
            _ => unreachable!("invalid operator"),
        }
    }

    // try to lex a number
    fn lex_number(&mut self, first_char: char, cur_ref: SourceRef) -> Result<Token, LexerError> {
        // if it is unsigned, call lex_unsigned_number
        panic!("not implemented yet");
    }

    // try to lex an unsigned number which is one of:
    // - u8
    // - u16
    // - u32
    // - u64
    // - usize
    fn lex_unsigned_number(&mut self, first_char: char) -> Result<Token, LexerError> {
        // if it is u8, call lex_u8

        // if it is u16, call lex_u16

        // if it is u32, call lex_u32

        // if it is u64, call lex_u64
        panic!("not implemented yet");
        // if it is usize, call lex_usize
    }

    // try to lex an unsigned number which is one of:
    // - i8
    // - i16
    // - i32
    // - i64
    // - isize
    fn lex_signed_number(&mut self, first_char: char) -> Result<Token, LexerError> {
        // if it is u8, call lex_u8

        // if it is u16, call lex_u16

        // if it is u32, call lex_u32

        // if it is u64, call lex_u64
        panic!("not implemented yet");
        // if it is usize, call lex_usize
    }
}
