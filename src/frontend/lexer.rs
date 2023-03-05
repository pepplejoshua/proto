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

    // try to lex the next possible token
    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        // skip all whitespace
        self.skip_whitespace();

        // get the current character
        let c = self.src.cur_char();

        // if we reached the end of the file, return EOF
        if c == '\0' {
            return Ok(Token::Eof(self.src.get_ref()));
        }

        match c {
            // operators
            '+' | '-' | '*' | '/' | '%' | '!' | '=' | '<' | '>' => self.lex_operator(),
            // numbers
            _ if c.is_ascii_digit() => self.lex_number(),
            // identifiers | keywords
            _ if c.is_alphabetic() => self.lex_potential_identifier(),
            // invalid character
            _ => Err(LexerError::InvalidCharacter(self.src.get_ref())),
        }
    }

    // lex a potential identifier
    // it might turn out to be either a:
    // - keyword
    // - identifier
    fn lex_potential_identifier(&mut self) -> Result<Token, LexerError> {
        let mut id = String::new();
        let c_ref = self.src.get_ref();
        let cur = self.src.next_char();
        id.push(cur);

        // read all characters that are alphanumeric or '_'
        let mut end_ref = self.src.get_ref();
        loop {
            let c = self.src.next_char();
            if c.is_alphanumeric() || c == '_' {
                end_ref = self.src.get_ref();
                id.push(self.src.next_char());
            } else {
                break;
            }
        }

        let combined_ref = c_ref.combine(&end_ref);
        // check if the identifier is a keyword
        match id.as_str() {
            "fn" => Ok(Token::Fn(combined_ref)),
            "let" => Ok(Token::Let(combined_ref)),
            "mut" => Ok(Token::Mut(combined_ref)),
            "if" => Ok(Token::If(combined_ref)),
            "else" => Ok(Token::Else(combined_ref)),
            "loop" => Ok(Token::Loop(combined_ref)),
            "while" => Ok(Token::While(combined_ref)),
            "void" => Ok(Token::Void(combined_ref)),
            "true" => Ok(Token::True(combined_ref)),
            "false" => Ok(Token::False(combined_ref)),

            "and" => Ok(Token::And(combined_ref)),
            "or" => Ok(Token::Or(combined_ref)),
            "not" => Ok(Token::Not(combined_ref)),

            "i8" => Ok(Token::I8(combined_ref)),
            "i16" => Ok(Token::I16(combined_ref)),
            "i32" => Ok(Token::I32(combined_ref)),
            "i64" => Ok(Token::I64(combined_ref)),
            "isize" => Ok(Token::Isize(combined_ref)),
            "u8" => Ok(Token::U8(combined_ref)),
            "u16" => Ok(Token::U16(combined_ref)),
            "u32" => Ok(Token::U32(combined_ref)),
            "u64" => Ok(Token::U64(combined_ref)),
            "usize" => Ok(Token::Usize(combined_ref)),
            "bool" => Ok(Token::Bool(combined_ref)),
            "char" => Ok(Token::Char(combined_ref)),
            _ => Ok(Token::Identifier(id, combined_ref)),
        }
    }

    // lex an operator
    fn lex_operator(&mut self) -> Result<Token, LexerError> {
        let cur_ref = self.src.get_ref();
        let cur = self.src.next_char();
        // detect a signed number
        if cur == '-' {
            // if the next character is a digit, lex a signed number
            let c = self.src.peek_char();
            if c.is_digit(10) {
                return self.lex_signed_number();
            } else {
                // otherwise, return a minus operator
                return Ok(Token::Minus(cur_ref));
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
                let c = self.src.peek_char();
                if c == '=' {
                    return Ok(Token::NotEqual(cur_ref));
                }
                // otherwise, return a Not operator
                Ok(Token::Not(cur_ref))
            }
            '=' => {
                // if the next character is a '=', return a Equal operator
                let c = self.src.peek_char();
                if c == '=' {
                    return Ok(Token::Equal(cur_ref));
                }
                // otherwise, return a Assign operator
                Ok(Token::Assign(cur_ref))
            }
            '<' => {
                // if the next character is a '=', return a LessEqual operator
                let c = self.src.peek_char();
                if c == '=' {
                    return Ok(Token::LessEqual(cur_ref));
                }
                // otherwise, return a Less operator
                Ok(Token::Less(cur_ref))
            }
            '>' => {
                // if the next character is a '=', return a GreaterEqual operator
                let c = self.src.peek_char();
                if c == '=' {
                    return Ok(Token::GreaterEqual(cur_ref));
                }
                // otherwise, return a Greater operator
                Ok(Token::Greater(cur_ref))
            }
            _ => unreachable!("invalid operator"),
        }
    }

    // try to lex a number
    fn lex_number(&mut self) -> Result<Token, LexerError> {
        // if it is unsigned, call lex_unsigned_number
        panic!("not implemented yet");
    }

    // try to lex an unsigned number which is one of:
    // - u8
    // - u16
    // - u32
    // - u64
    // - usize
    fn lex_unsigned_number(&mut self) -> Result<Token, LexerError> {
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
    fn lex_signed_number(&mut self) -> Result<Token, LexerError> {
        let mut signed_number = String::new();
        let cur_ref = self.src.get_ref();
        let cur = self.src.next_char();
        signed_number.push(cur);
    }
}
