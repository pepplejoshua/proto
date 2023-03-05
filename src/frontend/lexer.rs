use super::errors::LexerError;
use super::source::SourceFile;
use super::token::Token;

#[allow(dead_code)]
pub struct Lexer {
    src: SourceFile,
}

#[allow(dead_code)]
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
        // skip all preceding whitespace
        self.skip_whitespace();

        // get the current character
        let c = self.src.cur_char();

        // if we reached the end of the file, return EOF
        if c == '\0' {
            return Ok(Token::Eof(self.src.get_ref()));
        }
        match c {
            // operators
            '+' | '-' | '*' | '/' | '%' | '!' | '=' | '<' | '>' | '(' | ')' | '{' | '}' | '['
            | ']' | ',' | '.' | ':' | ';' => self.lex_operator(),
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

        // read all characters that are alphanumeric or '_'
        let mut end_ref = self.src.get_ref();
        loop {
            let c = self.src.cur_char();
            if c.is_alphanumeric() || c == '_' {
                end_ref = self.src.get_ref();
                self.src.next_char();
                id.push(c);
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
        let cur = self.src.cur_char();
        // detect a signed number
        if cur == '-' {
            // if the next character is a digit, lex a signed number
            let peek = self.src.peek_char();
            if peek.is_ascii_digit() {
                return self.lex_signed_number();
            } else {
                // otherwise, return a minus operator
                self.src.next_char();
                return Ok(Token::Minus(cur_ref.combine(&self.src.get_ref())));
            }
        }

        match cur {
            // arithmetic operators
            '+' => {
                self.src.next_char();
                Ok(Token::Plus(cur_ref.combine(&self.src.get_ref())))
            }
            '-' => {
                self.src.next_char();
                Ok(Token::Minus(cur_ref.combine(&self.src.get_ref())))
            }
            '*' => {
                self.src.next_char();
                Ok(Token::Star(cur_ref.combine(&self.src.get_ref())))
            }
            '/' => {
                self.src.next_char();
                Ok(Token::Slash(cur_ref.combine(&self.src.get_ref())))
            }
            '%' => {
                self.src.next_char();
                Ok(Token::Modulo(cur_ref.combine(&self.src.get_ref())))
            }

            // logical operators
            '!' => {
                // if the next character is a '=', return a NotEqual operator
                let c = self.src.peek_char();
                if c == '=' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(Token::NotEqual(cur_ref.combine(&self.src.get_ref())));
                }
                // otherwise, return a Not operator
                self.src.next_char();
                Ok(Token::Not(cur_ref.combine(&self.src.get_ref())))
            }
            '=' => {
                // if the next character is a '=', return a Equal operator
                let c = self.src.peek_char();
                if c == '=' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(Token::Equal(cur_ref.combine(&self.src.get_ref())));
                }
                // otherwise, return a Assign operator
                self.src.next_char();
                Ok(Token::Assign(cur_ref.combine(&self.src.get_ref())))
            }
            '<' => {
                // if the next character is a '=', return a LessEqual operator
                let c = self.src.peek_char();
                if c == '=' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(Token::LessEqual(cur_ref.combine(&self.src.get_ref())));
                }
                // otherwise, return a Less operator
                self.src.next_char();
                Ok(Token::Less(cur_ref.combine(&self.src.get_ref())))
            }
            '>' => {
                // if the next character is a '=', return a GreaterEqual operator
                let c = self.src.peek_char();
                if c == '=' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(Token::GreaterEqual(cur_ref.combine(&self.src.get_ref())));
                }
                // otherwise, return a Greater operator
                self.src.next_char();
                Ok(Token::Greater(cur_ref.combine(&self.src.get_ref())))
            }
            '(' => {
                self.src.next_char();
                Ok(Token::LParen(cur_ref.combine(&self.src.get_ref())))
            }
            ')' => {
                self.src.next_char();
                Ok(Token::RParen(cur_ref.combine(&self.src.get_ref())))
            }
            '{' => {
                self.src.next_char();
                Ok(Token::LBrace(cur_ref.combine(&self.src.get_ref())))
            }
            '}' => {
                self.src.next_char();
                Ok(Token::RBrace(cur_ref.combine(&self.src.get_ref())))
            }
            '[' => {
                self.src.next_char();
                Ok(Token::LBracket(cur_ref.combine(&self.src.get_ref())))
            }
            ']' => {
                self.src.next_char();
                Ok(Token::RBracket(cur_ref.combine(&self.src.get_ref())))
            }
            ',' => {
                self.src.next_char();
                Ok(Token::Comma(cur_ref.combine(&self.src.get_ref())))
            }
            '.' => {
                self.src.next_char();
                Ok(Token::Dot(cur_ref.combine(&self.src.get_ref())))
            }
            ':' => {
                self.src.next_char();
                Ok(Token::Colon(cur_ref.combine(&self.src.get_ref())))
            }
            ';' => {
                self.src.next_char();
                Ok(Token::Semicolon(cur_ref.combine(&self.src.get_ref())))
            }
            _ => unreachable!("invalid operator character: '{}' at {:?}", cur, cur_ref),
        }
    }

    // try to lex a number
    fn lex_number(&mut self) -> Result<Token, LexerError> {
        // by default, calling lex_number will lex a signed number
        // since users tend to perform signed arithmetic more often
        self.lex_signed_number()
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
        let cur = self.src.cur_char();
        signed_number.push(cur);

        // read all characters that are digits
        let mut end_ref = self.src.get_ref();
        loop {
            let c = self.src.next_char();
            if c.is_ascii_digit() {
                end_ref = self.src.get_ref();
                signed_number.push(c);
            } else if c == '_' {
                // ignore underscores
                continue;
            } else {
                break;
            }
        }

        let combined_ref = cur_ref.combine(&end_ref);
        // check if signed_number is i8
        if let Ok(num) = signed_number.parse::<i8>() {
            return Ok(Token::I8Literal(num, combined_ref));
        }

        if let Ok(num) = signed_number.parse::<i16>() {
            return Ok(Token::I16Literal(num, combined_ref));
        }

        if let Ok(num) = signed_number.parse::<i32>() {
            return Ok(Token::I32Literal(num, combined_ref));
        }

        if let Ok(num) = signed_number.parse::<i64>() {
            return Ok(Token::I64Literal(num, combined_ref));
        }

        if let Ok(num) = signed_number.parse::<isize>() {
            return Ok(Token::IsizeLiteral(num, combined_ref));
        }

        // if it is not a valid signed number, return an error
        Err(LexerError::CannotMakeSignedNumber(combined_ref))
    }

    // try to lex an unsigned number which is one of:
    // - u8
    // - u16
    // - u32
    // - u64
    // - usize
    fn lex_unsigned_number(&mut self) -> Result<Token, LexerError> {
        let mut unsigned_number = String::new();
        let cur_ref = self.src.get_ref();
        let cur = self.src.cur_char();
        unsigned_number.push(cur);

        // read all characters that are digits
        let mut end_ref = self.src.get_ref();
        loop {
            let c = self.src.next_char();
            if c.is_ascii_digit() {
                end_ref = self.src.get_ref();
                unsigned_number.push(c);
            } else if c == '_' {
                // ignore underscores
                continue;
            } else {
                break;
            }
        }

        let combined_ref = cur_ref.combine(&end_ref);
        // check if unsigned_number is u8
        if let Ok(num) = unsigned_number.parse::<u8>() {
            return Ok(Token::U8Literal(num, combined_ref));
        }

        if let Ok(num) = unsigned_number.parse::<u16>() {
            return Ok(Token::U16Literal(num, combined_ref));
        }

        if let Ok(num) = unsigned_number.parse::<u32>() {
            return Ok(Token::U32Literal(num, combined_ref));
        }

        if let Ok(num) = unsigned_number.parse::<u64>() {
            return Ok(Token::U64Literal(num, combined_ref));
        }

        if let Ok(num) = unsigned_number.parse::<usize>() {
            return Ok(Token::UsizeLiteral(num, combined_ref));
        }

        // if it is not a valid unsigned number, return an error
        Err(LexerError::CannotMakeUnsignedNumber(combined_ref))
    }
}

#[allow(dead_code)]
#[derive(serde::Deserialize, serde::Serialize, Debug)]
struct LexerTestResult {
    stringified_tokens: Vec<String>,
}

#[test]
fn test_lexer() {
    let mut res = LexerTestResult {
        stringified_tokens: Vec::new(),
    };
    insta::glob!("lexer_inputs/*.json", |path| {
        // build the SourceFile from the json file
        let file_contents = std::fs::read_to_string(path).unwrap();
        let src: SourceFile = serde_json::from_str(&file_contents).unwrap();

        // build the lexer
        let mut lexer = Lexer::new(src);

        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            if token.is_err() {
                panic!("error lexing: {token:?}");
            }
            let token = token.unwrap();
            if let Token::Eof(_) = token {
                break;
            }
            res.stringified_tokens.push(format!("{token:?}"));
            tokens.push(token);
        }
        insta::assert_yaml_snapshot!(res)
    });
}
