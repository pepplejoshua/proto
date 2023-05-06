use super::errors::LexError;
use super::source::SourceFile;
use super::token::Token;

#[allow(dead_code)]
pub struct Lexer {
    pub src: SourceFile,
    last_lexed: Option<Token>,
}

#[allow(dead_code)]
impl Lexer {
    pub fn new(src: SourceFile) -> Lexer {
        Lexer {
            src,
            last_lexed: None,
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

    // process comments
    fn process_comment(&mut self) {
        let start = self.src.cur_char();
        let peek = self.src.peek_char();

        if start == '/' && peek == '/' {
            // consume both "//" characters
            self.src.next_char();
            self.src.next_char();

            // single line comment
            // skip until we reach a newline
            loop {
                let cur = self.src.cur_char();
                self.src.next_char();
                if cur != '\n' {
                    continue;
                } else {
                    break;
                }
            }
        }
    }

    // try to lex the next possible token
    pub fn next_token(&mut self) -> Result<Token, LexError> {
        while self.src.cur_char().is_whitespace()
            || (self.src.cur_char() == '/' && self.src.peek_char() == '/')
        {
            // skip all preceding whitespace
            self.skip_whitespace();
            // process comments
            // eventually, it'll get documentation comments
            // for now, it just skips single line comments
            self.process_comment();
        }

        // get the current character
        let c = self.src.cur_char();

        // if we reached the end of the file, return EOF
        if c == '\0' {
            return Ok(Token::Eof(self.src.get_ref()));
        }
        let maybe_token = match c {
            // operators
            '+' | '-' | '*' | '/' | '%' | '!' | '=' | '<' | '>' | '(' | ')' | '{' | '}' | '['
            | ']' | ',' | '.' | ':' | ';' | '^' | '$' | '@' => self.lex_operator(),
            // numbers
            _ if c.is_ascii_digit() => self.lex_number(),
            // identifiers | keywords
            _ if c.is_alphabetic() || c == '_' => self.lex_potential_identifier(),
            // ' which is used to start characters or strings
            '\'' => self.lex_char(),
            // " which is used to start strings
            '"' => self.lex_string(),
            // invalid character
            _ => {
                let mut err_ref = self.src.get_ref();
                err_ref.end_col += 1;
                let err = Err(LexError::InvalidCharacter(err_ref));
                self.src.next_char(); // try to keep lexing
                err
            }
        };

        match &maybe_token {
            Ok(t) => self.last_lexed = Some(t.clone()),
            Err(_) => self.last_lexed = None,
        }
        maybe_token
    }

    // lex a potential identifier
    // it might turn out to be either a:
    // - keyword
    // - identifier
    fn lex_potential_identifier(&mut self) -> Result<Token, LexError> {
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
            "break" => Ok(Token::Break(combined_ref)),
            "continue" => Ok(Token::Continue(combined_ref)),
            "return" => Ok(Token::Return(combined_ref)),
            "use" => Ok(Token::Use(combined_ref)),
            "pub" => Ok(Token::Pub(combined_ref)),
            "mod" => Ok(Token::Mod(combined_ref)),
            "as" => Ok(Token::As(combined_ref)),

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
            "str" => Ok(Token::Str(combined_ref)),
            _ => Ok(Token::Identifier(id, combined_ref)),
        }
    }

    // lex an operator
    fn lex_operator(&mut self) -> Result<Token, LexError> {
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
                return Ok(Token::Minus(cur_ref.combine(self.src.get_ref())));
            }
        }

        match cur {
            // arithmetic operators
            '+' => {
                self.src.next_char();
                Ok(Token::Plus(cur_ref.combine(self.src.get_ref())))
            }
            '-' => {
                self.src.next_char();
                Ok(Token::Minus(cur_ref.combine(self.src.get_ref())))
            }
            '*' => {
                self.src.next_char();
                Ok(Token::Star(cur_ref.combine(self.src.get_ref())))
            }
            '/' => {
                self.src.next_char();
                Ok(Token::Slash(cur_ref.combine(self.src.get_ref())))
            }
            '%' => {
                self.src.next_char();
                Ok(Token::Modulo(cur_ref.combine(self.src.get_ref())))
            }

            // special characters
            '^' => {
                self.src.next_char();
                Ok(Token::Caret(cur_ref.combine(self.src.get_ref())))
            }
            '@' => {
                self.src.next_char();
                Ok(Token::At(cur_ref.combine(self.src.get_ref())))
            }
            '$' => {
                self.src.next_char();
                Ok(Token::Dollar(cur_ref.combine(self.src.get_ref())))
            }

            // logical operators
            '!' => {
                // if the next character is a '=', return a NotEqual operator
                let c = self.src.peek_char();
                if c == '=' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(Token::NotEqual(cur_ref.combine(self.src.get_ref())));
                }
                // otherwise, return a Not operator
                self.src.next_char();
                Ok(Token::Exclamation(cur_ref.combine(self.src.get_ref())))
            }
            '=' => {
                // if the next character is a '=', return a Equal operator
                let c = self.src.peek_char();
                if c == '=' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(Token::Equal(cur_ref.combine(self.src.get_ref())));
                }
                // otherwise, return a Assign operator
                self.src.next_char();
                Ok(Token::Assign(cur_ref.combine(self.src.get_ref())))
            }
            '<' => {
                // if the next character is a '=', return a LessEqual operator
                let c = self.src.peek_char();
                if c == '=' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(Token::LessEqual(cur_ref.combine(self.src.get_ref())));
                }
                // otherwise, return a Less operator
                self.src.next_char();
                Ok(Token::Less(cur_ref.combine(self.src.get_ref())))
            }
            '>' => {
                // if the next character is a '=', return a GreaterEqual operator
                let c = self.src.peek_char();
                if c == '=' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(Token::GreaterEqual(cur_ref.combine(self.src.get_ref())));
                }
                // otherwise, return a Greater operator
                self.src.next_char();
                Ok(Token::Greater(cur_ref.combine(self.src.get_ref())))
            }
            ':' => {
                // if the next character is a ':', return a Scope operator
                let c = self.src.peek_char();
                if c == ':' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(Token::Scope(cur_ref.combine(self.src.get_ref())));
                }
                // otherwise, return a Colon operator
                self.src.next_char();
                Ok(Token::Colon(cur_ref.combine(self.src.get_ref())))
            }
            '(' => {
                self.src.next_char();
                Ok(Token::LParen(cur_ref.combine(self.src.get_ref())))
            }
            ')' => {
                self.src.next_char();
                Ok(Token::RParen(cur_ref.combine(self.src.get_ref())))
            }
            '{' => {
                self.src.next_char();
                Ok(Token::LCurly(cur_ref.combine(self.src.get_ref())))
            }
            '}' => {
                self.src.next_char();
                Ok(Token::RCurly(cur_ref.combine(self.src.get_ref())))
            }
            '[' => {
                self.src.next_char();
                Ok(Token::LBracket(cur_ref.combine(self.src.get_ref())))
            }
            ']' => {
                self.src.next_char();
                Ok(Token::RBracket(cur_ref.combine(self.src.get_ref())))
            }
            ',' => {
                self.src.next_char();
                Ok(Token::Comma(cur_ref.combine(self.src.get_ref())))
            }
            '.' => {
                self.src.next_char();
                Ok(Token::Dot(cur_ref.combine(self.src.get_ref())))
            }
            ';' => {
                self.src.next_char();
                Ok(Token::Semicolon(cur_ref.combine(self.src.get_ref())))
            }
            _ => unreachable!("invalid operator character: '{}' at {:?}", cur, cur_ref),
        }
    }

    fn lex_string(&mut self) -> Result<Token, LexError> {
        let mut span = self.src.get_ref();
        let mut content = String::new();

        // read all characters until terminating "
        while !self.src.is_eof() {
            let c = self.src.next_char();
            span = span.combine(self.src.get_ref());
            if c == '"' {
                break;
            } else if c == '\\' {
                // if the next character is a ", add it to the content
                let c = self.src.next_char();
                span = span.combine(self.src.get_ref());
                if c == '"' {
                    content.push('"');
                } else {
                    // otherwise, add the escape character
                    content.push('\\');
                    content.push(c);
                }
            } else {
                content.push(c);
            }
        }

        // if the string is not terminated, return an error
        if self.src.is_eof() {
            Err(LexError::UnterminatedStringLiteral(span))
        } else {
            self.src.next_char();
            span = span.combine(self.src.get_ref());
            Ok(Token::StringLiteral(span, content))
        }
    }

    // try to lex a character
    fn lex_char(&mut self) -> Result<Token, LexError> {
        let mut span = self.src.get_ref();
        let mut content = String::new();

        // read single character
        let c = self.src.next_char();
        span = span.combine(self.src.get_ref());

        // if the next character is a ', return an error
        if c == '\'' {
            return Err(LexError::EmptyCharacterLiteral(span));
        } else if c == '\\' {
            // if the next character is a ', add it to the content
            let c = self.src.next_char();
            span = span.combine(self.src.get_ref());
            if c == '\'' {
                content.push('\'');
            } else {
                // otherwise, add the escape character
                content.push('\\');
                content.push(c);
            }
        } else {
            content.push(c);
        }

        // if the next character is not a ', return an error
        let c = self.src.next_char();
        span = span.combine(self.src.get_ref());
        if c != '\'' {
            return Err(LexError::UnterminatedCharacterLiteral(span));
        }

        self.src.next_char();
        span = span.combine(self.src.get_ref());
        Ok(Token::CharLiteral(span, content.chars().next().unwrap()))
    }

    // try to lex a number
    fn lex_number(&mut self) -> Result<Token, LexError> {
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
    fn lex_signed_number(&mut self) -> Result<Token, LexError> {
        let mut signed_number = String::new();
        let cur_ref = self.src.get_ref();
        let cur = self.src.cur_char();
        signed_number.push(cur);

        // read all characters that are digits
        let mut end_ref;
        loop {
            let c = self.src.next_char();
            end_ref = self.src.get_ref();
            if c.is_ascii_digit() {
                signed_number.push(c);
            } else if c == '_' {
                // ignore underscores
                continue;
            } else {
                break;
            }
        }

        let combined_ref = cur_ref.combine(end_ref);
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

        // if signed_number's first character is not '-',
        // then attempt to lex an unsigned number
        self.src.jump_to(&cur_ref);
        self.lex_unsigned_number()
    }

    // try to lex an unsigned number which is one of:
    // - u8
    // - u16
    // - u32
    // - u64
    // - usize
    fn lex_unsigned_number(&mut self) -> Result<Token, LexError> {
        let mut unsigned_number = String::new();
        let cur_ref = self.src.get_ref();
        let cur = self.src.cur_char();
        unsigned_number.push(cur);

        // read all characters that are digits
        let mut end_ref;
        loop {
            let c = self.src.next_char();
            end_ref = self.src.get_ref();
            if c.is_ascii_digit() {
                unsigned_number.push(c);
            } else if c == '_' {
                // ignore underscores
                continue;
            } else {
                break;
            }
        }

        let combined_ref = cur_ref.combine(end_ref);
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
        Err(LexError::CannotMakeUnsignedNumber(combined_ref))
    }
}

#[allow(dead_code)]
#[derive(serde::Deserialize, serde::Serialize, Debug)]
struct LexerTestResult {
    stringified_tokens: Vec<String>,
}

#[test]
fn test_lexer() {
    insta::glob!("lexer_inputs/*.pr", |path| {
        // build the SourceFile from the proto file
        let path = path.to_str().unwrap().to_string();
        let src: SourceFile = SourceFile::new(path);

        // build the lexer
        let mut lexer = Lexer::new(src);

        let mut tokens = Vec::new();
        let mut res = LexerTestResult {
            stringified_tokens: Vec::new(),
        };
        loop {
            let token = lexer.next_token();
            if token.is_err() {
                panic!("error lexing: {token:?}");
            }
            let token = token.unwrap();
            if let Token::Eof(_) = token {
                break;
            }
            res.stringified_tokens.push(format!(
                "{} at {}",
                token.as_str(),
                token.get_source_ref().as_str()
            ));
            tokens.push(token);
        }
        insta::assert_yaml_snapshot!(res)
    });
}
