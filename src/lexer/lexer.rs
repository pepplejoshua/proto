use super::token::Token;
use crate::source::{errors::LexError, source::SourceFile};

#[allow(dead_code)]
pub struct Lexer {
    pub src: SourceFile,
    last_lexed: Option<Token>,
    queue: Vec<Token>,
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

    fn lex_comment(&mut self) -> Result<Token, LexError> {
        let start = self.src.cur_char();
        let mut content = String::from(start);
        let mut span = self.src.get_ref();
        // skip both '/' characters
        content.push(self.src.next_char());
        span = span.combine(self.src.get_ref());
        self.src.next_char();

        while !self.src.is_eof() {
            let cur = self.src.cur_char();
            span = span.combine(self.src.get_ref());
            self.src.next_char();
            if cur != '\n' {
                content.push(cur);
                continue;
            } else {
                break;
            }
        }
        Ok(Token::SingleLineComment(span, content))
    }

    // try to lex the next possible token
    pub fn next_token(&mut self) -> Result<Token, LexError> {
        if !self.queue.is_empty() {
            let tok = self.queue.remove(0);
            self.last_lexed = Some(tok.clone());
            return Ok(tok);
        }

        while self.src.cur_char().is_whitespace() {
            // TODO: understand the ramifications of using newlines instead of ;
            // we care about newline characters since they take the place
            // of semicolons in the grammar
            // if self.src.cur_char() == '\n' {
            //     let newline_loc = self.src.get_ref();
            //     self.src.next_char();
            //     return Ok(Token::Newline(newline_loc));
            // }
            // skip all preceding whitespace
            self.skip_whitespace();
            // process comments
            // eventually, it'll get documentation comments
            // for now, it just skips single line comments
            // self.process_comment();
        }

        // get the current character
        let c = self.src.cur_char();

        // if we reached the end of the file, return EOF
        if c == '\0' {
            return Ok(Token::Eof(self.src.get_ref()));
        }
        let maybe_token = match c {
            '`' => self.lex_interpolated_string(),
            // operators
            '+' | '-' | '*' | '/' | '%' | '!' | '=' | '<' | '>' | '(' | ')' | '{' | '}' | '['
            | ']' | ',' | '.' | ':' | ';' | '@' | '|' | '&' | '?' => self.lex_operator(),
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

    fn lex_interpolated_string(&mut self) -> Result<Token, LexError> {
        let mut span = self.src.get_ref();
        self.src.next_char(); // consume initial backtick
        span = span.combine(self.src.get_ref());
        let bt = Token::BackTick(span.clone());
        let mut parts = vec![];

        let mut buf = String::new();
        let mut buf_span = self.src.get_ref();
        while !self.src.is_eof() {
            let c = self.src.cur_char();

            match c {
                '`' => {
                    span = self.src.get_ref();
                    self.src.next_char();
                    span = span.combine(self.src.get_ref());
                    if !buf.is_empty() {
                        parts.push(Token::InterpStrLiteral(buf_span.clone(), buf.clone()))
                    }
                    parts.push(Token::BackTick(span.clone()));
                    break;
                }
                '{' => {
                    if self.src.peek_char() == '{' {
                        // we can escape the { character and add it to the buf
                        self.src.next_char();
                        self.src.next_char();
                        buf.push('{');
                        buf_span = buf_span.combine(self.src.get_ref());
                        continue;
                    }

                    // store whatever we have in buf and reset it
                    span = span.combine(self.src.get_ref());
                    if !buf.is_empty() {
                        parts.push(Token::InterpStrLiteral(buf_span.clone(), buf.clone()));
                        buf.clear();
                    }

                    let mut lcurly_span = self.src.get_ref();
                    self.src.next_char();
                    lcurly_span = lcurly_span.combine(self.src.get_ref());
                    let lcurly = Token::LCurly(lcurly_span);
                    parts.push(lcurly);

                    // from this point till we see a }, we will call next_token() to read
                    // regular token to us, instead of a string
                    let mut saw_rcurly = false;
                    while !self.src.is_eof() {
                        if let Ok(tok) = self.next_token() {
                            span = span.combine(tok.get_source_ref());
                            saw_rcurly = matches!(tok, Token::RCurly(_));
                            parts.push(tok);
                            if saw_rcurly {
                                break;
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
                    buf.push(c);
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

    // a multi line string fragment is a string fragment that is
    // preceded by "---". It goes till the end of the line
    fn lex_multi_line_string_fragment(&mut self) -> Result<Token, LexError> {
        let mut content = String::new();
        let mut span = self.src.get_ref();
        // skip the remaining 2 '-' characters out of the 3
        // the first one was already consumed by lex_operator()
        self.src.next_char();
        self.src.next_char();
        self.src.next_char();

        span = span.combine(self.src.get_ref());

        while !self.src.is_eof() {
            let cur = self.src.cur_char();
            span = span.combine(self.src.get_ref());
            self.src.next_char();
            if cur != '\n' {
                content.push(cur);
                continue;
            } else {
                break;
            }
        }
        Ok(Token::MultiLineStringFragment(span, content))
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
            "true" => Ok(Token::True(combined_ref)),
            "false" => Ok(Token::False(combined_ref)),
            "break" => Ok(Token::Break(combined_ref)),
            "continue" => Ok(Token::Continue(combined_ref)),
            "return" => Ok(Token::Return(combined_ref)),
            "pub" => Ok(Token::Pub(combined_ref)),
            "print" => Ok(Token::Print(combined_ref)),
            "println" => Ok(Token::Println(combined_ref)),
            "some" => Ok(Token::Some(combined_ref)),
            "none" => Ok(Token::None(combined_ref)),

            "i8" => Ok(Token::I8(combined_ref)),
            "i16" => Ok(Token::I16(combined_ref)),
            "i32" => Ok(Token::I32(combined_ref)),
            "i64" => Ok(Token::I64(combined_ref)),
            "int" => Ok(Token::Int(combined_ref)),
            "u8" => Ok(Token::U8(combined_ref)),
            "u16" => Ok(Token::U16(combined_ref)),
            "u32" => Ok(Token::U32(combined_ref)),
            "u64" => Ok(Token::U64(combined_ref)),
            "uint" => Ok(Token::UInt(combined_ref)),
            "bool" => Ok(Token::Bool(combined_ref)),
            "char" => Ok(Token::Char(combined_ref)),
            "str" => Ok(Token::Str(combined_ref)),
            "type" => Ok(Token::Type(combined_ref)),
            "void" => Ok(Token::Void(combined_ref)),
            "struct" => Ok(Token::Struct(combined_ref)),
            "mod" => Ok(Token::Mod(combined_ref)),
            "_" => Ok(Token::Underscore(combined_ref)),
            _ => Ok(Token::Identifier(id, combined_ref)),
        }
    }

    // lex an operator
    fn lex_operator(&mut self) -> Result<Token, LexError> {
        let cur_ref = self.src.get_ref();
        let cur = self.src.cur_char();

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
                // if the next character is a '/', lex a comment
                if self.src.peek_char() == '/' {
                    return self.lex_comment();
                }
                self.src.next_char();
                Ok(Token::Slash(cur_ref.combine(self.src.get_ref())))
            }
            '%' => {
                self.src.next_char();
                Ok(Token::Modulo(cur_ref.combine(self.src.get_ref())))
            }

            '@' => {
                self.src.next_char();
                Ok(Token::At(cur_ref.combine(self.src.get_ref())))
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
                Ok(Token::Not(cur_ref.combine(self.src.get_ref())))
            }
            '&' => {
                // if the next character is a '&', return a And operator
                let c = self.src.peek_char();
                if c == '&' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(Token::And(cur_ref.combine(self.src.get_ref())));
                }
                // otherwise, return an error
                Err(LexError::InvalidCharacter(
                    cur_ref.combine(self.src.get_ref()),
                ))
            }
            '|' => {
                // if the next character is a '|', return a Or operator
                let c = self.src.peek_char();
                if c == '|' {
                    self.src.next_char();
                    self.src.next_char();
                    return Ok(Token::Or(cur_ref.combine(self.src.get_ref())));
                }
                // otherwise, return an error
                Err(LexError::InvalidCharacter(
                    cur_ref.combine(self.src.get_ref()),
                ))
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
            '?' => {
                self.src.next_char();
                Ok(Token::QuestionMark(cur_ref.combine(self.src.get_ref())))
            }
            _ => unreachable!("invalid operator character: '{}' at {:?}", cur, cur_ref),
        }
    }

    fn lex_string(&mut self) -> Result<Token, LexError> {
        let mut span = self.src.get_ref();
        let mut content = String::new();

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
        if self.src.is_eof() || self.src.cur_char() == '\n' {
            Err(LexError::UnterminatedStringLiteral(span))
        } else {
            self.src.next_char();
            span = span.combine(self.src.get_ref());
            Ok(Token::SingleLineStringLiteral(span, content))
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

    fn lex_number(&mut self) -> Result<Token, LexError> {
        let mut number = String::new();
        let cur_ref = self.src.get_ref();
        let cur = self.src.cur_char();
        number.push(cur);

        // read all characters that are digits
        let mut end_ref;
        loop {
            let c = self.src.next_char();
            end_ref = self.src.get_ref();
            if c.is_ascii_digit() {
                number.push(c);
            } else if c == '_' {
                // ignore underscores
                continue;
            } else {
                break;
            }
        }

        let combined_ref = cur_ref.combine(end_ref);

        // TODO: if the next character is a '.', try to lex a float

        Ok(Token::NumberLiteral(number, combined_ref))
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
