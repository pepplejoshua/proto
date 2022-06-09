package lexer

import "strconv"

type Lexer struct {
	source   string
	pos      int
	peek_pos int
	line     int
	column   int
	cur_byte byte
}

func New(src string) *Lexer {
	l := &Lexer{
		source:   src,
		pos:      0,
		peek_pos: 0,
		line:     1,
		column:   1,
		cur_byte: 0,
	}
	l.next_char()
	return l
}

func (l *Lexer) is_at_end() bool {
	return l.pos >= len(l.source)
}

func (l *Lexer) advance_line() {
	l.line += 1
	l.column = 1
}

func (l *Lexer) next_char() {
	if l.peek_pos >= len(l.source) {
		l.cur_byte = 0
	} else {
		l.cur_byte = l.source[l.peek_pos]
	}
	l.pos = l.peek_pos
	l.peek_pos += 1
}

func (l *Lexer) peek_char() byte {
	if l.peek_pos >= len(l.source) {
		return 0
	} else {
		return l.source[l.peek_pos]
	}
}

func (l *Lexer) skip_comments() {
	for !l.is_at_end() {
		if l.cur_byte == '/' { // if we have seen a /
			if l.peek_char() == '/' { // and seen another /, then single line comment
				l.next_char() // move to the second /
				for !l.is_at_end() && l.cur_byte != '\n' {
					l.next_char()
				}
			} else if l.peek_char() == '*' { // multi line
				l.next_char() // move to the *
				l.next_char() // move past the *
				for !l.is_at_end() {
					if l.cur_byte == '*' {
						// we might be potentially at the end of the comment
						if l.peek_char() == '/' {
							// we are at the end of the comment
							l.next_char() // advance to the / character
							l.next_char() // advance past the / character
							break
						} else { // we are not at the end of the loop so move past the *
							l.next_char()
						}
					} else if l.peek_char() == '\n' { // register a newline and keep moving
						l.advance_line()
					} else { // skip past comment content
						l.next_char()
					}
				}
			}
		} else { // we are not at a comment so return
			return
		}
	}
}

func (l *Lexer) skip_whitespace() {
	for l.cur_byte == ' ' ||
		l.cur_byte == '\t' ||
		l.cur_byte == '\n' ||
		l.cur_byte == '\r' {
		if l.cur_byte == '\n' {
			l.advance_line()
		}
		l.next_char()
	}
}

func (l *Lexer) Next_Token() ProtoToken {
	var token ProtoToken

	l.skip_whitespace()
	l.skip_comments()

	switch l.cur_byte {
	case '=':
		if l.peek_char() == '=' {
			l.next_char() // go to second =
			l.next_char() // skip past second =
			token = make_token(EQUAL_TO, "==")
		} else {
			token = make_singlechar_token(ASSIGN, l.cur_byte)
			l.next_char() // skip past the =
		}
	case '!':
		if l.peek_char() == '=' {
			l.next_char()
			l.next_char()
			token = make_token(NOT_EQUAL_TO, "!=")
		} else { // erroneous
			token = make_singlechar_token(ERROR, l.cur_byte)
			l.next_char()
		}
	case '+':
		token = make_singlechar_token(PLUS, l.cur_byte)
		l.next_char()
	case '-': // TODO: allow negative number creation
		token = make_singlechar_token(MINUS, l.cur_byte)
		l.next_char()
	case '*':
		token = make_singlechar_token(STAR, l.cur_byte)
		l.next_char()
	case '/':
		token = make_singlechar_token(SLASH, l.cur_byte)
		l.next_char()
	case '%':
		token = make_singlechar_token(MODULO, l.cur_byte)
		l.next_char()
	case '\'': // read character and handle errors
		token, _ = l.read_char()
	case '"': // read string and handle errors
		token, _ = l.read_string()
	case '<':
		if l.peek_char() == '=' {
			l.next_char() // go to =
			l.next_char() // skip past =
			token = make_token(LESS_OR_EQUAL, "<=")
		} else {
			token = make_singlechar_token(LESS_THAN, l.cur_byte)
			l.next_char() // skip past the =
		}
	case '>':
		if l.peek_char() == '=' {
			l.next_char() // go to =
			l.next_char() // skip past =
			token = make_token(GREATER_OR_EQUAL, "<=")
		} else {
			token = make_singlechar_token(GREATER_THAN, l.cur_byte)
			l.next_char() // skip past the =
		}
	case '&':
		if l.peek_char() == '&' {
			l.next_char()
			l.next_char()
			token = make_token(AND, "&&")
		} else { // erroneous
			token = make_singlechar_token(ERROR, l.cur_byte)
			l.next_char() // skip past |
		}
	case '|':
		if l.peek_char() == '|' {
			l.next_char()
			l.next_char()
			token = make_token(OR, "||")
		} else { // erroneous
			token = make_singlechar_token(ERROR, l.cur_byte)
			l.next_char() // skip past |
		}
	case ',':
		token = make_singlechar_token(COMMA, l.cur_byte)
		l.next_char()
	case '.':
		token = make_singlechar_token(DOT, l.cur_byte)
		l.next_char()
	case ';':
		token = make_singlechar_token(SEMI_COLON, l.cur_byte)
		l.next_char()
	case ':':
		token = make_singlechar_token(COLON, l.cur_byte)
		l.next_char()
	case '?':
		token = make_singlechar_token(QUESTION_MARK, l.cur_byte)
		l.next_char()
	case '(':
		token = make_singlechar_token(OPEN_PAREN, l.cur_byte)
		l.next_char()
	case ')':
		token = make_singlechar_token(CLOSE_PAREN, l.cur_byte)
		l.next_char()
	case '[':
		token = make_singlechar_token(OPEN_BRACKET, l.cur_byte)
		l.next_char()
	case ']':
		token = make_singlechar_token(CLOSE_BRACKET, l.cur_byte)
		l.next_char()
	case '{':
		token = make_singlechar_token(OPEN_CURLY, l.cur_byte)
		l.next_char()
	case '}':
		token = make_singlechar_token(CLOSE_CURLY, l.cur_byte)
		l.next_char()
	case 0:
		token = make_token(END, "EOF")
	default: // handle digits, keywords and identifiers
		if is_alphabet(l.cur_byte) { // check for identifier/keywords
			token = l.read_identifier()
		} else if is_digit(l.cur_byte) {
			// handle _ error later
			token, _ = l.read_number()
		} else {
			token = make_singlechar_token(ERROR, l.cur_byte)
			l.next_char()
		}
	}

	return token
}

func (l *Lexer) read_string() (ProtoToken, string) {
	start := l.pos
	l.read_char()
	for !l.is_at_end() && l.cur_byte != '"' {
		if l.cur_byte == '\\' && l.peek_char() == '"' { // an escaped char
			l.read_char() // move to the ", which will be skipped in next codeline
		}
		if l.cur_byte == '\n' { // no multiline strings
			break
		}
		l.read_char()
	}

	slice := l.source[start:l.pos]
	if l.cur_byte != '"' { //unterminated string
		return ProtoToken{
			Type:    UNTERMINATED,
			Literal: slice,
		}, "Unterminated string literal"
	} else {
		l.next_char()
		return ProtoToken{
			Type:    STRING,
			Literal: slice + "\"",
		}, ""
	}
}

func (l *Lexer) read_char() (ProtoToken, string) {
	l.next_char()
	start := l.pos

	if l.cur_byte == '\'' { //empty char
		return ProtoToken{
			Type:    CHAR,
			Literal: "",
		}, ""
	} else { // contains some character
		char := l.cur_byte
		l.next_char()
		if l.cur_byte != '\'' { // string is either too long or unterminated character
			return ProtoToken{
				Type:    UNTERMINATED,
				Literal: l.source[start:l.pos],
			}, "Character literal unterminated (or contains more than 1 character)"
		} else {
			return ProtoToken{
				Type:    CHAR,
				Literal: string(char),
			}, ""
		}
	}
}

func (l *Lexer) read_identifier() ProtoToken {
	start := l.pos
	l.next_char() // move to next character
	for is_alphabet(l.cur_byte) || is_digit(l.cur_byte) {
		l.next_char()
	}
	slice := l.source[start:l.pos]
	tokentype := CheckPotentialKeyword(slice)
	return ProtoToken{
		Type:    tokentype,
		Literal: slice,
	}
}

func (l *Lexer) read_number() (ProtoToken, string) {
	start := l.pos
	l.next_char()
	for is_digit(l.cur_byte) {
		l.next_char()
	}
	slice := l.source[start:l.pos]

	// check if number is in i64 range
	if _, err := strconv.ParseInt(slice, 10, 64); err == nil {
		return ProtoToken{
			Type:    I64,
			Literal: slice,
		}, ""
	} else {
		return ProtoToken{
				Type:    INVALID_I64,
				Literal: slice,
			},
			"Number isn't i64"
	}
}
