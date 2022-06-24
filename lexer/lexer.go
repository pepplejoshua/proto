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
		column:   0,
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
	l.column = 0
}

func (l *Lexer) next_char() {
	if l.peek_pos >= len(l.source) {
		l.cur_byte = 0
	} else {
		l.cur_byte = l.source[l.peek_pos]
	}
	l.pos = l.peek_pos
	l.peek_pos += 1
	l.column += 1
}

func (l *Lexer) peek_char() byte {
	if l.peek_pos >= len(l.source) {
		return 0
	} else {
		return l.source[l.peek_pos]
	}
}

func (l *Lexer) skip_comments_and_whitespace() {
	for !l.is_at_end() {
		if l.cur_byte == ' ' ||
			l.cur_byte == '\t' ||
			l.cur_byte == '\n' ||
			l.cur_byte == '\r' {
			if l.cur_byte == '\n' {
				l.advance_line()
			}
			l.next_char()
		} else if l.cur_byte == '/' { // if we have seen a /
			if l.peek_char() == '/' { // and seen another /, then single line comment
				l.next_char() // move to the second /
				for !l.is_at_end() && l.cur_byte != '\n' {
					l.next_char()
				}
				l.advance_line()
				l.next_char()
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
					} else if l.cur_byte == '\n' { // register a newline and keep moving
						l.advance_line()
						l.next_char()
					} else { // skip past comment content
						l.next_char()
					}
				}
			} else {
				return
			}
		} else { // we are not at a comment so return
			return
		}
	}
}

func (l *Lexer) Next_Token() ProtoToken {
	l.skip_comments_and_whitespace()
	var token ProtoToken
	if l.is_at_end() {
		token = l.make_token(END, "EOF")
		token.TokenSpan = Span{
			Line:  l.line,
			Col:   l.column,
			Start: l.pos,
			End:   l.pos,
		}
		return token
	}

	cur_char := l.cur_byte
	switch cur_char {
	case '=':
		if l.peek_char() == '=' {
			l.next_char() // go to second =
			l.next_char() // skip past second =
			token = l.make_token(IS_EQUAL_TO, "==")
		} else {
			l.next_char() // skip past the =
			token = l.make_singlechar_token(ASSIGN, cur_char)
		}
	case '!':
		if l.peek_char() == '=' {
			l.next_char()
			l.next_char()
			token = l.make_token(NOT_EQUAL_TO, "!=")
		} else { // erroneous
			l.next_char()
			token = l.make_singlechar_token(ERROR, cur_char)
		}
	case '+':
		if l.peek_char() == '=' {
			l.next_char()
			l.next_char()
			token = l.make_token(PLUS_EQUAL, "+=")
		} else {
			l.next_char()
			token = l.make_singlechar_token(PLUS, cur_char)
		}
	case '-': // TODO: allow negative number creation
		if l.peek_char() == '=' {
			l.next_char()
			l.next_char()
			token = l.make_token(MINUS_EQUAL, "-=")
		} else if l.peek_char() == '>' {
			l.next_char()
			l.next_char()
			token = l.make_token(ARROW, "->")
		} else {
			l.next_char()
			token = l.make_singlechar_token(MINUS, cur_char)
		}
	case '*':
		if l.peek_char() == '=' {
			l.next_char()
			l.next_char()
			token = l.make_token(STAR_EQUAL, "*=")
		} else {
			l.next_char()
			token = l.make_singlechar_token(STAR, cur_char)
		}
	case '/':
		if l.peek_char() == '=' {
			l.next_char()
			l.next_char()
			token = l.make_token(SLASH_EQUAL, "/=")
		} else {
			l.next_char()
			token = l.make_singlechar_token(SLASH, cur_char)
		}
	case '%':
		if l.peek_char() == '=' {
			l.next_char()
			l.next_char()
			token = l.make_token(MODULO_EQUAL, "%=")
		} else {
			l.next_char()
			token = l.make_singlechar_token(MODULO, cur_char)
		}
	case '\'': // read character and handle errors
		token = l.read_character()
	case '"': // read string and handle errors
		token = l.read_string()
	case '<':
		if l.peek_char() == '=' {
			l.next_char() // go to =
			l.next_char() // skip past =
			token = l.make_token(LESS_OR_EQUAL, "<=")
		} else {
			l.next_char() // skip past the =
			token = l.make_singlechar_token(LESS_THAN, cur_char)
		}
	case '>':
		if l.peek_char() == '=' {
			l.next_char() // go to =
			l.next_char() // skip past =
			token = l.make_token(GREATER_OR_EQUAL, ">=")
		} else {
			l.next_char() // skip past the =
			token = l.make_singlechar_token(GREATER_THAN, cur_char)
		}
	case '&':
		if l.peek_char() == '&' {
			l.next_char()
			l.next_char()
			token = l.make_token(AND, "&&")
		} else { // erroneous
			l.next_char() // skip past |
			token = l.make_singlechar_token(ERROR, cur_char)
		}
	case '|':
		if l.peek_char() == '|' {
			l.next_char()
			l.next_char()
			token = l.make_token(OR, "||")
		} else {
			l.next_char() // skip past |
			token = l.make_singlechar_token(PIPE, cur_char)
		}
	case ',':
		l.next_char()
		token = l.make_singlechar_token(COMMA, cur_char)
	case '.':
		if l.peek_char() == '.' {
			l.next_char()
			l.next_char()

			if l.cur_byte == '=' {
				l.next_char()
				token = ProtoToken{
					Type:    INCLUSIVE_RANGE,
					Literal: "..=",
					TokenSpan: Span{
						Line:  l.line,
						Col:   l.column - 3,
						Start: l.pos - 3,
						End:   l.peek_pos,
					},
				}
			} else {
				token = l.make_token(RANGE, "..")
			}
		} else {
			l.next_char()
			token = l.make_singlechar_token(DOT, cur_char)
		}
	case ';':
		l.next_char()
		token = l.make_singlechar_token(SEMI_COLON, cur_char)
	case ':':
		l.next_char()
		token = l.make_singlechar_token(COLON, cur_char)
	case '?':
		l.next_char()
		token = l.make_singlechar_token(QUESTION_MARK, cur_char)
	case '(':
		l.next_char()
		token = l.make_singlechar_token(OPEN_PAREN, cur_char)
	case ')':
		l.next_char()
		token = l.make_singlechar_token(CLOSE_PAREN, cur_char)
	case '[':
		l.next_char()
		token = l.make_singlechar_token(OPEN_BRACKET, cur_char)
	case ']':
		l.next_char()
		token = l.make_singlechar_token(CLOSE_BRACKET, cur_char)
	case '{':
		l.next_char()
		token = l.make_singlechar_token(OPEN_CURLY, cur_char)
	case '}':
		l.next_char()
		token = l.make_singlechar_token(CLOSE_CURLY, cur_char)
	default: // handle digits, keywords and identifiers
		if is_alphabet(cur_char) || cur_char == '_' { // check for identifier/keywords
			token = l.read_identifier()
		} else if is_digit(cur_char) {
			// handle _ error later
			token = l.read_number()
		} else {
			l.next_char()
			token = l.make_singlechar_token(ERROR, cur_char)
		}
	}

	return token
}

func (l *Lexer) make_token(tokentype TokenType, literal string) ProtoToken {
	return ProtoToken{
		Type:    tokentype,
		Literal: literal,
		TokenSpan: Span{
			Line:  l.line,
			Col:   l.column - 2,
			Start: l.pos - 2,
			End:   l.peek_pos,
		},
	}
}

func (l *Lexer) make_singlechar_token(tokentype TokenType, char byte) ProtoToken {
	return ProtoToken{
		Type:    tokentype,
		Literal: string(char),
		TokenSpan: Span{
			Line:  l.line,
			Col:   l.column - 1,
			Start: l.pos - 1,
			End:   l.peek_pos,
		},
	}
}

func (l *Lexer) read_string() ProtoToken {
	start := l.pos
	col := l.column
	l.next_char()
	for !l.is_at_end() && l.cur_byte != '"' {
		ch := l.cur_byte
		if ch == '\\' && l.peek_char() == '"' { // an escaped char
			l.next_char() // move to the ", will be skipped in the next_char() call below
		}
		if l.cur_byte == '\n' { // no multiline strings
			break
		}
		l.next_char()
	}

	if l.is_at_end() || l.cur_byte != '"' {
		return ProtoToken{
			Type:    ERROR,
			Literal: "Unterminated string literal",
			TokenSpan: Span{
				Line:  l.line,
				Col:   l.column - 1,
				Start: start,
				End:   l.pos,
			},
		}
	}
	l.next_char()
	slice := l.source[start:l.pos]
	return ProtoToken{
		Type:    STRING,
		Literal: slice,
		TokenSpan: Span{
			Line:  l.line,
			Col:   col,
			Start: start,
			End:   l.pos,
		},
	}
}

func (l *Lexer) read_character() ProtoToken {
	start := l.pos
	col := l.column
	l.next_char()

	if l.cur_byte == '\'' { //empty char
		l.next_char()
		return ProtoToken{
			Type:    CHAR,
			Literal: "",
			TokenSpan: Span{
				Line:  l.line,
				Col:   col,
				Start: start,
				End:   l.pos,
			},
		}
	} else { // contains some character
		char := l.cur_byte
		l.next_char()
		if l.cur_byte != '\'' { // character is either too long or unterminated
			return ProtoToken{
				Type:    ERROR,
				Literal: "Character literal unterminated (or contains more than 1 character)",
				TokenSpan: Span{
					Line:  l.line,
					Col:   col,
					Start: start,
					End:   l.pos,
				},
			}
		} else {
			l.next_char()
			return ProtoToken{
				Type:    CHAR,
				Literal: string(char),
				TokenSpan: Span{
					Line:  l.line,
					Col:   col,
					Start: start,
					End:   l.pos,
				},
			}
		}
	}
}

func (l *Lexer) read_identifier() ProtoToken {
	start := l.pos
	col := l.column
	l.next_char() // move to next character
	for !l.is_at_end() &&
		(is_alphabet(l.cur_byte) ||
			is_digit(l.cur_byte) ||
			l.cur_byte == '_') {
		l.next_char()
	}
	slice := l.source[start:l.pos]
	tokentype := CheckPotentialKeyword(slice)
	return ProtoToken{
		Type:    tokentype,
		Literal: slice,
		TokenSpan: Span{
			Line:  l.line,
			Col:   col,
			Start: start,
			End:   l.pos,
		},
	}
}

func (l *Lexer) read_number() ProtoToken {
	start := l.pos
	col := l.column
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
			TokenSpan: Span{
				Line:  l.line,
				Col:   col,
				Start: start,
				End:   l.pos,
			},
		}
	} else {
		return ProtoToken{
			Type:    ERROR,
			Literal: slice + " isn't i64",
			TokenSpan: Span{
				Line:  l.line,
				Col:   col,
				Start: start,
				End:   l.pos,
			},
		}
	}
}
