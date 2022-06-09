package lexer

type TokenType string

// TODO: Add span to Tokens
type ProtoToken struct {
	Type    TokenType
	Literal string
}

// all lexable tokens
const (
	ERROR        = "ERROR"
	INVALID_I64  = "INVALID_I64"
	UNTERMINATED = "UNTERMINATEDs"
	END          = "EOF"

	IDENT  = "IDENT"
	I64    = "I64"
	STRING = "STRING"
	CHAR   = "CHAR"

	ASSIGN     = "="
	PLUS       = "+"
	MINUS      = "-"
	STAR       = "*"
	SLASH      = "/"
	MODULO     = "%"
	APOSTROPHE = "'"

	LESS_THAN        = "<"
	GREATER_THAN     = ">"
	LESS_OR_EQUAL    = "<="
	GREATER_OR_EQUAL = ">="

	AND = "&&"
	OR  = "||"
	NOT = "not"

	EQUAL_TO     = "=="
	NOT_EQUAL_TO = "!="

	COMMA         = ","
	DOT           = "."
	SEMI_COLON    = ";"
	COLON         = ":"
	QUESTION_MARK = "?"

	OPEN_PAREN    = "("
	CLOSE_PAREN   = ")"
	OPEN_BRACKET  = "["
	CLOSE_BRACKET = "]"
	OPEN_CURLY    = "{"
	CLOSE_CURLY   = "}"

	FN     = "fn"
	LET    = "let"
	MUT    = "mut"
	TRUE   = "true"
	FALSE  = "FALSE"
	IF     = "IF"
	ELSE   = "ELSE"
	RETURN = "RETURN"
)

var keywords = map[string]TokenType{
	"fn":     FN,
	"let":    LET,
	"mut":    MUT,
	"true":   TRUE,
	"false":  FALSE,
	"if":     IF,
	"else":   ELSE,
	"return": RETURN,
	"not":    NOT,
}

func CheckPotentialKeyword(candidate string) TokenType {
	if tok, ok := keywords[candidate]; ok {
		return tok
	}
	return IDENT
}

func make_singlechar_token(tokentype TokenType, char byte) ProtoToken {
	return ProtoToken{
		Type:    tokentype,
		Literal: string(char),
	}
}

func make_token(tokentype TokenType, literal string) ProtoToken {
	return ProtoToken{
		Type:    tokentype,
		Literal: literal,
	}
}

func is_digit(char byte) bool {
	return '0' <= char && char <= '9'
}

func is_alphabet(char byte) bool {
	return 'a' <= char && char <= 'z' ||
		'A' <= char && char <= 'Z'
}
