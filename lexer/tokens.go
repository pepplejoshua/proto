package lexer

type TokenType string

// TODO: Add span to Tokens
type ProtoToken struct {
	Type      TokenType
	Literal   string
	TokenSpan Span
}

type Span struct {
	Line  int
	Col   int
	Start int
	End   int
}

// all lexable tokens
const (
	ERROR = "ERROR"
	END   = "EOF"

	IDENT  = "IDENT"
	I64    = "I64"
	STRING = "STRING"
	CHAR   = "CHAR"

	REF          = "&"
	ASSIGN       = "="
	PLUS         = "+"
	PLUS_EQUAL   = "+="
	MINUS        = "-"
	MINUS_EQUAL  = "-="
	STAR         = "*"
	STAR_EQUAL   = "*="
	SLASH        = "/"
	SLASH_EQUAL  = "/="
	MODULO       = "%"
	MODULO_EQUAL = "%="
	APOSTROPHE   = "'"
	PIPE         = "|"
	HASH         = "#"
	CPP_CODE     = "CPP_CODE"
	CPP          = "cpp"
	PUB          = "pub"

	LESS_THAN        = "<"
	GREATER_THAN     = ">"
	LESS_OR_EQUAL    = "<="
	GREATER_OR_EQUAL = ">="
	ARROW            = "->"

	AND = "&&"
	OR  = "||"
	NOT = "NOT"

	IS_EQUAL_TO  = "=="
	NOT_EQUAL_TO = "!="

	USE      = "use"
	PATH_SEP = "::"
	AS       = "as"
	MOD      = "mod"

	COMMA           = ","
	DOT             = "."
	VARIAD          = "..."
	RANGE           = ".."
	INCLUSIVE_RANGE = "..="
	SEMI_COLON      = ";"
	COLON           = ":"
	QUESTION_MARK   = "?"

	OPEN_PAREN    = "("
	CLOSE_PAREN   = ")"
	OPEN_BRACKET  = "["
	CLOSE_BRACKET = "]"
	OPEN_CURLY    = "{"
	CLOSE_CURLY   = "}"

	FN       = "fn"
	LET      = "let"
	MUT      = "mut"
	TRUE     = "TRUE"
	FALSE    = "FALSE"
	IF       = "IF"
	ELSE     = "ELSE"
	RETURN   = "RETURN"
	STRUCT   = "STRUCT"
	BREAK    = "BREAK"
	CONTINUE = "CONTINUE"
	FOR      = "FOR"
	LOOP     = "LOOP"
	IN       = "IN"
	WHILE    = "WHILE"

	RANGE_TYPE  = "RANGE_TYPE"
	STRING_TYPE = "STRING_TYPE"
	CHAR_TYPE   = "CHAR_TYPE"
	BOOL_TYPE   = "BOOL_TYPE"
	I64_TYPE    = "I64_TYPE"
)

var keywords = map[string]TokenType{
	"fn":       FN,
	"let":      LET,
	"mut":      MUT,
	"true":     TRUE,
	"false":    FALSE,
	"if":       IF,
	"else":     ELSE,
	"return":   RETURN,
	"not":      NOT,
	"struct":   STRUCT,
	"break":    BREAK,
	"continue": CONTINUE,
	"for":      FOR,
	"in":       IN,
	"while":    WHILE,
	"loop":     LOOP,
	"str":      STRING_TYPE,
	"char":     CHAR_TYPE,
	"bool":     BOOL_TYPE,
	"i64":      I64_TYPE,
	"Range":    RANGE_TYPE,
	"use":      USE,
	"as":       AS,
	"mod":      MOD,
	"cpp":      CPP,
	"pub":      PUB,
}

func CheckPotentialKeyword(candidate string) TokenType {
	if tok, ok := keywords[candidate]; ok {
		return tok
	}
	return IDENT
}

func is_digit(char byte) bool {
	return '0' <= char && char <= '9'
}

func is_alphabet(char byte) bool {
	return 'a' <= char && char <= 'z' ||
		'A' <= char && char <= 'Z'
}
