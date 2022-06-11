package lexer

type TokenType string

// TODO: Add span to Tokens
type ProtoToken struct {
	Type    TokenType
	Literal string
	Span    Span
}

type Span struct {
	line  int
	col   int
	start int
	end   int
}

// all lexable tokens
const (
	ERROR = "ERROR"
	END   = "EOF"

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
	NOT = "NOT"

	IS_EQUAL_TO  = "=="
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
	WHILE    = "WHILE"

	STR_TYPE  = "STRING_TYPE"
	CHAR_TYPE = "CHAR_TYPE"
	BOOL_TYPE = "BOOL_TYPE"
	I64_TYPE  = "I64_TYPE"
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
	"while":    WHILE,
	"loop":     LOOP,
	"str":      STR_TYPE,
	"char":     CHAR_TYPE,
	"bool":     BOOL_TYPE,
	"i64":      I64_TYPE,
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
