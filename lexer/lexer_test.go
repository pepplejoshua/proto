package lexer

import (
	"fmt"
	"io/ioutil"
	"log"
	"testing"
)

type Pair struct {
	a, b interface{}
}

func ReadFile(path string) string {
	src, err := ioutil.ReadFile(path)
	if err != nil {
		log.Fatal(err)
	}
	return string(src)
}

func TestLexingAllTokens(t *testing.T) {
	path := "../samples/test_sources/lexer/valid/all_tokens.pr"
	source := ReadFile(path)

	lexer := New(source)

	for {
		token := lexer.Next_Token()
		if token.Type == ERROR {
			msg := "Ran into an error: " + token.Literal
			msg = msg + " {" + fmt.Sprint(token.Span.line) + ":" + fmt.Sprint(token.Span.col) + "}"
			log.Fatal(msg)
		} else {
			msg := string(token.Type) + " {" + token.Literal + "}"
			msg = msg + " at " + fmt.Sprint(token.Span.line) + ":" + fmt.Sprint(token.Span.col)
			println(msg)

			if token.Type == END {
				break
			}
		}
	}
}

func TestLexingComments(t *testing.T) {
	path := "../samples/test_sources/lexer/valid/comments.pr"
	source := ReadFile(path)

	lexer := New(source)

	token := lexer.Next_Token()

	if token.Type != END {
		log.Fatal("Expected EOF token but got ",
			token.Type, " {",
			token.Span.line, ":",
			token.Span.col, "}")
	}
}

func TestLexingDoubleOperators(t *testing.T) {
	path := "../samples/test_sources/lexer/valid/double_operators.pr"
	source := ReadFile(path)

	lexer := New(source)

	expected := []Pair{
		{LESS_OR_EQUAL, "<="},
		{GREATER_OR_EQUAL, ">="},
		{AND, "&&"},
		{OR, "||"},
		{IS_EQUAL_TO, "=="},
		{NOT_EQUAL_TO, "!="},
	}

	for _, checkable := range expected {
		token := lexer.Next_Token()

		if token.Type != TokenType(fmt.Sprint(checkable.a)) {
			log.Fatalf("Expected Type [%s] but got [%s]", checkable.a, token.Type)
		}

		if token.Literal != checkable.b {
			log.Fatalf("Expected Literal [%s] but got [%s]", checkable.b, token.Literal)
		}
	}

	token := lexer.Next_Token()

	if token.Type != END {
		log.Fatal("Expected EOF token but got ",
			token.Type, " {",
			token.Span.line, ":",
			token.Span.col, "}")
	}
}

func TestLexingSingleOperators(t *testing.T) {
	path := "../samples/test_sources/lexer/valid/single_operators.pr"
	source := ReadFile(path)

	lexer := New(source)

	expected := []Pair{
		{ASSIGN, "="},
		{PLUS, "+"},
		{MINUS, "-"},
		{STAR, "*"},
		{SLASH, "/"},
		{DOT, "."},
		{MODULO, "%"},
		{COMMA, ","},
		{COLON, ":"},
		{SEMI_COLON, ";"},
		{GREATER_THAN, ">"},
		{LESS_THAN, "<"},
		{QUESTION_MARK, "?"},
		{NOT, "not"},
	}

	for _, checkable := range expected {
		token := lexer.Next_Token()

		if token.Type != TokenType(fmt.Sprint(checkable.a)) {
			log.Fatalf("Expected Type [%s] but got [%s]", checkable.a, token.Type)
		}

		if token.Literal != checkable.b {
			log.Fatalf("Expected Literal [%s] but got [%s]", checkable.b, token.Literal)
		}
	}

	token := lexer.Next_Token()

	if token.Type != END {
		log.Fatal("Expected EOF token but got ",
			token.Type, " {",
			token.Span.line, ":",
			token.Span.col, "}")
	}
}

func TestLexingTypeTokens(t *testing.T) {
	path := "../samples/test_sources/lexer/valid/type_tokens.pr"
	source := ReadFile(path)

	lexer := New(source)

	expected := []Pair{
		{I64_TYPE, "i64"},
		{CHAR_TYPE, "char"},
		{STRING_TYPE, "str"},
		{BOOL_TYPE, "bool"},
	}

	for _, checkable := range expected {
		token := lexer.Next_Token()

		if token.Type != TokenType(fmt.Sprint(checkable.a)) {
			log.Fatalf("Expected Type [%s] but got [%s]", checkable.a, token.Type)
		}

		if token.Literal != checkable.b {
			log.Fatalf("Expected Literal [%s] but got [%s]", checkable.b, token.Literal)
		}
	}

	token := lexer.Next_Token()

	if token.Type != END {
		log.Fatal("Expected EOF token but got ",
			token.Type, " {",
			token.Span.line, ":",
			token.Span.col, "}")
	}
}

func TestLexingGroupingTokens(t *testing.T) {
	path := "../samples/test_sources/lexer/valid/grouping_tokens.pr"
	source := ReadFile(path)

	lexer := New(source)

	expected := []Pair{
		{OPEN_PAREN, "("},
		{CLOSE_PAREN, ")"},
		{OPEN_CURLY, "{"},
		{CLOSE_CURLY, "}"},
		{OPEN_BRACKET, "["},
		{CLOSE_BRACKET, "]"},
	}

	for _, checkable := range expected {
		token := lexer.Next_Token()

		if token.Type != TokenType(fmt.Sprint(checkable.a)) {
			log.Fatalf("Expected Type [%s] but got [%s]", checkable.a, token.Type)
		}

		if token.Literal != checkable.b {
			log.Fatalf("Expected Literal [%s] but got [%s]", checkable.b, token.Literal)
		}
	}

	token := lexer.Next_Token()

	if token.Type != END {
		log.Fatal("Expected EOF token but got ",
			token.Type, " {",
			token.Span.line, ":",
			token.Span.col, "}")
	}
}

func TestLexingIdentifiers(t *testing.T) {
	path := "../samples/test_sources/lexer/valid/identifier_tokens.pr"
	source := ReadFile(path)

	lexer := New(source)

	expected := []Pair{
		{IDENT, "someidentifier"},
		{IDENT, "another_identifier"},
		{IDENT, "a"},
		{IDENT, "c"},
		{IDENT, "_d"},
		{IDENT, "_identifier"},
		{IDENT, "a1b2c3d4e5f6"},
	}

	for _, checkable := range expected {
		token := lexer.Next_Token()

		if token.Type != TokenType(fmt.Sprint(checkable.a)) {
			log.Fatalf("Expected Type [%s] but got [%s]", checkable.a, token.Type)
		}

		if token.Literal != checkable.b {
			log.Fatalf("Expected Literal [%s] but got [%s]", checkable.b, token.Literal)
		}
	}

	token := lexer.Next_Token()

	if token.Type != END {
		log.Fatal("Expected EOF token but got ",
			token.Type, " {",
			token.Span.line, ":",
			token.Span.col, "}")
	}
}

func TestLexingKeywords(t *testing.T) {
	path := "../samples/test_sources/lexer/valid/keyword_tokens.pr"
	source := ReadFile(path)

	lexer := New(source)

	expected := []Pair{
		{LET, "let"},
		{MUT, "mut"},
		{STRUCT, "struct"},
		{FN, "fn"},
		{FOR, "for"},
		{IF, "if"},
		{LOOP, "loop"},
		{WHILE, "while"},
		{ELSE, "else"},
		{BREAK, "break"},
		{CONTINUE, "continue"},
		{RETURN, "return"},
		{NOT, "not"},
	}

	for _, checkable := range expected {
		token := lexer.Next_Token()

		if token.Type != TokenType(fmt.Sprint(checkable.a)) {
			log.Fatalf("Expected Type [%s] but got [%s]", checkable.a, token.Type)
		}

		if token.Literal != checkable.b {
			log.Fatalf("Expected Literal [%s] but got [%s]", checkable.b, token.Literal)
		}
	}

	token := lexer.Next_Token()

	if token.Type != END {
		log.Fatal("Expected EOF token but got ",
			token.Type, " {",
			token.Span.line, ":",
			token.Span.col, "}")
	}
}

func TestLexingLiteralTokens(t *testing.T) {
	path := "../samples/test_sources/lexer/valid/literal_tokens.pr"
	source := ReadFile(path)

	lexer := New(source)

	expected := []Pair{
		{I64, "12345"},
		{I64, "32100"},
		{I64, "9223372036854775807"},
		{CHAR, "a"},
		{CHAR, "b"},
		{CHAR, "c"},
		{CHAR, "d"},
		{CHAR, "e"},
		{CHAR, "\""},
		{STRING, "\"some string\""},
		{FALSE, "false"},
		{TRUE, "true"},
	}

	for _, checkable := range expected {
		token := lexer.Next_Token()

		if token.Type != TokenType(fmt.Sprint(checkable.a)) {
			log.Fatalf("Expected Type [%s] but got [%s]", checkable.a, token.Type)
		}

		if token.Literal != checkable.b {
			log.Fatalf("Expected Literal [%s] but got [%s]", checkable.b, token.Literal)
		}
	}

	token := lexer.Next_Token()

	if token.Type != END {
		log.Fatal("Expected EOF token but got ",
			token.Type, " {",
			token.Span.line, ":",
			token.Span.col, "}")
	}
}

func TestLexingNumberTokens(t *testing.T) {
	path := "../samples/test_sources/lexer/valid/number_tokens.pr"
	source := ReadFile(path)

	lexer := New(source)

	expected := []Pair{
		{I64, "12345"},
		{I64, "32100"},
		{I64, "9223372036854775807"},
	}

	for _, checkable := range expected {
		token := lexer.Next_Token()

		if token.Type != TokenType(fmt.Sprint(checkable.a)) {
			log.Fatalf("Expected Type [%s] but got [%s]", checkable.a, token.Type)
		}

		if token.Literal != checkable.b {
			log.Fatalf("Expected Literal [%s] but got [%s]", checkable.b, token.Literal)
		}
	}

	token := lexer.Next_Token()

	if token.Type != END {
		log.Fatal("Expected EOF token but got ",
			token.Type, " {",
			token.Span.line, ":",
			token.Span.col, "}")
	}
}

func TestLexingStringLiterals(t *testing.T) {
	path := "../samples/test_sources/lexer/valid/string_literals.pr"
	source := ReadFile(path)

	lexer := New(source)

	expected := []Pair{
		{STRING, "\"some string\""},
		{STRING, "\"another string\""},
		{STRING, "\"a\""},
		{STRING, "\"eiou\""},
		{STRING, "\"quote ' in string '\""},
		{STRING, "\"escaped \\\" string\""},
		{STRING, "\"another \\\"escaped \\\"string, yayy\\\"\""},
	}

	for _, checkable := range expected {
		token := lexer.Next_Token()

		if token.Type != TokenType(fmt.Sprint(checkable.a)) {
			log.Fatalf("Expected Type [%s] but got [%s]", checkable.a, token.Type)
		}

		if token.Literal != checkable.b {
			log.Fatalf("Expected Literal [%s] but got [%s]", checkable.b, token.Literal)
		}
	}

	token := lexer.Next_Token()

	if token.Type != END {
		log.Fatal("Expected EOF token but got ",
			token.Type, " {",
			token.Span.line, ":",
			token.Span.col, "}")
	}
}
