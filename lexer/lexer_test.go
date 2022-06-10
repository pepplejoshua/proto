package lexer

import (
	"fmt"
	"io/ioutil"
	"log"
	"testing"
)

func ReadFile(path string) string {
	src, err := ioutil.ReadFile(path)
	if err != nil {
		log.Fatal(err)
	}
	return string(src)
}

func TestAllTokens(t *testing.T) {
	path := "../samples/test_sources/lexer/valid/all_tokens.pr"
	source := ReadFile(path)

	lexer := New(source)

	for {
		token := lexer.Next_Token()
		if token.Type == ERROR {
			msg := "Ran into an error: " + token.Literal
			msg = msg + " {" + fmt.Sprint(token.Span.line) + ":" + fmt.Sprint(token.Span.col) + "}"
			log.Fatal(msg)
		} else if token.Type == END {
			println("EOF")
			break
		} else {
			msg := string(token.Type) + " {" + token.Literal + "}"
			msg = msg + " at " + fmt.Sprint(token.Span.line) + ":" + fmt.Sprint(token.Span.col)
			println(msg)
		}
	}
}
