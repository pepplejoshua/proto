package parser

import (
	"log"
	"proto/shared"
	"testing"
)

func TestParsingBinaryOperations(t *testing.T) {
	path := "../samples/test_sources/parser/valid/binary_operations.pr"
	source := shared.ReadFile(path)

	program := Parse(source)
	contents := program.Contents
	expected := []string{
		"(+ 1 2)",
		"(- 3 1)",
		"(* 3 2)",
		"(/ 3 4)",
		"(% 4 2)",
		"(< 1 2)",
		"(> 3 1)",
		"(>= 3 3)",
		"(<= 2 3)",
		"(!= 2 3)",
		"(== 1 2)",
		"(|| true false)",
		"(&& false true)",
		"(== true true)",
		"(!= false true)",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingLiterals(t *testing.T) {
	path := "../samples/test_sources/parser/valid/literals.pr"
	source := shared.ReadFile(path)

	program := Parse(source)
	contents := program.Contents
	expected := []string{
		"1",
		"2",
		"\"string\"",
		"\"\"",
		"'c'",
		"true",
		"false",
		"some_identifier",
		"_another_identifier",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingBinaryOperationsPrecedences(t *testing.T) {

}

func TestParsingParenthesizedExpressions(t *testing.T) {
	path := "../samples/test_sources/parser/valid/parenthesized_expr.pr"
	source := shared.ReadFile(path)

	program := Parse(source)
	contents := program.Contents
	expected := []string{
		"(+ 1 2)",
		"1",
		"(- 2)",
		"true",
		"\"str\"",
		"'c'",
		"(== a b)",
		"id",
		"(not false)",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingStructs(t *testing.T) {

}

func TestParsingUnaryOperations(t *testing.T) {
	path := "../samples/test_sources/parser/valid/unary_operations.pr"
	source := shared.ReadFile(path)

	program := Parse(source)
	contents := program.Contents
	expected := []string{
		"(- 300)",
		"(not true)",
		"(not false)",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingMixedBinaryAndUnaryOperations(t *testing.T) {

}

func TestParsingVariableDeclarations(t *testing.T) {
	path := "../samples/test_sources/parser/valid/variable_declarations.pr"
	source := shared.ReadFile(path)

	program := Parse(source)
	contents := program.Contents
	expected := []string{
		"(let a: i64 3)",
		"(let b: str \"string\")",
		"(let c: char 'c')",
		"(mut d: [i64])",
		"(mut e: (i64, char, bool))",
		"(let f: bool true)",
		"(mut g: [i64] [1, 2, 3, 4])",
		"(let h: (i64, char, bool) (1, '2', false))",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}
