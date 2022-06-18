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
	path := "../samples/test_sources/parser/valid/structs.pr"
	source := shared.ReadFile(path)

	program := Parse(source)
	contents := program.Contents
	expected := []string{
		"(struct Person { name: str, age: i64 })",
		"(struct Token { literal: str, line: i64, col: i64 })",
		"(struct BasketBall { HomePoints: i64, AwayPoints: i64, HomePlayers: [Person], AwayPlayers: [Person], MVP: Person })",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
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
		"(mut i: (char, str, i64, bool, [i64]) ('c', \"str\", 24, true, [1, 2, 3, 4]))",
		"(let j: [i64] [1, 2, 3, 4])",
		"(mut k: untyped [1, '2', true])",
		"(let l: untyped (+ 1 2))",
		"(mut m: [(i64, i64, char)] [(1, 2, 'a'), (2, 3, 'b')])",
		"(let n: UserDefined)",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingAssignment(t *testing.T) {
	path := "../samples/test_sources/parser/valid/assignment.pr"
	source := shared.ReadFile(path)

	program := Parse(source)
	contents := program.Contents
	expected := []string{
		"(a = 3)",
		"(some_ident = true)",
		"(array = [1, 2, 3])",
		"(tuple = (1, true, 'c'))",
		"(string = \"str\")",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingBlocks(t *testing.T) {
	path := "../samples/test_sources/parser/valid/blocks.pr"
	source := shared.ReadFile(path)

	program := Parse(source)
	contents := program.Contents
	expected := []string{
		"{ 1 }: i64",
		"{ (1, true, 'c') }: (i64, bool, char)",
		"{ [1, 2, 3, 4] }: [i64]",
		"{ 1; }: ()",
		"{ (let a: i64 2) }: ()",
		"{ (mut c: bool) }: ()",
		"{ (+ 1 2); (let a: bool true) (a = (* a 2)) \"string\" }: str",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

// func TestParsingBinaryOperationsPrecedences(t *testing.T) {

// }

// func TestParsingMixedBinaryAndUnaryOperations(t *testing.T) {

// }
