package parser

import (
	"log"
	"proto/ast"
	"proto/shared"
	"testing"
)

type Pair struct {
	a, b string
}

func TestParsingBinaryOperations(t *testing.T) {
	path := "../samples/test_sources/parser/valid/binary_operations.pr"
	source := shared.ReadFile(path)

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"(+ 1 2);",
		"(- 3 1);",
		"(* 3 2);",
		"(/ 3 4);",
		"(% 4 2);",
		"(< 1 2);",
		"(> 3 1);",
		"(>= 3 3);",
		"(<= 2 3);",
		"(!= 2 3);",
		"(== 1 2);",
		"(|| true false);",
		"(&& false true);",
		"(== true true);",
		"(!= false true);",
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

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"1;",
		"2;",
		"\"string\";",
		"\"\";",
		"'c';",
		"true;",
		"false;",
		"some_identifier;",
		"_another_identifier;",
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

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"(+ 1 2);",
		"1;",
		"(- 2);",
		"true;",
		"\"str\";",
		"'c';",
		"(== a b);",
		"id;",
		"(not false);",
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

	program := Parse(source, false)
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

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"(- 300);",
		"(not true);",
		"(not false);",
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

	program := Parse(source, false)
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
		"(mut o: ())",
		"(mut p: () ())",
		"(let q: fn(i64, bool) -> char)",
		"(let r: fn(fn(i64, bool) -> bool, fn(char, str) -> ()) -> fn(i64, str) -> ())",
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

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"(a = 3)",
		"(some_ident = true)",
		"(array = [1, 2, 3])",
		"(tuple = (1, true, 'c'))",
		"(string = \"str\")",
		"(a += 3)",
		"(b -= 2)",
		"(c *= 4)",
		"(d /= 2)",
		"(e %= 2)",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingBlockExpressions(t *testing.T) {
	path := "../samples/test_sources/parser/valid/blocks.pr"
	source := shared.ReadFile(path)

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"{ 1; }: i64",
		"{ (1, true, 'c'); }: (i64, bool, char)",
		"{ [1, 2, 3, 4]; }: [i64]",
		"{ 1; }: ()",
		"{ (let a: i64 2) }: ()",
		"{ (mut c: bool) }: ()",
		"{ (+ 1 2); (let a: bool true) (a = (* a 2)) \"string\"; }: str",
		"{ (let a: i64 3) (mut b: i64 5) (* a b); }: untyped",
		"{  }: ()",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingIfExpressions(t *testing.T) {
	path := "../samples/test_sources/parser/valid/if_expressions.pr"
	source := shared.ReadFile(path)

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"(if true { 1; }: i64 else { 3; }: i64): i64",
		"(if (== a b) { \"same\"; }: str else { \"different\"; }: str): str",
		"(let a: char (if (== (- 200 1) 199) { 'a' }: char else { 'b' }: char): char)",
		"(if true {  }: ()): ()",
		"(if stuff { 'a'; }: char else { 400; }: i64): untyped",
		"(let b: i64 (if true { 1 }: i64 else (if (not false) { 2 }: i64 else { 3 }: i64): i64): i64)",
		"(if true { (+ 1 2); }: untyped else { 1; }: i64): untyped",
		"(if false { (+ 1 2); }: untyped else { (* 3 4); }: untyped): untyped",
		"(let f: i64 (if true { 1 }: i64 else { 2 }: i64): i64)",
		"(while (if true { true }: bool else { false }: bool): bool {  }: ())",
		"(a = (if b { 1 }: i64 else { 2 }: i64): i64)",
		"(not (if true { 1 }: i64 else { 2 }: i64): i64);",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingComplexTypes(t *testing.T) {
	path := "../samples/test_sources/parser/valid/complex_types.pr"
	source := shared.ReadFile(path)

	program := Parse(source, false)
	contents := program.Contents
	expected := []Pair{
		{"[1, 2, 3, 4, 5];", "[i64]"},
		{"(3, );", "(i64)"},
		{"(1, 2, 'c', false);", "(i64, i64, char, bool)"},
		{"[];", "[i64]"},
		{"[[1, 2, 3], [3, 4, 5]];", "[[i64]]"},
		{"([1, 2], (1, 'b'), false);", "([i64], (i64, char), bool)"},
		{"[1, 2, 3];", "[i64]"},
		{"[];", "[Person]"},
		{"[a, b, c];", "[untyped]"},
		{"[1];", "[i64]"},
		{"[[a], 1, 2];", "[untyped]"},
		{"[1, 2];", "[untyped]"},
	}

	for index, node := range contents {
		literal := expected[index].a
		node_type := expected[index].b

		if literal != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}

		switch v := node.(type) {
		case ast.Expression:
			if v.Type().TypeSignature() != node_type {
				log.Fatalf("[%d] Expected type [%s] but got [%s]", index, node_type, v.Type().TypeSignature())
			}
		case *ast.PromotedExpr:
			if v.Expr.Type().TypeSignature() != node_type {
				log.Fatalf("[%d] Expected type [%s] but got [%s]", index, node_type, v.Expr.Type().TypeSignature())
			}
		}
	}
}

func TestParsingGenericForLoops(t *testing.T) {
	path := "../samples/test_sources/parser/valid/generic_for_loops.pr"
	source := shared.ReadFile(path)

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"(for (mut a: i64 0) (< a 5) (a = (+ a 1)) {  }: ())",
		"(for (mut b: bool true) b (!= b false) {  }: ())",
		"(for (mut c: i64 3) (== (% c 3) 0) (c = (+ c 3)) { 300 }: i64)",
		"(for (mut i: i64 0) (< i 30) (+ i 30) { 300 }: i64)",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingCollectionsForLoops(t *testing.T) {
	path := "../samples/test_sources/parser/valid/collections_for_loops.pr"
	source := shared.ReadFile(path)

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"(for a in [1, 2, 3, 4, 5] {  }: ())",
		"(for chr in ('c', 'd', 'e') {  }: ())",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingInfiniteLoops(t *testing.T) {
	path := "../samples/test_sources/parser/valid/infinite_loops.pr"
	source := shared.ReadFile(path)

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"(loop { (let a: i64 5) (+ a a) }: untyped)",
		"(loop {  }: ())",
		"(loop { (if (< 3 2) { (break) }: ()): () (continue) }: ())",
		"(loop { 300 }: i64)",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingFunctionDefinitions(t *testing.T) {
	path := "../samples/test_sources/parser/valid/function_definitions.pr"
	source := shared.ReadFile(path)

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"(fn is_even(n: i64) -> bool { (== (% n 2) 0) }: untyped)",
		"(fn negate(value: bool) -> bool { (not value) }: untyped)",
		"(fn do_nothing() -> () { (return) }: ())",
		"(fn no_params() -> char { 'a' }: char)",
		"(fn three_params(m: i64, n: bool, o: [str]) -> () { m }: untyped)",
		"(fn closure(c: char, b: bool) -> fn(char, bool) -> str {  }: ())",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingFunctionCalls(t *testing.T) {
	path := "../samples/test_sources/parser/valid/function_calls.pr"
	source := shared.ReadFile(path)

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"is_even(2);",
		"do_stuff([1, 2, 3, 4]);",
		"no_params();",
		"(let a: untyped is_even(3))",
		"closure(1)(2);",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingIndexingExpressions(t *testing.T) {
	path := "../samples/test_sources/parser/valid/indexing_expressions.pr"
	source := shared.ReadFile(path)

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"[1, 2, 3, 4][0];",
		"some_arr[2];",
		"function_call([1, 2, 3, 4])[3];",
		"(let a: untyped (1, 'b', true, \"stringed\")[3])",
		"[[1, 2, 3], [3, 4, 5], [5, 6, 7]][1][2];",
		"[1, 2, 3][get_index()];",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingRanges(t *testing.T) {
	path := "../samples/test_sources/parser/valid/ranges.pr"
	source := shared.ReadFile(path)

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"a..b: Range<untyped>;",
		"1..=2: Range<i64>;",
		"3..5: Range<i64>;",
		"'a'..'z': Range<char>;",
		"'a'..='e': Range<char>;",
		"300..end: Range<untyped>;",
		"(let range: Range<i64> 1..5: Range<i64>)",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingMembershipExpressions(t *testing.T) {
	path := "../samples/test_sources/parser/valid/dot_expressions.pr"
	source := shared.ReadFile(path)

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"[1, 2, 3, 4, 5].length();",
		"some_object.member;",
		"object.function_call();",
		"object.member.function_call();",
		"tuple.1;",
		"another_tuple.0;",
		"function_call().member;",
		"arr[3].function().member;",
		"arr[1].member[1];",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingBinaryOperationsPrecedences(t *testing.T) {
	path := "../samples/test_sources/parser/valid/binary_precedence.pr"
	source := shared.ReadFile(path)

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"(+ 1 (* 2 3));",
		"(|| 1 2)..(&& 3 4): Range<untyped>;",
		"(|| (&& 1 2) (&& 3 4));",
		"(* (+ 1 2) 3);",
		"(|| (&& (== 1 2) (!= 3 3)) false);",
		"(>= (+ 1 2) 3);",
		"(* 1 (- 3));",
		"(== true (not false));",
		"(&& some_boolean() some_boolean());",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingStructFunctionInits(t *testing.T) {
	path := "../samples/test_sources/parser/valid/struct_inits.pr"
	source := shared.ReadFile(path)

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"(struct Person { name: str, age: i64, hobbies: [str], sex: char })",
		"(let joshua: Person Person { name: \"Joshua\", age: 23, hobbies: [\"programming\", \"cooking\", \"gaming\"], sex: 'M' })",
		"(let adult: i64 18)",
		"(let msg: str (if (>= joshua.age adult) { \"you are old enough\" }: str else { \"you are underage\" }: str): str)",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingReferencedVariables(t *testing.T) {
	path := "../samples/test_sources/parser/valid/references.pr"
	source := shared.ReadFile(path)

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}
