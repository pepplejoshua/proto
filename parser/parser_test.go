package parser

import (
	"log"
	"proto/shared"
	"testing"
)

func TestParsingBinaryOperations(t *testing.T) {
	path := "../samples/test_sources/parser/valid/binary_operations.pr"
	source := shared.ReadFile(path)

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"1 + 2;",
		"3 - 1;",
		"3 * 2;",
		"3 / 4;",
		"4 % 2;",
		"1 < 2;",
		"3 > 1;",
		"3 >= 3;",
		"2 <= 3;",
		"2 != 3;",
		"1 == 2;",
		"true || false;",
		"false && true;",
		"true == true;",
		"false != true;",
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
		"1 + 2;",
		"1;",
		"- 2;",
		"true;",
		"\"str\";",
		"'c';",
		"a == b;",
		"id;",
		"not false;",
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
		"struct Person { name: str, age: i64 }",
		"struct Token { literal: str, line: i64, col: i64 }",
		"struct BasketBall { HomePoints: i64, AwayPoints: i64, HomePlayers: [Person], AwayPlayers: [Person], MVP: Person }",
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
		"- 300;",
		"not true;",
		"not false;",
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
		"let a = 3",
		"let b = \"string\"",
		"let c = 'c'",
		"mut d",
		"mut e",
		"let f = true",
		"mut g = [1, 2, 3, 4]",
		"let h = (1, '2', false)",
		"mut i = ('c', \"str\", 24, true, [1, 2, 3, 4])",
		"let j = [1, 2, 3, 4]",
		"mut k = [1, '2', true]",
		"let l = 1 + 2",
		"mut m = [(1, 2, 'a'), (2, 3, 'b')]",
		"let n",
		"mut o",
		"mut p = ()",
		"let q",
		"let r",
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
		"a = 3",
		"some_ident = true",
		"array = [1, 2, 3]",
		"tuple = (1, true, 'c')",
		"string = \"str\"",
		"a += 3",
		"b -= 2",
		"c *= 4",
		"d /= 2",
		"e %= 2",
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

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"{ 1; }",
		"{ (1, true, 'c'); }",
		"{ [1, 2, 3, 4]; }",
		"{ 1; }",
		"{ let a = 2 }",
		"{ mut c }",
		"{ 1 + 2; let a = true a = a * 2 \"string\"; }",
		"{ let a = 3 mut b = 5 a * b; }",
		"{  }",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingIfConditionals(t *testing.T) {
	path := "../samples/test_sources/parser/valid/if_conds.pr"
	source := shared.ReadFile(path)

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"if true { 1; } else { 3; }",
		"if a == b { \"same\"; } else { \"different\"; }",
		"let a = if 200 - 1 == 199 { 'a' } else { 'b' }",
		"if true {  }",
		"if stuff { 'a'; } else { 400; }",
		"let b = if true { 1 } else if not false { 2 } else { 3 }",
		"if true { 1 + 2; } else { 1; }",
		"while if true { true } else { false } {  }",
		"a = if b { 1 } else { 2 }",
		"not if true { 1 } else { 2 };",
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
	expected := []string{
		"[1, 2, 3, 4, 5];",
		"(3, );",
		"(1, 2, 'c', false);",
		"[];",
		"[[1, 2, 3], [3, 4, 5]];",
		"([1, 2], (1, 'b'), false);",
		"[1, 2, 3];",
		"[];",
		"[a, b, c];",
		"[1];",
		"[[a], 1, 2];",
		"[1, 2];",
	}

	for index, node := range contents {
		literal := expected[index]
		if literal != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingGenericForLoops(t *testing.T) {
	path := "../samples/test_sources/parser/valid/generic_for_loops.pr"
	source := shared.ReadFile(path)

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"for mut a = 0 a < 5 a = a + 1 {  }",
		"for mut b = true b b != false {  }",
		"for mut c = 3 c % 3 == 0 c = c + 3 { 300; }",
		"for mut i = 0 i < 30 i + 30 { 300; }",
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
		"for a in [1, 2, 3, 4, 5] {  }",
		"for chr in ('c', 'd', 'e') {  }",
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
		"loop { let a = 5 a + a; }",
		"loop {  }",
		"loop { if 3 < 2 { break; } continue; }",
		"loop { 300; }",
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
		"fn is_even(n: i64) -> bool { n % 2 == 0 }",
		"fn negate(value: bool) -> bool { not value }",
		"fn do_nothing() -> () { return; }",
		"fn no_params() -> char { 'a' }",
		"fn three_params(m: i64, n: bool, o: [str]) -> () { m }",
		"fn closure(c: char, b: bool) -> fn(char, bool) -> str {  }",
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
		"let a = is_even(3)",
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
		"let a = (1, 'b', true, \"stringed\")[3]",
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
		"a..b;",
		"1..=2;",
		"3..5;",
		"'a'..'z';",
		"'a'..='e';",
		"300..end;",
		"let range = 1..5",
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
		"1 + 2 * 3;",
		"1 || 2..3 && 4;",
		"1 && 2 || 3 && 4;",
		"1 + 2 * 3;",
		"1 == 2 && 3 != 3 || false;",
		"1 + 2 >= 3;",
		"1 * - 3;",
		"true == not false;",
		"some_boolean() && some_boolean();",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingStructInits(t *testing.T) {
	path := "../samples/test_sources/parser/valid/struct_inits.pr"
	source := shared.ReadFile(path)

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"struct Person { name: str, age: i64, hobbies: [str], sex: char }",
		"let joshua = Person { name: \"Joshua\", age: 23, hobbies: [\"programming\", \"cooking\", \"gaming\"], sex: 'M' }",
		"let adult = 18",
		"let msg = if joshua.age >= adult { \"you are old enough\" } else { \"you are underage\" }",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingReferences(t *testing.T) {
	path := "../samples/test_sources/parser/valid/references.pr"
	source := shared.ReadFile(path)

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"let a = 3 + 3",
		"mut ref_a = &a",
		"let ref_expr = &[1, 2, 4, 5]",
		"let ref_ref = &a",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}

func TestParsingDereferences(t *testing.T) {
	path := "../samples/test_sources/parser/valid/derefs.pr"
	source := shared.ReadFile(path)

	program := Parse(source, false)
	contents := program.Contents
	expected := []string{
		"let a = 300",
		"let b = &a",
		"let c = *b + a",
	}

	for index, node := range contents {
		if expected[index] != node.LiteralRepr() {
			log.Fatalf("[%d] Expected literal [%s] but got [%s]", index, expected[index], node.LiteralRepr())
		}
	}
}
