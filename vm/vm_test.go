package vm

import (
	"proto/analysis/name_resolver"
	"proto/analysis/type_checker"
	"proto/compiler"
	"proto/parser"
	"testing"
)

type vmTestCase struct {
	input    string
	expected string
}

func TestIntegerArithmetic(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() -> i64 { 1 }",
			expected: "1",
		},
		{
			input:    "fn main() -> i64 { 2 }",
			expected: "2",
		},
		{
			input:    "fn main() -> i64 { 1 + 2 }",
			expected: "3",
		},
		{
			input:    "fn main() -> i64 { 2 - 1 }",
			expected: "1",
		},
		{
			input:    "fn main() -> i64 { 3 * 3 }",
			expected: "9",
		},
		{
			input:    "fn main() -> i64 { 42 / 21 }",
			expected: "2",
		},
		{
			input:    "fn main() -> i64 { 3 / 2 }",
			expected: "1",
		},
		{
			input:    "fn main() -> i64 { 1 % 2 }",
			expected: "1",
		},
		{
			input:    "fn main() -> i64 { -1 }",
			expected: "-1",
		},
		{
			input:    "fn main() -> i64 { 300 * -2 }",
			expected: "-600",
		},
		{
			input:    "fn main() -> i64 { -(300 * -2) }",
			expected: "600",
		},
	}

	runVmTest(t, tests)
}

func TestBooleanValues(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() -> bool { true }",
			expected: "true",
		},
		{
			input:    "fn main() -> bool { false }",
			expected: "false",
		},
		{
			input:    "fn main() -> bool { false; true }",
			expected: "true",
		},
		{
			input:    "fn main() -> bool { not true }",
			expected: "false",
		},
		{
			input:    "fn main() -> bool { not false }",
			expected: "true",
		},
		{
			input:    "fn main() -> bool { not not not false }",
			expected: "true",
		},
		{
			input:    "fn main() -> bool { not if false { false } else { true } }",
			expected: "false",
		},
	}

	runVmTest(t, tests)
}

func TestEquality(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() -> bool { 1 == 2 }",
			expected: "false",
		},
		{
			input:    "fn main() -> bool { 1 != 1 }",
			expected: "false",
		},
		{
			input:    "fn main() -> bool { true == true }",
			expected: "true",
		},
		{
			input:    "fn main() -> bool { 'a' != 'b' }",
			expected: "true",
		},
		{
			input:    "fn main() -> bool { \"str\" == \"str\" }",
			expected: "true",
		},
		{
			input:    "fn main() -> bool { 1 + 2 == 3 * 1 }",
			expected: "true",
		},
		{
			input:    "fn main() -> bool { 3 > 2 == true }",
			expected: "true",
		},
	}

	runVmTest(t, tests)
}

func TestAndOr(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() -> bool { true || false }",
			expected: "true",
		},
		{
			input:    "fn main() -> bool { false && true }",
			expected: "false",
		},
		{
			input:    "fn main() -> bool { true && false }",
			expected: "false",
		},
		{
			input:    "fn main() -> bool { false || true }",
			expected: "true",
		},
		{
			input:    "fn main() -> bool { true || true }",
			expected: "true",
		},
		{
			input:    "fn main() -> bool { false && false }",
			expected: "false",
		},
	}

	runVmTest(t, tests)
}

func TestComparison(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() -> bool { 1 > 2 }",
			expected: "false",
		},
		{
			input:    "fn main() -> bool { 3 <= 1 }",
			expected: "false",
		},
		{
			input:    "fn main() -> bool { 500 > 499 }",
			expected: "true",
		},
		{
			input:    "fn main() -> bool { 2 < 3 }",
			expected: "true",
		},
		{
			input:    "fn main() -> bool { 3 >= 3 }",
			expected: "true",
		},
		{
			input:    "fn main() -> bool { 'b' > 'a' }",
			expected: "true",
		},
		{
			input:    "fn main() -> bool { '' >= '' }",
			expected: "true",
		},
		{
			input:    "fn main() -> bool { 'a' < 'z' }",
			expected: "true",
		},
		{
			input:    "fn main() -> bool { 'a' >= 'z' }",
			expected: "false",
		},
		{
			input:    "fn main() -> bool { 'z' < 'a' }",
			expected: "false",
		},
	}

	runVmTest(t, tests)
}

func TestStringsAndChars(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() -> str { 'a' + 'b' }",
			expected: "\"ab\"",
		},
		{
			input:    "fn main() -> str { \"proto \" + \"language\" }",
			expected: "\"proto language\"",
		},
		{
			input:    "fn main() -> str { \"proto \" + \"language\" + '!' }",
			expected: "\"proto language!\"",
		},
		{
			input:    "fn main() -> str { \"proto \" + \"language\" + '!' + \" It is fun!\" }",
			expected: "\"proto language! It is fun!\"",
		},
	}

	runVmTest(t, tests)
}

func TestIfConditional(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() -> bool { if 1 < 2 { true } }",
			expected: "true",
		},
		{
			input:    "fn main() -> i64 { if 3 * 2 == 6 { 6 } }",
			expected: "6",
		},
		{
			input:    "fn main() -> i64 { if 1 > 2 { 1 } else { 2 } }",
			expected: "2",
		},
		{
			input:    "fn main() -> i64 { if 1 > 2 { 1 } else if 2 > 3 { 2 } else { 3 } }",
			expected: "3",
		},
		{
			input:    "fn main() -> i64 { if 1 > 2 { 1 } else if 2 <= 3 { 2 } else { 3 } }",
			expected: "2",
		},
		{
			input:    "fn main() -> i64 { if false { 1 } else { 2 } }",
			expected: "2",
		},
	}

	runVmTest(t, tests)
}

func TestGlobalUseOfIdentifiers(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() -> i64 { let un: i64 = 1; un }",
			expected: "1",
		},
		{
			input:    "fn main() -> i64 { let un = 1; let deux = 2; un + deux }",
			expected: "3",
		},
		{
			input:    "fn main() -> i64 { let un = 1; let deux = 2; if un + deux >= 3 { 3 } else { un + deux } }",
			expected: "3",
		},
	}

	runVmTest(t, tests)
}

func TestMakingArrays(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() -> [i64] { [i64;] }",
			expected: "[]",
		},
		{
			input:    "fn main() -> [i64] { let a = 4; [1, 2, 3, a] }",
			expected: "[1, 2, 3, 4]",
		},
		{
			input:    "fn main() -> [i64] { let a = 4; [1, 2, 3, a, a + 1] }",
			expected: "[1, 2, 3, 4, 5]",
		},
	}

	runVmTest(t, tests)
}

func TestIndexingArrays(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() -> i64 { let a = [1, 2, 3]; a[1] }",
			expected: "2",
		},
		{
			input:    "fn main() -> i64 { let a = [1, 2, 3]; a[1 + 1] }",
			expected: "3",
		},
		{
			input:    "fn main() -> i64 { let a = [1, 2, 3, 4]; let b = 2; a[b + 1] }",
			expected: "4",
		},
		{
			input:    "fn main() -> i64 { let a = [1, 2, 3]; let b = [0]; a[b[0]] }",
			expected: "1",
		},
		{
			input:    "fn main() -> i64 { let a = [i64; 1, 2, 3]; a[2] }",
			expected: "3",
		},
		{
			input:    "fn main() -> [i64] { let a = [[1, 2, 3], [4, 5, 6]]; a[0] }",
			expected: "[1, 2, 3]",
		},
		{
			input:    "fn main() -> i64 { let a = [[1, 2, 3], [4, 5, 6]]; a[0][1] }",
			expected: "2",
		},
	}

	runVmTest(t, tests)
}

func TestMakingTuples(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() -> (i64, char, bool) { let a: (i64, char, bool) = (300, 'a', false); a }",
			expected: "(300, 'a', false)",
		},
		{
			input:    "let a = (1, 'b', 3, 'd'); fn main() -> ((i64, char, i64, char), i64, bool, bool) { let b = (a, 1, false, true); b }",
			expected: "((1, 'b', 3, 'd'), 1, false, true)",
		},
	}

	runVmTest(t, tests)
}

func TestUpdatingArrayIndex(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() -> [i64] { mut a = [0, 2, 3]; let b = 2; a[b] = 3 + 1; a }",
			expected: "[0, 2, 4]",
		},
		{
			input:    "fn main() -> [i64] { mut a = [[0, 2, 3], [1, 3, 5]]; let b = 2; a[0][b] = 3 + 1; a[0] }",
			expected: "[0, 2, 4]",
		},
		{
			input:    "fn main() -> [i64] { mut a = [1, 2]; a[0] += 3; a }",
			expected: "[4, 2]",
		},
		{
			input:    "fn main() -> [i64] { mut a = [1, 2]; a[0] -= 1; a }",
			expected: "[0, 2]",
		},
		{
			input:    "fn main() -> [i64] { mut a = [1, 2]; a[0] *= 3; a }",
			expected: "[3, 2]",
		},
		{
			input:    "fn main() -> [i64] { mut a = [4, 2]; a[0] /= 2; a }",
			expected: "[2, 2]",
		},
		{
			input:    "fn main() -> [i64] { mut a = [3, 2]; a[0] %= 3; a }",
			expected: "[0, 2]",
		},
	}

	runVmTest(t, tests)
}

func TestAccessingTupleMembers(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() -> char { let a = (1, 'c', false); let b: char = a.1; b }",
			expected: "'c'",
		},
		{
			input:    "fn main() -> (i64, bool) { let a = ((1, 'c', false), (3, true)); let b: i64 = a.0.0; let c: bool = a.1.1; (b, c) }",
			expected: "(1, true)",
		},
	}

	runVmTest(t, tests)
}

func TestAccessingStructMembers(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() -> i64 { struct Test { a: i64 } let a = Test { a: 3 }; a.a }",
			expected: "3",
		},
		{
			input:    "struct Test { a: char } struct Nest { b: Test } fn main() -> char { let b = Nest { b: Test { a: 'c' } }; b.b.a }",
			expected: "'c'",
		},
	}

	runVmTest(t, tests)
}

func TestBlocks(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() { let a = 3; { let b = 3; let c = a + b; } }",
			expected: "()",
		},
		{
			input:    "fn main() { { let a = 4; let a = 6; let b = a + 1; let c = b + a; } }",
			expected: "()",
		},
		{
			input:    "fn main() -> i64 { { let a = 4; let a = 6; let b = a + 1; let c = b + a; c } }",
			expected: "13",
		},
		{
			input:    "fn main() -> bool { let a: bool = true; { let a = a; a } }",
			expected: "true",
		},
		{
			input:    "fn main() -> i64 { { 1 + 2 + 3; let a = 4; a } }",
			expected: "4",
		},
		{
			input:    "fn main() -> i64 { { let a = 10; { let b = a; b } } }",
			expected: "10",
		},
		{
			input:    "fn main() -> i64 { { let a = 10; { let b = a; { let c = b + a; c } } } }",
			expected: "20",
		},
		{
			input:    "fn main() -> i64 { { let a = 10; { let a = 20; a + a } + a } }",
			expected: "50",
		},
		{
			input:    "fn main() -> i64 { 1 + 1 + 3; let a = 6; { let a = 10; a } }",
			expected: "10",
		},
	}

	runVmTest(t, tests)
}

func TestInfiniteLoops(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() -> i64 { mut count = 10; loop { if count <= 0 { break; } count -= 1; } count }",
			expected: "0",
		},
		{
			input:    "fn main() -> i64 { mut i = 0; loop { if i == 10 { break; } else { i += 1; } } i }",
			expected: "10",
		},
		{
			input:    "fn main() -> i64 { mut i = 0; loop { if i == 200 { break; } else { loop { if i == 200 { break; } i += 10; continue; } } } i }",
			expected: "200",
		},
	}

	runVmTest(t, tests)
}

func TestGenericForLoops(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() -> i64 { mut i = 0; for mut j = i; j < 10; j += 1 { i += j; } i}",
			expected: "45",
		},
		{
			input:    "fn main() -> i64 { mut largest = 0; for mut j = 0; j <= 20; j += 2 { if j % 2 == 0 { largest = j; } } largest }",
			expected: "20",
		},
	}

	runVmTest(t, tests)
}

func TestWhileLoops(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() -> i64 { mut a = true; mut b = 0; while a { if b == 10 { a = not a; } else { b += 1; } } b }",
			expected: "10",
		},
		{
			input:    "fn main() -> i64 { mut n = 32; mut i = 1; mut x = 0; mut y = i; mut z = x; while i < n { z = x + y; x = y; y = z; i += 1; } z }",
			expected: "2178309",
		},
	}

	runVmTest(t, tests)
}

func TestFunctionDef(t *testing.T) {
	tests := []vmTestCase{
		{
			input: `
		fn returns() -> i64 {
			1
		}

		fn main() {
			let a = 5;
		}`,
			expected: "()",
		},
		{
			input:    "fn main() { } fn adds_1(n: i64) -> i64 { n + 1 }",
			expected: "()",
		},
	}

	runVmTest(t, tests)
}

func TestFunctionCalls(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() -> i64 { fn squared(n: i64) -> i64 { n * n } mut a = 0; fn add_1(n: i64) -> i64 { n + 1 } a = add_1(2); squared(a) }",
			expected: "9",
		},
		{
			input:    "fn returns_1() -> i64 { 1 } fn main() -> i64 { returns_1() }",
			expected: "1",
		},
		{
			input: `
fn fib(x: i64) -> i64 {
	if x <= 1 {
		x
	} else {
		fib(x - 1) + fib(x - 2)
	}
}

fn main() -> i64 { 
	fib(2)
}`,
			expected: "1",
		},
	}

	runVmTest(t, tests)
}

func TestMakingRanges(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() -> Range<i64> { 1..2 }",
			expected: "1..2",
		},
		{
			input:    "fn main() -> Range<i64> { let a = 1; let b = 5; let c = a..b; c }",
			expected: "1..5",
		},
		{
			input:    "fn main() -> Range<char> { 'a'..'z' }",
			expected: "'a'..'z'",
		},
		{
			input:    "fn main() -> Range<char> { let a = 'a'; let b = 'b'; let c = a..b; c }",
			expected: "'a'..'b'",
		},
	}

	runVmTest(t, tests)
}

func TestMakingInclusiveRanges(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() -> Range<i64> { 1..=2 }",
			expected: "1..=2",
		},
		{
			input:    "fn main() -> Range<i64> { let a = 1; let b = 5; let c = a..=b; c }",
			expected: "1..=5",
		},
		{
			input:    "fn main() -> Range<char> { 'a'..='z' }",
			expected: "'a'..='z'",
		},
		{
			input:    "fn main() -> Range<char> { let a = 'a'; let b = 'b'; let c = a..=b; c }",
			expected: "'a'..='b'",
		},
	}

	runVmTest(t, tests)
}

func TestStructInitialization(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "struct Test { a: i64 } fn main() -> Test { let a = Test { a: 30 }; a }",
			expected: "Test { a: 30 }",
		},
		{
			input:    "struct Local { loc: i64, point: char } fn main() -> Local { let a = Local { loc: 4, point: 'd' }; a }",
			expected: "Local { point: 'd', loc: 4 }",
		},
	}

	runVmTest(t, tests)
}

func TestUpdatingStructMembers(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "struct Test { a: i64 } fn main() -> i64 { mut a = Test { a: 4 }; a.a = 50; a.a }",
			expected: "50",
		},
		{
			input:    "struct Test { a: i64 } struct Nest { b: Test } fn main() -> (Test, i64) { mut a = Test { a: 5 }; a.a = 45; let b = Nest { b: a }; (b.b, b.b.a) }",
			expected: "(Test { a: 45 }, 45)",
		},
		{
			input:    "struct Test { a: i64 } fn main() -> i64 { mut a = Test { a: 1 }; a.a += 5; a.a }",
			expected: "6",
		},
		{
			input:    "struct Test { a: i64 } fn main() -> i64 { mut a = Test { a: 1 }; a.a -= 1; a.a }",
			expected: "0",
		},
		{
			input:    "struct Test { a: i64 } fn main() -> i64 { mut a = Test { a: 1 }; a.a *= 5; a.a }",
			expected: "5",
		},
		{
			input:    "struct Test { a: i64 } fn main() -> i64 { mut a = Test { a: 7 }; a.a /= 5; a.a }",
			expected: "1",
		},
		{
			input:    "struct Test { a: i64 } fn main() -> i64 { mut a = Test { a: 3 }; a.a %= 2; a.a }",
			expected: "1",
		},
		{
			input:    `struct Test { a: str } fn main() -> str { mut a = Test { a: "proto" }; a.a += " is awesome."; a.a }`,
			expected: `"proto is awesome."`,
		},
	}

	runVmTest(t, tests)
}

func TestCallingBuiltinFns(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "fn main() { let a = 0; println(a); }",
			expected: "()",
		},
		{
			input:    "fn main() { println(); print(); }",
			expected: "1",
		},
		// {
		// 	input:    "",
		// 	expected: "",
		// },
	}

	runVmTest(t, tests)
}

func runVmTest(t *testing.T, tests []vmTestCase) {
	t.Helper()

	for i, tt := range tests {
		println(tt.input + "\n")
		prog := parser.Parse(tt.input, true)
		nr := name_resolver.NewNameResolver()
		tc := type_checker.NewTypeChecker()
		nr.ResolveProgram(prog)
		if nr.FoundError {
			t.Fatal("Found errors during name resolution")
		}

		tc.TypeCheckProgram(prog)
		if tc.FoundError {
			t.Fatal("Found errors during type checking")
		}

		compiler := compiler.NewCompiler()
		compiler.CompileProgram(prog)

		if compiler.FoundError {
			t.Fatal("Found errors during compilation")
		}

		bc := compiler.ByteCode()
		vm := NewVM(bc)
		// println(vm.instructions.Disassemble())
		vm.Run()

		if vm.FoundError {
			t.Fatal("Found errors during virtual machine execution")
		}

		res := vm.LastPoppedElem()

		if res == nil {
			res = vm.StackTop()
		}

		if res.String() != tt.expected {
			t.Errorf("%d. Expected %s as stack top but found %s.", i, tt.expected, res.String())
			// t.Fatal(compiler.ByteCode().Instructions.Disassemble())
		}
	}
}
