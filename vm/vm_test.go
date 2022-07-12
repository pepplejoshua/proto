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
			input:    "1;",
			expected: "1",
		},
		{
			input:    "2;",
			expected: "2",
		},
		{
			input:    "1 + 2;",
			expected: "3",
		},
		{
			input:    "2 - 1;",
			expected: "1",
		},
		{
			input:    "3 * 3;",
			expected: "9",
		},
		{
			input:    "42 / 21;",
			expected: "2",
		},
		{
			input:    "3 / 2;",
			expected: "1",
		},
		{
			input:    "1 % 2;",
			expected: "1",
		},
		{
			input:    "-1;",
			expected: "-1",
		},
		{
			input:    "300 * -2;",
			expected: "-600",
		},
		{
			input:    "-(300 * -2);",
			expected: "600",
		},
	}

	runVmTest(t, tests)
}

func TestBooleanValues(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "true;",
			expected: "true",
		},
		{
			input:    "false;",
			expected: "false",
		},
		{
			input:    "false; true;",
			expected: "true",
		},
		{
			input:    "not true;",
			expected: "false",
		},
		{
			input:    "not false;",
			expected: "true",
		},
		{
			input:    "not not not false;",
			expected: "true",
		},
		{
			input:    "not if false { false } else { true };",
			expected: "false",
		},
	}

	runVmTest(t, tests)
}

func TestEquality(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "1 == 2;",
			expected: "false",
		},
		{
			input:    "1 != 1;",
			expected: "false",
		},
		{
			input:    "true == true;",
			expected: "true",
		},
		{
			input:    "'a' != 'b';",
			expected: "true",
		},
		{
			input:    "\"str\" == \"str\";",
			expected: "true",
		},
		{
			input:    "1 + 2 == 3 * 1;",
			expected: "true",
		},
		{
			input:    "3 > 2 == true;",
			expected: "true",
		},
	}

	runVmTest(t, tests)
}

func TestAndOr(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "true || false;",
			expected: "true",
		},
		{
			input:    "false && true;",
			expected: "false",
		},
		{
			input:    "true && false;",
			expected: "false",
		},
		{
			input:    "false || true;",
			expected: "true",
		},
		{
			input:    "true || true;",
			expected: "true",
		},
		{
			input:    "false && false;",
			expected: "false",
		},
	}

	runVmTest(t, tests)
}

func TestComparison(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "1 > 2;",
			expected: "false",
		},
		{
			input:    "3 <= 1;",
			expected: "false",
		},
		{
			input:    "500 > 499;",
			expected: "true",
		},
		{
			input:    "2 < 3;",
			expected: "true",
		},
		{
			input:    "3 >= 3;",
			expected: "true",
		},
		{
			input:    "'b' > 'a';",
			expected: "true",
		},
		{
			input:    "'' >= '';",
			expected: "true",
		},
		{
			input:    "'a' < 'z';",
			expected: "true",
		},
	}

	runVmTest(t, tests)
}

func TestStringsAndChars(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "'a' + 'b';",
			expected: "\"ab\"",
		},
		{
			input:    "\"proto \" + \"language\";",
			expected: "\"proto language\"",
		},
		{
			input:    "\"proto \" + \"language\" + '!';",
			expected: "\"proto language!\"",
		},
		{
			input:    "\"proto \" + \"language\" + '!' + \" It is fun!\";",
			expected: "\"proto language! It is fun!\"",
		},
	}

	runVmTest(t, tests)
}

func TestIfConditional(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "if 1 < 2 { true; }",
			expected: "true",
		},
		{
			input:    "if 3 * 2 == 6 { 6; }",
			expected: "6",
		},
		{
			input:    "if 1 > 2 { 1; } else { 2; }",
			expected: "2",
		},
		{
			input:    "if 1 > 2 { 1; } else if 2 > 3 { 2; } else { 3; }",
			expected: "3",
		},
		{
			input:    "if 1 > 2 { 1 } else if 2 <= 3 { 2 } else { 3 };",
			expected: "2",
		},
		{
			input:    "if false { 1 } else { 2 }",
			expected: "2", // this is because the condition is not popped off the stack
		},
	}

	runVmTest(t, tests)
}

func TestGlobalUseOfIdentifiers(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "let un: i64 = 1; un;",
			expected: "1",
		},
		{
			input:    "let un = 1; let deux = 2; un + deux;",
			expected: "3",
		},
		{
			input:    "let un = 1; let deux = 2; if un + deux >= 3 { 3 } else { un + deux }",
			expected: "3",
		},
	}

	runVmTest(t, tests)
}

func TestMakingArrays(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "[i64;];",
			expected: "[]",
		},
		{
			input:    "let a = 4; [1, 2, 3, a];",
			expected: "[1, 2, 3, 4]",
		},
		{
			input:    "let a = 4; [1, 2, 3, a, a + 1];",
			expected: "[1, 2, 3, 4, 5]",
		},
	}

	runVmTest(t, tests)
}

func TestIndexingArrays(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "let a = [1, 2, 3]; a[1];",
			expected: "2",
		},
		{
			input:    "let a = [1, 2, 3]; a[1 + 1];",
			expected: "3",
		},
		{
			input:    "let a = [1, 2, 3, 4]; let b = 2; a[b + 1];",
			expected: "4",
		},
		{
			input:    "let a = [1, 2, 3]; let b = [0]; a[b[0]];",
			expected: "1",
		},
		{
			input:    "let a = [i64; 1, 2, 3]; a[2];",
			expected: "3",
		},
		{
			input:    "let a = [[1, 2, 3], [4, 5, 6]]; a[0];",
			expected: "[1, 2, 3]",
		},
		{
			input:    "let a = [[1, 2, 3], [4, 5, 6]]; a[0][1];",
			expected: "2",
		},
	}

	runVmTest(t, tests)
}

func TestBlocks(t *testing.T) {
	tests := []vmTestCase{
		{
			input:    "let a = 3; { let b = 3; let c = a + b; };",
			expected: "()",
		},
		{
			input:    "{ let a = 4; let a = 6; let b = a + 1; let c = b + a; };",
			expected: "()",
		},
		{
			input:    "{ let a = 4; let a = 6; let b = a + 1; let c = b + a; c };",
			expected: "13",
		},
		{
			input:    "let a: bool = true; { let a = a; a };",
			expected: "true",
		},
	}

	runVmTest(t, tests)
}

func runVmTest(t *testing.T, tests []vmTestCase) {
	t.Helper()

	for i, tt := range tests {
		prog := parser.Parse(tt.input)
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
		vm.Run()

		if vm.FoundError {
			t.Fatal("Found errors during virtual machine execution")
		}

		res := vm.LastPoppedElem()

		if res == nil {
			res = vm.StackTop()
		}

		if res.LiteralRepr() != tt.expected {
			t.Fatalf("%d. Expected %s as stack top but found %s.", i, tt.expected, res.LiteralRepr())
		}
	}
}
