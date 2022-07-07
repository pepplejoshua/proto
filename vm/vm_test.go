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
	}

	runVmTest(t, tests)
}

func TestStringsAndChars(t *testing.T) {
	test := []vmTestCase{
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
	}

	runVmTest(t, test)
}

func runVmTest(t *testing.T, tests []vmTestCase) {
	t.Helper()

	for _, tt := range tests {
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

		if res.LiteralRepr() != tt.expected {
			t.Fatalf("Expected %s as stack top but found %s.", tt.expected, res.LiteralRepr())
		}
	}
}
