package vm

import (
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
		// {
		// 	input:    "1 + 2;",
		// 	expected: "3",
		// },
	}

	runVmTest(t, tests)
}

func runVmTest(t *testing.T, tests []vmTestCase) {
	t.Helper()

	for _, tt := range tests {
		prog := parser.Parse(tt.input)
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

		res := vm.StackTop()

		if res.LiteralRepr() != tt.expected {
			t.Fatalf("Expected %s as stack top but found %s.", res.LiteralRepr(), tt.expected)
		}
	}
}
