package compiler

import (
	"proto/ast"
	"proto/opcode"
	"proto/parser"
	"testing"
)

type compilerTestCase struct {
	input             string
	expectedConstants []string
	expectedIns       []opcode.VMInstructions
}

func TestIntegerArithmetic(t *testing.T) {
	tests := []compilerTestCase{
		{
			input: "1 + 2;",
			expectedConstants: []string{
				"1", "2",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
			},
		},
		{
			input: "1 + 1;",
			expectedConstants: []string{
				"1",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 0),
			},
		},
	}

	runCompilerTest(t, tests)
}

func TestBooleanValues(t *testing.T) {
	tests := []compilerTestCase{
		{
			input:             "true;",
			expectedConstants: []string{},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.PushBoolTrue),
			},
		},
		{
			input:             "false;",
			expectedConstants: []string{},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.PushBoolFalse),
			},
		},
		{
			input:             "false; true;",
			expectedConstants: []string{},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.PushBoolFalse),
				opcode.MakeInstruction(opcode.PushBoolTrue),
			},
		},
	}

	runCompilerTest(t, tests)
}

func runCompilerTest(t *testing.T, tests []compilerTestCase) {
	t.Helper()

	for _, tt := range tests {
		prog := parser.Parse(tt.input)
		compiler := NewCompiler()

		compiler.CompileProgram(prog)

		if compiler.FoundError {
			t.Fatal("Found errors during compilation")
		}

		bc := compiler.ByteCode()

		testInstructions(t, concatInstructions(tt.expectedIns), bc.Instructions)
		testConstants(t, tt.expectedConstants, bc.Constants)
	}
}

func concatInstructions(ins []opcode.VMInstructions) opcode.VMInstructions {
	conc := opcode.VMInstructions{}
	for _, in := range ins {
		conc = append(conc, in...)
	}
	return conc
}

func testInstructions(t *testing.T, exp opcode.VMInstructions, ins opcode.VMInstructions) {
	t.Helper()
	if len(exp) != len(ins) {
		t.Errorf("Wrong instructions.\nwant: %q\ngot: %q", exp.Disassemble(), ins.Disassemble())
	}

	for i, in := range exp {
		if ins[i] != in {
			t.Errorf("Wrong instruction at %d.\nwant: %q\ngot: %q", i, in, ins[i])
		}
	}
}

func testConstants(t *testing.T, exp []string, cons []ast.ProtoNode) {
	t.Helper()
	if len(exp) != len(cons) {
		t.Errorf("Expected %d constants, got %d.", len(exp), len(cons))
	}

	for i, con := range exp {
		if con != cons[i].LiteralRepr() {
			t.Errorf("Expected %s at constant position %d, found %s", cons, i, cons[i].LiteralRepr())
		}
	}
}
