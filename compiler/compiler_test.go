package compiler

import (
	"proto/analysis/name_resolver"
	"proto/analysis/type_checker"
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

func TestUnaryOperations(t *testing.T) {
	tests := []compilerTestCase{
		{
			input: "-1;",
			expectedConstants: []string{
				"1",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.NegateI64),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input:             "not true;",
			expectedConstants: []string{},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.PushBoolTrue),
				opcode.MakeInstruction(opcode.NegateBool),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input:             "not false;",
			expectedConstants: []string{},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.PushBoolFalse),
				opcode.MakeInstruction(opcode.NegateBool),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "1 + -1;",
			expectedConstants: []string{
				"1",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.NegateI64),
				opcode.MakeInstruction(opcode.AddI64),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
	}

	runCompilerTest(t, tests)
}

func TestBinaryOperations(t *testing.T) {
	tests := []compilerTestCase{
		{
			input: "1 + 2;",
			expectedConstants: []string{
				"1", "2",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.AddI64),
				opcode.MakeInstruction(opcode.Pop),
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
				opcode.MakeInstruction(opcode.AddI64),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "'a' + 'b';",
			expectedConstants: []string{
				"'a'",
				"'b'",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.AddChar),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "\"proto \" + \"language\";",
			expectedConstants: []string{
				"\"proto \"",
				"\"language\"",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.AddStr),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "\"proto \" + \"language\" + '!';",
			expectedConstants: []string{
				"\"proto \"",
				"\"language\"",
				"'!'",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.AddStr),
				opcode.MakeInstruction(opcode.LoadConstant, 2),
				opcode.MakeInstruction(opcode.AddStrChar),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "1 + 2 + 3 + 1;",
			expectedConstants: []string{
				"1",
				"2",
				"3",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.AddI64),
				opcode.MakeInstruction(opcode.LoadConstant, 2),
				opcode.MakeInstruction(opcode.AddI64),
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.AddI64),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "1 - 2;",
			expectedConstants: []string{
				"1",
				"2",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.SubI64),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "1 * 2;",
			expectedConstants: []string{
				"1",
				"2",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.MultI64),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "2 / 1;",
			expectedConstants: []string{
				"2",
				"1",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.DivI64),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "1 % 2;",
			expectedConstants: []string{
				"1",
				"2",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.ModuloI64),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "1 + 2 * 3;",
			expectedConstants: []string{
				"1",
				"2",
				"3",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.LoadConstant, 2),
				opcode.MakeInstruction(opcode.MultI64),
				opcode.MakeInstruction(opcode.AddI64),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "1 * 2 + 3;",
			expectedConstants: []string{
				"1",
				"2",
				"3",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.MultI64),
				opcode.MakeInstruction(opcode.LoadConstant, 2),
				opcode.MakeInstruction(opcode.AddI64),
				opcode.MakeInstruction(opcode.Pop),
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
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input:             "false;",
			expectedConstants: []string{},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.PushBoolFalse),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input:             "false; true;",
			expectedConstants: []string{},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.PushBoolFalse),
				opcode.MakeInstruction(opcode.Pop),
				opcode.MakeInstruction(opcode.PushBoolTrue),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
	}

	runCompilerTest(t, tests)
}

func TestStringAndChar(t *testing.T) {
	tests := []compilerTestCase{
		{
			input: "'a'; 'b'; 'c'; 'a';",
			expectedConstants: []string{
				"'a'",
				"'b'",
				"'c'",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.Pop),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.Pop),
				opcode.MakeInstruction(opcode.LoadConstant, 2),
				opcode.MakeInstruction(opcode.Pop),
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "\"this is a string\"; \"another string\";",
			expectedConstants: []string{
				"\"this is a string\"",
				"\"another string\"",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.Pop),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "\"this is a string\"; 'c'; 'd';",
			expectedConstants: []string{
				"\"this is a string\"",
				"'c'",
				"'d'",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.Pop),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.Pop),
				opcode.MakeInstruction(opcode.LoadConstant, 2),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
	}

	runCompilerTest(t, tests)
}

func runCompilerTest(t *testing.T, tests []compilerTestCase) {
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
