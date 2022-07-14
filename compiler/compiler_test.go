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
			input: "-1; fn main() { }",
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
			input:             "not true; fn main() { }",
			expectedConstants: []string{},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.PushBoolTrue),
				opcode.MakeInstruction(opcode.NegateBool),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input:             "not false; fn main() { }",
			expectedConstants: []string{},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.PushBoolFalse),
				opcode.MakeInstruction(opcode.NegateBool),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "1 + -1; fn main() { }",
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
			input: "fn main() { 1 + 2; }",
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
			input: "fn main() { 1 + 1; }",
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
			input: "fn main() { 'a' + 'b'; }",
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
			input: "fn main() { \"proto \" + \"language\"; }",
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
			input: "fn main() { \"proto \" + \"language\" + '!'; }",
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
			input: "fn main() { 1 + 2 + 3 + 1; }",
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
			input: "fn main() { 1 - 2; }",
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
			input: "fn main() { 1 * 2; }",
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
			input: "fn main() { 2 / 1; }",
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
			input: "fn main() { 1 % 2; }",
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
			input: "fn main() { 1 + 2 * 3; }",
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
			input: "fn main() { 1 * 2 + 3; }",
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
		{
			input: "fn main() { 1 == 2; }",
			expectedConstants: []string{
				"1",
				"2",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.EqualsComp),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input:             "fn main() { true == false; }",
			expectedConstants: []string{},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.PushBoolTrue),
				opcode.MakeInstruction(opcode.PushBoolFalse),
				opcode.MakeInstruction(opcode.EqualsComp),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "fn main() { 1 != 1; }",
			expectedConstants: []string{
				"1",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.NotEqualsComp),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "fn main() { 2 > 1; }",
			expectedConstants: []string{
				"2",
				"1",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.GreaterThanComp),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "fn main() { 1 < 2; }",
			expectedConstants: []string{
				"2",
				"1",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.GreaterThanComp),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "fn main() { 3 >= 2; }",
			expectedConstants: []string{
				"3",
				"2",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.GreaterEqualsComp),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "fn main() { 2 <= 4; }",
			expectedConstants: []string{
				"4",
				"2",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.GreaterEqualsComp),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
	}

	runCompilerTest(t, tests)
}

func TestBooleanValues(t *testing.T) {
	tests := []compilerTestCase{
		{
			input:             "fn main() { true; }",
			expectedConstants: []string{},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.PushBoolTrue),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input:             "fn main() { false; }",
			expectedConstants: []string{},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.PushBoolFalse),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input:             "fn main() { false; true; }",
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

func TestAndOr(t *testing.T) {
	tests := []compilerTestCase{
		{
			input:             "fn main() { true || false; }",
			expectedConstants: []string{},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.PushBoolTrue),
				opcode.MakeInstruction(opcode.PushBoolFalse),
				opcode.MakeInstruction(opcode.Or),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input:             "fn main() { false && true; }",
			expectedConstants: []string{},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.PushBoolFalse),
				opcode.MakeInstruction(opcode.PushBoolTrue),
				opcode.MakeInstruction(opcode.And),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
	}

	runCompilerTest(t, tests)
}

func TestStringAndChar(t *testing.T) {
	tests := []compilerTestCase{
		{
			input: "fn main() { 'a'; 'b'; 'c'; 'a'; }",
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
			input: "fn main() { \"this is a string\"; \"another string\"; }",
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
			input: "fn main() { \"this is a string\"; 'c'; 'd'; }",
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

func TestIfConditionals(t *testing.T) {
	tests := []compilerTestCase{
		{
			input: "fn main() { if true { 10; } 20; }",
			expectedConstants: []string{
				"10",
				"20",
			},
			expectedIns: []opcode.VMInstructions{
				// 0
				opcode.MakeInstruction(opcode.PushBoolTrue),
				// 1
				opcode.MakeInstruction(opcode.JumpOnNotTrueTo, 11),
				// 4
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				// 7
				opcode.MakeInstruction(opcode.Pop),
				// 8
				opcode.MakeInstruction(opcode.JumpTo, 12),
				// 11
				opcode.MakeInstruction(opcode.PushUnit),
				// 12
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				// 15
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "fn main() { if true { 10 }; 20; }",
			expectedConstants: []string{
				"10",
				"20",
			},
			expectedIns: []opcode.VMInstructions{
				// 0
				opcode.MakeInstruction(opcode.PushBoolTrue),
				// 1
				opcode.MakeInstruction(opcode.JumpOnNotTrueTo, 10),
				// 4
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				// 7
				opcode.MakeInstruction(opcode.JumpTo, 11),
				// 10
				opcode.MakeInstruction(opcode.PushUnit),
				// 11
				opcode.MakeInstruction(opcode.Pop),
				// 12
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				// 15
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "fn main() { if 1 < 2 { true } else { false } 10000; }",
			expectedConstants: []string{
				"2",
				"1",
				"10000",
			},
			expectedIns: []opcode.VMInstructions{
				// 0
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				// 3
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				// 6
				opcode.MakeInstruction(opcode.GreaterThanComp),
				// 7
				opcode.MakeInstruction(opcode.JumpOnNotTrueTo, 14),
				// 10
				opcode.MakeInstruction(opcode.PushBoolTrue),
				// 11
				opcode.MakeInstruction(opcode.JumpTo, 15),
				// 14
				opcode.MakeInstruction(opcode.PushBoolFalse),
				// 15
				opcode.MakeInstruction(opcode.LoadConstant, 2),
				// 18
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "fn main() { if 1 < 2 { true } else { false }; 10000; }",
			expectedConstants: []string{
				"2",
				"1",
				"10000",
			},
			expectedIns: []opcode.VMInstructions{
				// 0
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				// 3
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				// 6
				opcode.MakeInstruction(opcode.GreaterThanComp),
				// 7
				opcode.MakeInstruction(opcode.JumpOnNotTrueTo, 14),
				// 10
				opcode.MakeInstruction(opcode.PushBoolTrue),
				// 11
				opcode.MakeInstruction(opcode.JumpTo, 15),
				// 14
				opcode.MakeInstruction(opcode.PushBoolFalse),
				// 15
				opcode.MakeInstruction(opcode.Pop),
				// 16
				opcode.MakeInstruction(opcode.LoadConstant, 2),
				// 19
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "fn main() { if 1 < 2 { true } else if 2 < 1{ false } else { true }; 10000; }",
			expectedConstants: []string{
				"2",
				"1",
				"10000",
			},
			expectedIns: []opcode.VMInstructions{
				// 0 - 2
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				// 3 - 1
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				// 6 - 2 > 1
				opcode.MakeInstruction(opcode.GreaterThanComp),
				// 7 - Jump to first else
				opcode.MakeInstruction(opcode.JumpOnNotTrueTo, 14),
				// 10 - true
				opcode.MakeInstruction(opcode.PushBoolTrue),
				// 11 - just past rest of if expr to Pop (because of semi-colon)
				opcode.MakeInstruction(opcode.JumpTo, 29),
				// 14 - 1
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				// 17 - 2
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				// 20 - 1 > 2
				opcode.MakeInstruction(opcode.GreaterThanComp),
				// 21 - jump to second else
				opcode.MakeInstruction(opcode.JumpOnNotTrueTo, 28),
				// 24 - false
				opcode.MakeInstruction(opcode.PushBoolFalse),
				// 25 - jump past second else to Pop (because of semi-colon)
				opcode.MakeInstruction(opcode.JumpTo, 29),
				// 28 - true
				opcode.MakeInstruction(opcode.PushBoolTrue),
				// 29
				opcode.MakeInstruction(opcode.Pop),
				// 30
				opcode.MakeInstruction(opcode.LoadConstant, 2),
				// 33
				opcode.MakeInstruction(opcode.Pop),
			},
		},
	}

	runCompilerTest(t, tests)
}

func TestGlobalUseOfIdentifiers(t *testing.T) {
	tests := []compilerTestCase{
		{
			input: "let one = 1; let two = 2; fn main() { }",
			expectedConstants: []string{
				"1",
				"2",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.SetGlobal, 1),
			},
		},
		{
			input: "let un: i64 = 1; un; let deux = 2; deux; let un: i64 = 1; fn main() { }",
			expectedConstants: []string{
				"1",
				"2",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
				opcode.MakeInstruction(opcode.GetGlobal, 0),
				opcode.MakeInstruction(opcode.Pop),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.SetGlobal, 1),
				opcode.MakeInstruction(opcode.GetGlobal, 1),
				opcode.MakeInstruction(opcode.Pop),
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
			},
		},
	}

	runCompilerTest(t, tests)
}

func TestMakingArrays(t *testing.T) {
	tests := []compilerTestCase{
		{
			input:             "fn main() { [i64;]; }",
			expectedConstants: []string{},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.MakeArray, 0),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "let a = 4; fn main() { [1, 2, 3, a]; }",
			expectedConstants: []string{
				"4",
				"1",
				"2",
				"3",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.LoadConstant, 2),
				opcode.MakeInstruction(opcode.LoadConstant, 3),
				opcode.MakeInstruction(opcode.GetGlobal, 0),
				opcode.MakeInstruction(opcode.MakeArray, 4),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "let a = 4; fn main() { [1, 2, 3, a, a + 1]; }",
			expectedConstants: []string{
				"4",
				"1",
				"2",
				"3",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.LoadConstant, 2),
				opcode.MakeInstruction(opcode.LoadConstant, 3),
				opcode.MakeInstruction(opcode.GetGlobal, 0),
				opcode.MakeInstruction(opcode.GetGlobal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.AddI64),
				opcode.MakeInstruction(opcode.MakeArray, 5),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{},
	}

	runCompilerTest(t, tests)
}

func TestIndexExpressions(t *testing.T) {
	tests := []compilerTestCase{
		{
			input: "fn main() { [1, 2, 3][1]; }",
			expectedConstants: []string{
				"1",
				"2",
				"3",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.LoadConstant, 2),
				opcode.MakeInstruction(opcode.MakeArray, 3),
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.AccessIndex),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "let a = [1, 2, 3]; fn main() { a[1]; }",
			expectedConstants: []string{
				"1",
				"2",
				"3",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.LoadConstant, 2),
				opcode.MakeInstruction(opcode.MakeArray, 3),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
				opcode.MakeInstruction(opcode.GetGlobal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.AccessIndex),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "let a = [1, 2, 3]; let b = 2; fn main() { a[b]; }",
			expectedConstants: []string{
				"1",
				"2",
				"3",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.LoadConstant, 2),
				opcode.MakeInstruction(opcode.MakeArray, 3),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.SetGlobal, 1),
				opcode.MakeInstruction(opcode.GetGlobal, 0),
				opcode.MakeInstruction(opcode.GetGlobal, 1),
				opcode.MakeInstruction(opcode.AccessIndex),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
		{
			input: "let a = [1, 2, 3]; let b = 1; fn main() { a[b + 1]; }",
			expectedConstants: []string{
				"1",
				"2",
				"3",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.LoadConstant, 2),
				opcode.MakeInstruction(opcode.MakeArray, 3),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.SetGlobal, 1),
				opcode.MakeInstruction(opcode.GetGlobal, 0),
				opcode.MakeInstruction(opcode.GetGlobal, 1),
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.AddI64),
				opcode.MakeInstruction(opcode.AccessIndex),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
	}

	runCompilerTest(t, tests)
}

func TestBlocks(t *testing.T) {
	tests := []compilerTestCase{
		{
			input: "let a = 3; fn main() { { let b = 3; let c = a + b; } }",
			expectedConstants: []string{
				"3",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.GetGlobal, 0),
				opcode.MakeInstruction(opcode.GetLocal, 0),
				opcode.MakeInstruction(opcode.AddI64),
				opcode.MakeInstruction(opcode.PushUnit),
				opcode.MakeInstruction(opcode.PopN, 2),
			},
		},
		{
			input: "{ let a = 4; let a = 6; let b = a + 1; let c = b + a; }",
			expectedConstants: []string{
				"4",
				"6",
				"1",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.SetLocal, 0),
				opcode.MakeInstruction(opcode.GetLocal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 2),
				opcode.MakeInstruction(opcode.AddI64),
				opcode.MakeInstruction(opcode.GetLocal, 1),
				opcode.MakeInstruction(opcode.GetLocal, 0),
				opcode.MakeInstruction(opcode.AddI64),
				opcode.MakeInstruction(opcode.PushUnit),
				opcode.MakeInstruction(opcode.PopN, 3),
			},
		},
		{
			input: "fn main() { { let a = 4; let a = 6; let b = a + 1; let c = b + a; c } }",
			expectedConstants: []string{
				"4",
				"6",
				"1",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.SetLocal, 0),
				opcode.MakeInstruction(opcode.GetLocal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 2),
				opcode.MakeInstruction(opcode.AddI64),
				opcode.MakeInstruction(opcode.GetLocal, 1),
				opcode.MakeInstruction(opcode.GetLocal, 0),
				opcode.MakeInstruction(opcode.AddI64),
				opcode.MakeInstruction(opcode.GetLocal, 2),
				opcode.MakeInstruction(opcode.Pop),
				opcode.MakeInstruction(opcode.PushUnit),
				opcode.MakeInstruction(opcode.PopN, 3),
			},
		},
		{
			input: "fn main() { { let a = 10; { let b = a; b } } }",
			expectedConstants: []string{
				"10",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.GetLocal, 0),
				opcode.MakeInstruction(opcode.GetLocal, 1),
				opcode.MakeInstruction(opcode.Pop),
				opcode.MakeInstruction(opcode.PushUnit),
				opcode.MakeInstruction(opcode.Pop),
				opcode.MakeInstruction(opcode.Pop),
			},
		},
	}

	runCompilerTest(t, tests)
}

func TestSimpleAssignment(t *testing.T) {
	tests := []compilerTestCase{
		{
			input: "mut a = 0; fn main() { a += 2; }",
			expectedConstants: []string{
				"0",
				"2",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
				opcode.MakeInstruction(opcode.GetGlobal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.AddI64),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
			},
		},
		{
			input: "mut a = \"scop\"; fn main() { a += 'e'; }",
			expectedConstants: []string{
				"\"scop\"",
				"'e'",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
				opcode.MakeInstruction(opcode.GetGlobal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.AddStrChar),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
			},
		},
		{
			input: "mut a = \"scop\"; fn main() { a += \"ed\"; }",
			expectedConstants: []string{
				`"scop"`,
				`"ed"`,
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
				opcode.MakeInstruction(opcode.GetGlobal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.AddStr),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
			},
		},
		{
			input: "mut a = 5; fn main() { a -= 3; }",
			expectedConstants: []string{
				"5",
				"3",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
				opcode.MakeInstruction(opcode.GetGlobal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.SubI64),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
			},
		},
		{
			input: "mut a = 5; fn main() { a *= 3; }",
			expectedConstants: []string{
				"5",
				"3",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
				opcode.MakeInstruction(opcode.GetGlobal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.MultI64),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
			},
		},
		{
			input: "mut a = 5; fn main() { a %= 3; }",
			expectedConstants: []string{
				"5",
				"3",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
				opcode.MakeInstruction(opcode.GetGlobal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.ModuloI64),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
			},
		},
		{
			input: "mut a = 5; fn main() { a /= 2; }",
			expectedConstants: []string{
				"5",
				"2",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
				opcode.MakeInstruction(opcode.GetGlobal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.DivI64),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
			},
		},
		{
			input: "mut a = 0; fn main() { a += 10; a -= 5; a *= 2; a %= 0; a /= 2; }",
			expectedConstants: []string{
				"0",
				"10",
				"5",
				"2",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
				opcode.MakeInstruction(opcode.GetGlobal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.AddI64),
				opcode.MakeInstruction(opcode.SetGlobal, 0),

				opcode.MakeInstruction(opcode.GetGlobal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 2),
				opcode.MakeInstruction(opcode.SubI64),
				opcode.MakeInstruction(opcode.SetGlobal, 0),

				opcode.MakeInstruction(opcode.GetGlobal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 3),
				opcode.MakeInstruction(opcode.MultI64),
				opcode.MakeInstruction(opcode.SetGlobal, 0),

				opcode.MakeInstruction(opcode.GetGlobal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.ModuloI64),
				opcode.MakeInstruction(opcode.SetGlobal, 0),

				opcode.MakeInstruction(opcode.GetGlobal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 3),
				opcode.MakeInstruction(opcode.DivI64),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
			},
		},
		{
			input: "mut a = 0; a = 2; mut b = \"\"; b = \"stuff\"; mut c = 'a'; c = 'b'; mut d = true; d = false; fn main() { }",
			expectedConstants: []string{
				"0",
				"2",
				`""`,
				`"stuff"`,
				"'a'",
				"'b'",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 1),
				opcode.MakeInstruction(opcode.SetGlobal, 0),

				opcode.MakeInstruction(opcode.LoadConstant, 2),
				opcode.MakeInstruction(opcode.SetGlobal, 1),
				opcode.MakeInstruction(opcode.LoadConstant, 3),
				opcode.MakeInstruction(opcode.SetGlobal, 1),

				opcode.MakeInstruction(opcode.LoadConstant, 4),
				opcode.MakeInstruction(opcode.SetGlobal, 2),
				opcode.MakeInstruction(opcode.LoadConstant, 5),
				opcode.MakeInstruction(opcode.SetGlobal, 2),

				opcode.MakeInstruction(opcode.PushBoolTrue),
				opcode.MakeInstruction(opcode.SetGlobal, 3),
				opcode.MakeInstruction(opcode.PushBoolFalse),
				opcode.MakeInstruction(opcode.SetGlobal, 3),
			},
		},
		{
			input: "mut a = 0; fn main() { { mut b = a; b += 0; } }",
			expectedConstants: []string{
				"0",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.SetGlobal, 0),
				opcode.MakeInstruction(opcode.GetGlobal, 0),
				opcode.MakeInstruction(opcode.GetLocal, 0),
				opcode.MakeInstruction(opcode.LoadConstant, 0),
				opcode.MakeInstruction(opcode.AddI64),
				opcode.MakeInstruction(opcode.SetLocal, 0),
				opcode.MakeInstruction(opcode.PushUnit),
				opcode.MakeInstruction(opcode.PopN, 1),
			},
		},
	}

	runCompilerTest(t, tests)
}

func TestFunctionCompilation(t *testing.T) {
	// MakeFn arity, start of fn, name index
	tests := []compilerTestCase{
		{
			input:             "fn main() { }",
			expectedConstants: []string{},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.JumpTo, 6),
				opcode.MakeInstruction(opcode.PushUnit),
				opcode.MakeInstruction(opcode.Return),
				opcode.MakeInstruction(opcode.Halt),
				opcode.MakeInstruction(opcode.MakeFn, 0, 3, 0),
				opcode.MakeInstruction(opcode.JumpTo, 3),
			},
		},
		{
			input:             "fn stuff() { } fn main() { }",
			expectedConstants: []string{},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.JumpTo, 5),
				opcode.MakeInstruction(opcode.PushUnit),
				opcode.MakeInstruction(opcode.Return),
				opcode.MakeInstruction(opcode.MakeFn, 0, 3, 0),
				opcode.MakeInstruction(opcode.JumpTo, 17), // 14
				opcode.MakeInstruction(opcode.PushUnit),
				opcode.MakeInstruction(opcode.Return),
				opcode.MakeInstruction(opcode.Halt),
				opcode.MakeInstruction(opcode.MakeFn, 0, 14, 1),
				opcode.MakeInstruction(opcode.JumpTo, 14),
			},
		},
		{
			input: "fn returns() -> i64 { 1 } fn main() { }",
			expectedConstants: []string{
				"1",
			},
			expectedIns: []opcode.VMInstructions{
				opcode.MakeInstruction(opcode.JumpTo, 7),        // 3
				opcode.MakeInstruction(opcode.LoadConstant, 0),  // 6
				opcode.MakeInstruction(opcode.Return),           // 7
				opcode.MakeInstruction(opcode.MakeFn, 0, 3, 0),  // 13
				opcode.MakeInstruction(opcode.JumpTo, 19),       // 16
				opcode.MakeInstruction(opcode.PushUnit),         // 17
				opcode.MakeInstruction(opcode.Return),           // 18
				opcode.MakeInstruction(opcode.Halt),             // 19
				opcode.MakeInstruction(opcode.MakeFn, 0, 16, 1), // 25
				opcode.MakeInstruction(opcode.JumpTo, 16),       // 28
			},
		},
	}

	runCompilerTest(t, tests)
}

func concatInstructions(ins []opcode.VMInstructions) opcode.VMInstructions {
	conc := opcode.VMInstructions{}
	for _, in := range ins {
		conc = append(conc, in...)
	}
	return conc
}

func runCompilerTest(t *testing.T, tests []compilerTestCase) {
	t.Helper()

	for _, tt := range tests {
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

func testInstructions(t *testing.T, exp opcode.VMInstructions, ins opcode.VMInstructions) {
	t.Helper()
	if len(exp) != len(ins) {
		t.Fatalf("Wrong instructions.\nwant: %q\ngot: %q", exp.Disassemble(), ins.Disassemble())
	}

	for i, in := range exp {
		if ins[i] != in {
			t.Fatalf("Wrong instruction at %d.\nwant: %q\ngot: %q", i, exp.Disassemble(), ins.Disassemble())
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
