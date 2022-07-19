package compiler

import (
	"proto/analysis/name_resolver"
	"proto/analysis/type_checker"
	"proto/opcode"
	"proto/parser"
	"proto/runtime"
	"testing"
)

type compilerTestCase struct {
	input             string
	expectedConstants []string
	expectedIns       string
}

func TestUnaryOperations(t *testing.T) {
	tests := []compilerTestCase{
		{
			input: "-1; fn main() { }",
			expectedConstants: []string{
				"1",
			},
			expectedIns: `0000 LoadConstant 0
0003 NegateI64
0004 Pop
0005 JumpTo 11
0008 PushUnit
0009 Return
0010 Halt
0011 MakeFn 0 8 0
0017 GetGlobal 0
0020 CallFn 0
`,
		},
		{
			input:             "not true; fn main() { }",
			expectedConstants: []string{},
			expectedIns: `0000 PushBoolTrue
0001 NegateBool
0002 Pop
0003 JumpTo 9
0006 PushUnit
0007 Return
0008 Halt
0009 MakeFn 0 6 0
0015 GetGlobal 0
0018 CallFn 0
`,
		},
		{
			input:             "not false; fn main() { }",
			expectedConstants: []string{},
			expectedIns: `0000 PushBoolFalse
0001 NegateBool
0002 Pop
0003 JumpTo 9
0006 PushUnit
0007 Return
0008 Halt
0009 MakeFn 0 6 0
0015 GetGlobal 0
0018 CallFn 0
`,
		},
		{
			input:             "1 + -1; fn main() { }",
			expectedConstants: []string{"1"},
			expectedIns: `0000 LoadConstant 0
0003 LoadConstant 0
0006 NegateI64
0007 AddI64
0008 Pop
0009 JumpTo 15
0012 PushUnit
0013 Return
0014 Halt
0015 MakeFn 0 12 0
0021 GetGlobal 0
0024 CallFn 0
`,
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
			expectedIns: `0000 JumpTo 14
0003 LoadConstant 0
0006 LoadConstant 1
0009 AddI64
0010 Pop
0011 PushUnit
0012 Return
0013 Halt
0014 MakeFn 0 3 0
0020 GetGlobal 0
0023 CallFn 0
`,
		},
		{
			input: "fn main() { 1 + 1; }",
			expectedConstants: []string{
				"1",
			},
			expectedIns: `0000 JumpTo 14
0003 LoadConstant 0
0006 LoadConstant 0
0009 AddI64
0010 Pop
0011 PushUnit
0012 Return
0013 Halt
0014 MakeFn 0 3 0
0020 GetGlobal 0
0023 CallFn 0
`,
		},
		{
			input: "fn main() { 'a' + 'b'; }",
			expectedConstants: []string{
				"a",
				"b",
			},
			expectedIns: `0000 JumpTo 14
0003 LoadConstant 0
0006 LoadConstant 1
0009 AddChar
0010 Pop
0011 PushUnit
0012 Return
0013 Halt
0014 MakeFn 0 3 0
0020 GetGlobal 0
0023 CallFn 0
`,
		},
		{
			input: "fn main() { \"proto \" + \"language\"; }",
			expectedConstants: []string{
				"\"proto \"",
				"\"language\"",
			},
			expectedIns: `0000 JumpTo 14
0003 LoadConstant 0
0006 LoadConstant 1
0009 AddStr
0010 Pop
0011 PushUnit
0012 Return
0013 Halt
0014 MakeFn 0 3 0
0020 GetGlobal 0
0023 CallFn 0
`,
		},
		{
			input: "fn main() { \"proto \" + \"language\" + '!'; }",
			expectedConstants: []string{
				"\"proto \"",
				"\"language\"",
				"!",
			},
			expectedIns: `0000 JumpTo 18
0003 LoadConstant 0
0006 LoadConstant 1
0009 AddStr
0010 LoadConstant 2
0013 AddStrChar
0014 Pop
0015 PushUnit
0016 Return
0017 Halt
0018 MakeFn 0 3 0
0024 GetGlobal 0
0027 CallFn 0
`,
		},
		{
			input: "fn main() { 1 + 2 + 3 + 1; }",
			expectedConstants: []string{
				"1",
				"2",
				"3",
			},
			expectedIns: `0000 JumpTo 22
0003 LoadConstant 0
0006 LoadConstant 1
0009 AddI64
0010 LoadConstant 2
0013 AddI64
0014 LoadConstant 0
0017 AddI64
0018 Pop
0019 PushUnit
0020 Return
0021 Halt
0022 MakeFn 0 3 0
0028 GetGlobal 0
0031 CallFn 0
`,
		},
		{
			input: "fn main() { 1 - 2; }",
			expectedConstants: []string{
				"1",
				"2",
			},
			expectedIns: `0000 JumpTo 14
0003 LoadConstant 0
0006 LoadConstant 1
0009 SubI64
0010 Pop
0011 PushUnit
0012 Return
0013 Halt
0014 MakeFn 0 3 0
0020 GetGlobal 0
0023 CallFn 0
`,
		},
		{
			input: "fn main() { 1 * 2; }",
			expectedConstants: []string{
				"1",
				"2",
			},
			expectedIns: `0000 JumpTo 14
0003 LoadConstant 0
0006 LoadConstant 1
0009 MultI64
0010 Pop
0011 PushUnit
0012 Return
0013 Halt
0014 MakeFn 0 3 0
0020 GetGlobal 0
0023 CallFn 0
`,
		},
		{
			input: "fn main() { 2 / 1; }",
			expectedConstants: []string{
				"2",
				"1",
			},
			expectedIns: `0000 JumpTo 14
0003 LoadConstant 0
0006 LoadConstant 1
0009 DivI64
0010 Pop
0011 PushUnit
0012 Return
0013 Halt
0014 MakeFn 0 3 0
0020 GetGlobal 0
0023 CallFn 0
`,
		},
		{
			input: "fn main() { 1 % 2; }",
			expectedConstants: []string{
				"1",
				"2",
			},
			expectedIns: `0000 JumpTo 14
0003 LoadConstant 0
0006 LoadConstant 1
0009 ModuloI64
0010 Pop
0011 PushUnit
0012 Return
0013 Halt
0014 MakeFn 0 3 0
0020 GetGlobal 0
0023 CallFn 0
`,
		},
		{
			input: "fn main() { 1 + 2 * 3; }",
			expectedConstants: []string{
				"1",
				"2",
				"3",
			},
			expectedIns: `0000 JumpTo 18
0003 LoadConstant 0
0006 LoadConstant 1
0009 LoadConstant 2
0012 MultI64
0013 AddI64
0014 Pop
0015 PushUnit
0016 Return
0017 Halt
0018 MakeFn 0 3 0
0024 GetGlobal 0
0027 CallFn 0
`,
		},
		{
			input: "fn main() { 1 * 2 + 3; }",
			expectedConstants: []string{
				"1",
				"2",
				"3",
			},
			expectedIns: `0000 JumpTo 18
0003 LoadConstant 0
0006 LoadConstant 1
0009 MultI64
0010 LoadConstant 2
0013 AddI64
0014 Pop
0015 PushUnit
0016 Return
0017 Halt
0018 MakeFn 0 3 0
0024 GetGlobal 0
0027 CallFn 0
`,
		},
		{
			input: "fn main() { 1 == 2; }",
			expectedConstants: []string{
				"1",
				"2",
			},
			expectedIns: `0000 JumpTo 14
0003 LoadConstant 0
0006 LoadConstant 1
0009 EqualsComp
0010 Pop
0011 PushUnit
0012 Return
0013 Halt
0014 MakeFn 0 3 0
0020 GetGlobal 0
0023 CallFn 0
`,
		},
		{
			input:             "fn main() { true == false; }",
			expectedConstants: []string{},
			expectedIns: `0000 JumpTo 10
0003 PushBoolTrue
0004 PushBoolFalse
0005 EqualsComp
0006 Pop
0007 PushUnit
0008 Return
0009 Halt
0010 MakeFn 0 3 0
0016 GetGlobal 0
0019 CallFn 0
`,
		},
		{
			input: "fn main() { 1 != 1; }",
			expectedConstants: []string{
				"1",
			},
			expectedIns: `0000 JumpTo 14
0003 LoadConstant 0
0006 LoadConstant 0
0009 NotEqualsComp
0010 Pop
0011 PushUnit
0012 Return
0013 Halt
0014 MakeFn 0 3 0
0020 GetGlobal 0
0023 CallFn 0
`,
		},
		{
			input: "fn main() { 2 > 1; }",
			expectedConstants: []string{
				"2",
				"1",
			},
			expectedIns: `0000 JumpTo 14
0003 LoadConstant 0
0006 LoadConstant 1
0009 GreaterThanComp
0010 Pop
0011 PushUnit
0012 Return
0013 Halt
0014 MakeFn 0 3 0
0020 GetGlobal 0
0023 CallFn 0
`,
		},
		{
			input: "fn main() { 1 < 2; }",
			expectedConstants: []string{
				"2",
				"1",
			},
			expectedIns: `0000 JumpTo 14
0003 LoadConstant 0
0006 LoadConstant 1
0009 GreaterThanComp
0010 Pop
0011 PushUnit
0012 Return
0013 Halt
0014 MakeFn 0 3 0
0020 GetGlobal 0
0023 CallFn 0
`,
		},
		{
			input: "fn main() { 3 >= 2; }",
			expectedConstants: []string{
				"3",
				"2",
			},
			expectedIns: `0000 JumpTo 14
0003 LoadConstant 0
0006 LoadConstant 1
0009 GreaterEqualsComp
0010 Pop
0011 PushUnit
0012 Return
0013 Halt
0014 MakeFn 0 3 0
0020 GetGlobal 0
0023 CallFn 0
`,
		},
		{
			input: "fn main() { 2 <= 4; }",
			expectedConstants: []string{
				"4",
				"2",
			},
			expectedIns: `0000 JumpTo 14
0003 LoadConstant 0
0006 LoadConstant 1
0009 GreaterEqualsComp
0010 Pop
0011 PushUnit
0012 Return
0013 Halt
0014 MakeFn 0 3 0
0020 GetGlobal 0
0023 CallFn 0
`,
		},
	}

	runCompilerTest(t, tests)
}

func TestBooleanValues(t *testing.T) {
	tests := []compilerTestCase{
		{
			input:             "fn main() { true; }",
			expectedConstants: []string{},
			expectedIns: `0000 JumpTo 8
0003 PushBoolTrue
0004 Pop
0005 PushUnit
0006 Return
0007 Halt
0008 MakeFn 0 3 0
0014 GetGlobal 0
0017 CallFn 0
`,
		},
		{
			input:             "fn main() { false; }",
			expectedConstants: []string{},
			expectedIns: `0000 JumpTo 8
0003 PushBoolFalse
0004 Pop
0005 PushUnit
0006 Return
0007 Halt
0008 MakeFn 0 3 0
0014 GetGlobal 0
0017 CallFn 0
`,
		},
		{
			input:             "fn main() { false; true; }",
			expectedConstants: []string{},
			expectedIns: `0000 JumpTo 10
0003 PushBoolFalse
0004 Pop
0005 PushBoolTrue
0006 Pop
0007 PushUnit
0008 Return
0009 Halt
0010 MakeFn 0 3 0
0016 GetGlobal 0
0019 CallFn 0
`,
		},
	}

	runCompilerTest(t, tests)
}

func TestAndOr(t *testing.T) {
	tests := []compilerTestCase{
		{
			input:             "fn main() { true || false; }",
			expectedConstants: []string{},
			expectedIns: `0000 JumpTo 10
0003 PushBoolTrue
0004 PushBoolFalse
0005 Or
0006 Pop
0007 PushUnit
0008 Return
0009 Halt
0010 MakeFn 0 3 0
0016 GetGlobal 0
0019 CallFn 0
`,
		},
		{
			input:             "fn main() { false && true; }",
			expectedConstants: []string{},
			expectedIns: `0000 JumpTo 10
0003 PushBoolFalse
0004 PushBoolTrue
0005 And
0006 Pop
0007 PushUnit
0008 Return
0009 Halt
0010 MakeFn 0 3 0
0016 GetGlobal 0
0019 CallFn 0
`,
		},
	}

	runCompilerTest(t, tests)
}

func TestStringAndChar(t *testing.T) {
	tests := []compilerTestCase{
		{
			input:             "fn main() { 'a'; 'b'; 'c'; 'a'; }",
			expectedConstants: []string{"a", "b", "c"},
			expectedIns: `0000 JumpTo 22
0003 LoadConstant 0
0006 Pop
0007 LoadConstant 1
0010 Pop
0011 LoadConstant 2
0014 Pop
0015 LoadConstant 0
0018 Pop
0019 PushUnit
0020 Return
0021 Halt
0022 MakeFn 0 3 0
0028 GetGlobal 0
0031 CallFn 0
`,
		},
		{
			input:             "fn main() { \"this is a string\"; \"another string\"; }",
			expectedConstants: []string{"\"this is a string\"", "\"another string\""},
			expectedIns: `0000 JumpTo 14
0003 LoadConstant 0
0006 Pop
0007 LoadConstant 1
0010 Pop
0011 PushUnit
0012 Return
0013 Halt
0014 MakeFn 0 3 0
0020 GetGlobal 0
0023 CallFn 0
`,
		},
		{
			input:             "fn main() { \"this is a string\"; 'c'; 'd'; }",
			expectedConstants: []string{"\"this is a string\"", "c", "d"},
			expectedIns: `0000 JumpTo 18
0003 LoadConstant 0
0006 Pop
0007 LoadConstant 1
0010 Pop
0011 LoadConstant 2
0014 Pop
0015 PushUnit
0016 Return
0017 Halt
0018 MakeFn 0 3 0
0024 GetGlobal 0
0027 CallFn 0
`,
		},
	}

	runCompilerTest(t, tests)
}

func TestIfConditionals(t *testing.T) {
	tests := []compilerTestCase{
		{
			input:             "fn main() { if true { 10; } 20; }",
			expectedConstants: []string{"10", "20"},
			expectedIns: `0000 JumpTo 23
0003 PushBoolTrue
0004 JumpOnNotTrueTo 15
0007 LoadConstant 0
0010 Pop
0011 PushUnit
0012 JumpTo 16
0015 PushUnit
0016 LoadConstant 1
0019 Pop
0020 PushUnit
0021 Return
0022 Halt
0023 MakeFn 0 3 0
0029 GetGlobal 0
0032 CallFn 0
`,
		},
		{
			input:             "fn main() { if true { 10 }; 20; }",
			expectedConstants: []string{"10", "20"},
			expectedIns: `0000 JumpTo 22
0003 PushBoolTrue
0004 JumpOnNotTrueTo 13
0007 LoadConstant 0
0010 JumpTo 14
0013 PushUnit
0014 Pop
0015 LoadConstant 1
0018 Pop
0019 PushUnit
0020 Return
0021 Halt
0022 MakeFn 0 3 0
0028 GetGlobal 0
0031 CallFn 0
`,
		},
		{
			input:             "fn main() { if 1 < 2 { true } else { false } 10000; }",
			expectedConstants: []string{"2", "1", "10000"},
			expectedIns: `0000 JumpTo 25
0003 LoadConstant 0
0006 LoadConstant 1
0009 GreaterThanComp
0010 JumpOnNotTrueTo 17
0013 PushBoolTrue
0014 JumpTo 18
0017 PushBoolFalse
0018 LoadConstant 2
0021 Pop
0022 PushUnit
0023 Return
0024 Halt
0025 MakeFn 0 3 0
0031 GetGlobal 0
0034 CallFn 0
`,
		},
		{
			input:             "fn main() { if 1 < 2 { true } else { false }; 10000; }",
			expectedConstants: []string{"2", "1", "10000"},
			expectedIns: `0000 JumpTo 26
0003 LoadConstant 0
0006 LoadConstant 1
0009 GreaterThanComp
0010 JumpOnNotTrueTo 17
0013 PushBoolTrue
0014 JumpTo 18
0017 PushBoolFalse
0018 Pop
0019 LoadConstant 2
0022 Pop
0023 PushUnit
0024 Return
0025 Halt
0026 MakeFn 0 3 0
0032 GetGlobal 0
0035 CallFn 0
`,
		},
		{
			input: "fn main() { if 1 < 2 { true } else if 2 < 1{ false } else { true }; 10000; }",
			expectedConstants: []string{
				"2",
				"1",
				"10000",
			},
			expectedIns: `0000 JumpTo 40
0003 LoadConstant 0
0006 LoadConstant 1
0009 GreaterThanComp
0010 JumpOnNotTrueTo 17
0013 PushBoolTrue
0014 JumpTo 32
0017 LoadConstant 1
0020 LoadConstant 0
0023 GreaterThanComp
0024 JumpOnNotTrueTo 31
0027 PushBoolFalse
0028 JumpTo 32
0031 PushBoolTrue
0032 Pop
0033 LoadConstant 2
0036 Pop
0037 PushUnit
0038 Return
0039 Halt
0040 MakeFn 0 3 0
0046 GetGlobal 0
0049 CallFn 0
`,
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
			expectedIns: `0000 LoadConstant 0
0003 SetGlobal 1
0006 LoadConstant 1
0009 SetGlobal 2
0012 JumpTo 18
0015 PushUnit
0016 Return
0017 Halt
0018 MakeFn 0 15 0
0024 GetGlobal 0
0027 CallFn 0
`,
		},
		{
			input:             "let un: i64 = 1; un; let deux = 2; deux; let un: i64 = 1; fn main() { }",
			expectedConstants: []string{"1", "2"},
			expectedIns: `0000 LoadConstant 0
0003 SetGlobal 1
0006 GetGlobal 1
0009 Pop
0010 LoadConstant 1
0013 SetGlobal 2
0016 GetGlobal 2
0019 Pop
0020 LoadConstant 0
0023 SetGlobal 1
0026 JumpTo 32
0029 PushUnit
0030 Return
0031 Halt
0032 MakeFn 0 29 0
0038 GetGlobal 0
0041 CallFn 0
`,
		},
	}

	runCompilerTest(t, tests)
}

func TestMakingArrays(t *testing.T) {
	tests := []compilerTestCase{
		{
			input:             "fn main() { [i64;]; }",
			expectedConstants: []string{},
			expectedIns: `0000 JumpTo 10
0003 MakeArray 0
0006 Pop
0007 PushUnit
0008 Return
0009 Halt
0010 MakeFn 0 3 0
0016 GetGlobal 0
0019 CallFn 0
`,
		},
		{
			input:             "let a = 4; fn main() { [1, 2, 3, a]; }",
			expectedConstants: []string{"4", "1", "2", "3"},
			expectedIns: `0000 LoadConstant 0
0003 SetGlobal 1
0006 JumpTo 28
0009 LoadConstant 1
0012 LoadConstant 2
0015 LoadConstant 3
0018 GetGlobal 1
0021 MakeArray 4
0024 Pop
0025 PushUnit
0026 Return
0027 Halt
0028 MakeFn 0 9 0
0034 GetGlobal 0
0037 CallFn 0
`,
		},
		{
			input:             "let a = 4; fn main() { [1, 2, 3, a, a + 1]; }",
			expectedConstants: []string{"4", "1", "2", "3"},
			expectedIns: `0000 LoadConstant 0
0003 SetGlobal 1
0006 JumpTo 35
0009 LoadConstant 1
0012 LoadConstant 2
0015 LoadConstant 3
0018 GetGlobal 1
0021 GetGlobal 1
0024 LoadConstant 1
0027 AddI64
0028 MakeArray 5
0031 Pop
0032 PushUnit
0033 Return
0034 Halt
0035 MakeFn 0 9 0
0041 GetGlobal 0
0044 CallFn 0
`,
		},
	}

	runCompilerTest(t, tests)
}

func TestIndexExpressions(t *testing.T) {
	tests := []compilerTestCase{
		{
			input:             "fn main() { [1, 2, 3][1]; }",
			expectedConstants: []string{"1", "2", "3"},
			expectedIns: `0000 JumpTo 23
0003 LoadConstant 0
0006 LoadConstant 1
0009 LoadConstant 2
0012 MakeArray 3
0015 LoadConstant 0
0018 AccessIndex
0019 Pop
0020 PushUnit
0021 Return
0022 Halt
0023 MakeFn 0 3 0
0029 GetGlobal 0
0032 CallFn 0
`,
		},
		{
			input:             "let a = [1, 2, 3]; fn main() { a[1]; }",
			expectedConstants: []string{"1", "2", "3"},
			expectedIns: `0000 LoadConstant 0
0003 LoadConstant 1
0006 LoadConstant 2
0009 MakeArray 3
0012 SetGlobal 1
0015 JumpTo 29
0018 GetGlobal 1
0021 LoadConstant 0
0024 AccessIndex
0025 Pop
0026 PushUnit
0027 Return
0028 Halt
0029 MakeFn 0 18 0
0035 GetGlobal 0
0038 CallFn 0
`,
		},
		{
			input:             "let a = [1, 2, 3]; let b = 2; fn main() { a[b]; }",
			expectedConstants: []string{"1", "2", "3"},
			expectedIns: `0000 LoadConstant 0
0003 LoadConstant 1
0006 LoadConstant 2
0009 MakeArray 3
0012 SetGlobal 1
0015 LoadConstant 1
0018 SetGlobal 2
0021 JumpTo 35
0024 GetGlobal 1
0027 GetGlobal 2
0030 AccessIndex
0031 Pop
0032 PushUnit
0033 Return
0034 Halt
0035 MakeFn 0 24 0
0041 GetGlobal 0
0044 CallFn 0
`,
		},
		{
			input:             "let a = [1, 2, 3]; let b = 1; fn main() { a[b + 1]; }",
			expectedConstants: []string{"1", "2", "3"},
			expectedIns: `0000 LoadConstant 0
0003 LoadConstant 1
0006 LoadConstant 2
0009 MakeArray 3
0012 SetGlobal 1
0015 LoadConstant 0
0018 SetGlobal 2
0021 JumpTo 39
0024 GetGlobal 1
0027 GetGlobal 2
0030 LoadConstant 0
0033 AddI64
0034 AccessIndex
0035 Pop
0036 PushUnit
0037 Return
0038 Halt
0039 MakeFn 0 24 0
0045 GetGlobal 0
0048 CallFn 0
`,
		},
	}

	runCompilerTest(t, tests)
}

func TestBlocks(t *testing.T) {
	tests := []compilerTestCase{
		{
			input:             "let a = 3; fn main() { { let b = 3; let c = a + b; } }",
			expectedConstants: []string{"3"},
			expectedIns: `0000 LoadConstant 0
0003 SetGlobal 1
0006 JumpTo 25
0009 LoadConstant 0
0012 GetGlobal 1
0015 GetLocal 0
0018 AddI64
0019 PushUnit
0020 PopN 2
0023 Return
0024 Halt
0025 MakeFn 0 9 0
0031 GetGlobal 0
0034 CallFn 0
`,
		},
		{
			input:             "fn main() { let a = 4; let b = 6; { let a = 6; let b = b + 1; } let c = b + a; }",
			expectedConstants: []string{"4", "6", "1"},
			expectedIns: `0000 JumpTo 36
0003 LoadConstant 0
0006 LoadConstant 1
0009 LoadConstant 1
0012 GetLocal 1
0015 LoadConstant 2
0018 AddI64
0019 PushUnit
0020 PopN 2
0023 GetLocal 1
0026 GetLocal 0
0029 AddI64
0030 PushUnit
0031 PopN 3
0034 Return
0035 Halt
0036 MakeFn 0 3 0
0042 GetGlobal 0
0045 CallFn 0
`,
		},
		{
			input:             "fn main() -> i64 { { let a = 4; let a = 6; let b = a + 1; let c = b + a; c } }",
			expectedConstants: []string{"4", "6", "1"},
			expectedIns: `0000 JumpTo 34
0003 LoadConstant 0
0006 LoadConstant 1
0009 SetLocal 0
0012 GetLocal 0
0015 LoadConstant 2
0018 AddI64
0019 GetLocal 1
0022 GetLocal 0
0025 AddI64
0026 GetLocal 2
0029 PopN 3
0032 Return
0033 Halt
0034 MakeFn 0 3 0
0040 GetGlobal 0
0043 CallFn 0
`,
		},
		{
			input:             "fn main() -> i64 { { let a = 10; { let b = a; b } } }",
			expectedConstants: []string{"10"},
			expectedIns: `0000 JumpTo 20
0003 LoadConstant 0
0006 GetLocal 0
0009 GetLocal 1
0012 PopN 1
0015 PopN 1
0018 Return
0019 Halt
0020 MakeFn 0 3 0
0026 GetGlobal 0
0029 CallFn 0
`,
		},
	}

	runCompilerTest(t, tests)
}

func TestSimpleAssignment(t *testing.T) {
	tests := []compilerTestCase{
		{
			input:             "mut a = 0; fn main() { a += 2; }",
			expectedConstants: []string{"0", "2"},
			expectedIns: `0000 LoadConstant 0
0003 SetGlobal 1
0006 JumpTo 22
0009 GetGlobal 1
0012 LoadConstant 1
0015 AddI64
0016 SetGlobal 1
0019 PushUnit
0020 Return
0021 Halt
0022 MakeFn 0 9 0
0028 GetGlobal 0
0031 CallFn 0
`,
		},
		{
			input:             "mut a = \"scop\"; fn main() { a += 'e'; }",
			expectedConstants: []string{"\"scop\"", "e"},
			expectedIns: `0000 LoadConstant 0
0003 SetGlobal 1
0006 JumpTo 22
0009 GetGlobal 1
0012 LoadConstant 1
0015 AddStrChar
0016 SetGlobal 1
0019 PushUnit
0020 Return
0021 Halt
0022 MakeFn 0 9 0
0028 GetGlobal 0
0031 CallFn 0
`,
		},
		{
			input:             "mut a = \"scop\"; fn main() { a += \"ed\"; }",
			expectedConstants: []string{`"scop"`, `"ed"`},
			expectedIns: `0000 LoadConstant 0
0003 SetGlobal 1
0006 JumpTo 22
0009 GetGlobal 1
0012 LoadConstant 1
0015 AddStr
0016 SetGlobal 1
0019 PushUnit
0020 Return
0021 Halt
0022 MakeFn 0 9 0
0028 GetGlobal 0
0031 CallFn 0
`,
		},
		{
			input:             "mut a = 5; fn main() { a -= 3; }",
			expectedConstants: []string{"5", "3"},
			expectedIns: `0000 LoadConstant 0
0003 SetGlobal 1
0006 JumpTo 22
0009 GetGlobal 1
0012 LoadConstant 1
0015 SubI64
0016 SetGlobal 1
0019 PushUnit
0020 Return
0021 Halt
0022 MakeFn 0 9 0
0028 GetGlobal 0
0031 CallFn 0
`,
		},
		{
			input:             "mut a = 5; fn main() { a *= 3; }",
			expectedConstants: []string{"5", "3"},
			expectedIns: `0000 LoadConstant 0
0003 SetGlobal 1
0006 JumpTo 22
0009 GetGlobal 1
0012 LoadConstant 1
0015 MultI64
0016 SetGlobal 1
0019 PushUnit
0020 Return
0021 Halt
0022 MakeFn 0 9 0
0028 GetGlobal 0
0031 CallFn 0
`,
		},
		{
			input:             "mut a = 5; fn main() { a %= 3; }",
			expectedConstants: []string{"5", "3"},
			expectedIns: `0000 LoadConstant 0
0003 SetGlobal 1
0006 JumpTo 22
0009 GetGlobal 1
0012 LoadConstant 1
0015 ModuloI64
0016 SetGlobal 1
0019 PushUnit
0020 Return
0021 Halt
0022 MakeFn 0 9 0
0028 GetGlobal 0
0031 CallFn 0
`,
		},
		{
			input:             "mut a = 5; fn main() { a /= 2; }",
			expectedConstants: []string{"5", "2"},
			expectedIns: `0000 LoadConstant 0
0003 SetGlobal 1
0006 JumpTo 22
0009 GetGlobal 1
0012 LoadConstant 1
0015 DivI64
0016 SetGlobal 1
0019 PushUnit
0020 Return
0021 Halt
0022 MakeFn 0 9 0
0028 GetGlobal 0
0031 CallFn 0
`,
		},
		{
			input:             "mut a = 0; fn main() { a += 10; a -= 5; a *= 2; a %= 0; a /= 2; }",
			expectedConstants: []string{"0", "10", "5", "2"},
			expectedIns: `0000 LoadConstant 0
0003 SetGlobal 1
0006 JumpTo 62
0009 GetGlobal 1
0012 LoadConstant 1
0015 AddI64
0016 SetGlobal 1
0019 GetGlobal 1
0022 LoadConstant 2
0025 SubI64
0026 SetGlobal 1
0029 GetGlobal 1
0032 LoadConstant 3
0035 MultI64
0036 SetGlobal 1
0039 GetGlobal 1
0042 LoadConstant 0
0045 ModuloI64
0046 SetGlobal 1
0049 GetGlobal 1
0052 LoadConstant 3
0055 DivI64
0056 SetGlobal 1
0059 PushUnit
0060 Return
0061 Halt
0062 MakeFn 0 9 0
0068 GetGlobal 0
0071 CallFn 0
`,
		},
		{
			input:             "mut a = 0; a = 2; mut b = \"\"; b = \"stuff\"; mut c = 'a'; c = 'b'; mut d = true; d = false; fn main() { }",
			expectedConstants: []string{"0", "2", `""`, `"stuff"`, "a", "b"},
			expectedIns: `0000 LoadConstant 0
0003 SetGlobal 1
0006 LoadConstant 1
0009 SetGlobal 1
0012 LoadConstant 2
0015 SetGlobal 2
0018 LoadConstant 3
0021 SetGlobal 2
0024 LoadConstant 4
0027 SetGlobal 3
0030 LoadConstant 5
0033 SetGlobal 3
0036 PushBoolTrue
0037 SetGlobal 4
0040 PushBoolFalse
0041 SetGlobal 4
0044 JumpTo 50
0047 PushUnit
0048 Return
0049 Halt
0050 MakeFn 0 47 0
0056 GetGlobal 0
0059 CallFn 0
`,
		},
		{
			input:             "mut a = 0; fn main() { { mut b = a; b += 0; } }",
			expectedConstants: []string{"0"},
			expectedIns: `0000 LoadConstant 0
0003 SetGlobal 1
0006 JumpTo 28
0009 GetGlobal 1
0012 GetLocal 0
0015 LoadConstant 0
0018 AddI64
0019 SetLocal 0
0022 PushUnit
0023 PopN 1
0026 Return
0027 Halt
0028 MakeFn 0 9 0
0034 GetGlobal 0
0037 CallFn 0
`,
		},
	}

	runCompilerTest(t, tests)
}

func TestInfiniteLoops(t *testing.T) {
	tests := []compilerTestCase{
		{
			input: "fn main() -> i64 { mut count = 10; loop { if count <= 0 { break; } count -= 1; } count }",
			expectedConstants: []string{
				"10",
				"0",
				"1",
			},
			expectedIns: `0000 JumpTo 45
0003 LoadConstant 0
0006 LoadConstant 1
0009 GetLocal 0
0012 GreaterEqualsComp
0013 JumpOnNotTrueTo 22
0016 JumpTo 37
0019 JumpTo 23
0022 PushUnit
0023 GetLocal 0
0026 LoadConstant 2
0029 SubI64
0030 SetLocal 0
0033 PushUnit
0034 JumpTo 6
0037 GetLocal 0
0040 PopN 1
0043 Return
0044 Halt
0045 MakeFn 0 3 0
0051 GetGlobal 0
0054 CallFn 0
`,
		},
		{
			input: "fn main() { mut i = 0; loop { if i == 10 { break; } else { i += 1; } } }",
			expectedConstants: []string{
				"0",
				"10",
				"1",
			},
			expectedIns: `0000 JumpTo 42
0003 LoadConstant 0
0006 GetLocal 0
0009 LoadConstant 1
0012 EqualsComp
0013 JumpOnNotTrueTo 22
0016 JumpTo 36
0019 JumpTo 33
0022 GetLocal 0
0025 LoadConstant 2
0028 AddI64
0029 SetLocal 0
0032 PushUnit
0033 JumpTo 6
0036 PushUnit
0037 PopN 1
0040 Return
0041 Halt
0042 MakeFn 0 3 0
0048 GetGlobal 0
0051 CallFn 0
`,
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
			expectedIns: `0000 JumpTo 6
0003 PushUnit
0004 Return
0005 Halt
0006 MakeFn 0 3 0
0012 GetGlobal 0
0015 CallFn 0
`,
		},
		{
			input:             "fn empty() { } fn main() { }",
			expectedConstants: []string{},
			expectedIns: `0000 JumpTo 5
0003 PushUnit
0004 Return
0005 MakeFn 0 3 0
0011 JumpTo 17
0014 PushUnit
0015 Return
0016 Halt
0017 MakeFn 0 14 1
0023 GetGlobal 1
0026 CallFn 0
`,
		},
		{
			input:             "fn returns() -> i64 { 1 } fn main() { }",
			expectedConstants: []string{"1"},
			expectedIns: `0000 JumpTo 7
0003 LoadConstant 0
0006 Return
0007 MakeFn 0 3 0
0013 JumpTo 19
0016 PushUnit
0017 Return
0018 Halt
0019 MakeFn 0 16 1
0025 GetGlobal 1
0028 CallFn 0
`,
		},
		{
			input: "fn has_local() { let a = 10; let b = 5; a; } fn main() { let a = 2; a; }",
			expectedConstants: []string{
				"10",
				"5",
				"2",
			},
			expectedIns: `0000 JumpTo 18
0003 LoadConstant 0
0006 LoadConstant 1
0009 GetLocal 0
0012 Pop
0013 PushUnit
0014 PopN 2
0017 Return
0018 MakeFn 0 3 0
0024 JumpTo 40
0027 LoadConstant 2
0030 GetLocal 0
0033 Pop
0034 PushUnit
0035 PopN 1
0038 Return
0039 Halt
0040 MakeFn 0 27 1
0046 GetGlobal 1
0049 CallFn 0
`,
		},
		{
			input: "fn returns_i64() -> i64 { return 1; } fn returns_unit() { } fn main() { returns_i64; returns_unit; }",
			expectedConstants: []string{
				"1",
			},
			expectedIns: `0000 JumpTo 7
0003 LoadConstant 0
0006 Return
0007 MakeFn 0 3 0
0013 JumpTo 18
0016 PushUnit
0017 Return
0018 MakeFn 0 16 1
0024 JumpTo 38
0027 GetGlobal 0
0030 Pop
0031 GetGlobal 1
0034 Pop
0035 PushUnit
0036 Return
0037 Halt
0038 MakeFn 0 27 2
0044 GetGlobal 2
0047 CallFn 0
`,
		},
		{
			input: "fn main() { let a = 0; let b = 1; { let c = a + b + 3; c; } fn inner() { let a = 3; a + 1; } }",
			expectedConstants: []string{
				"0",
				"1",
				"3",
			},
			expectedIns: `0000 JumpTo 59
0003 LoadConstant 0
0006 LoadConstant 1
0009 GetLocal 0
0012 GetLocal 1
0015 AddI64
0016 LoadConstant 2
0019 AddI64
0020 GetLocal 2
0023 Pop
0024 PushUnit
0025 PopN 1
0028 JumpTo 47
0031 LoadConstant 2
0034 GetLocal 0
0037 LoadConstant 1
0040 AddI64
0041 Pop
0042 PushUnit
0043 PopN 1
0046 Return
0047 MakeFn 0 31 3
0053 PushUnit
0054 PopN 3
0057 Return
0058 Halt
0059 MakeFn 0 3 0
0065 GetGlobal 0
0068 CallFn 0
`,
		},
		{
			input:             "fn main() { has_params; } fn has_params(a: str, b: str) -> str { a + b }",
			expectedConstants: []string{},
			expectedIns: `0000 JumpTo 10
0003 GetGlobal 1
0006 Pop
0007 PushUnit
0008 Return
0009 Halt
0010 MakeFn 0 3 0
0016 JumpTo 30
0019 GetLocal 0
0022 GetLocal 1
0025 AddStr
0026 PopN 2
0029 Return
0030 MakeFn 2 19 1
0036 GetGlobal 0
0039 CallFn 0
`,
		},
	}

	runCompilerTest(t, tests)
}

func TestFunctionCalls(t *testing.T) {
	tests := []compilerTestCase{
		{
			input: "fn main() { returns_1(); } fn returns_1() -> i64 { 1 }",
			expectedConstants: []string{
				"1",
			},
			expectedIns: `0000 JumpTo 13
0003 GetGlobal 1
0006 CallFn 0
0009 Pop
0010 PushUnit
0011 Return
0012 Halt
0013 MakeFn 0 3 0
0019 JumpTo 26
0022 LoadConstant 0
0025 Return
0026 MakeFn 0 22 1
0032 GetGlobal 0
0035 CallFn 0
`,
		},
		{
			input: "fn main() { fn squared(a: i64) -> i64 { a * a } let a = squared(3); a; }",
			expectedConstants: []string{
				"3",
			},
			expectedIns: `0000 JumpTo 42
0003 JumpTo 17
0006 GetLocal 0
0009 GetLocal 0
0012 MultI64
0013 PopN 1
0016 Return
0017 MakeFn 1 6 0
0023 LoadConstant 0
0026 GetLocal 0
0029 CallFn 1
0032 GetLocal 1
0035 Pop
0036 PushUnit
0037 PopN 2
0040 Return
0041 Halt
0042 MakeFn 0 3 0
0048 GetGlobal 0
0051 CallFn 0
`,
		},
	}

	runCompilerTest(t, tests)
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

		testInstructions(t, tt.expectedIns, bc.Instructions)
		testConstants(t, tt.expectedConstants, bc.Constants)
	}
}

func testInstructions(t *testing.T, exp string, ins opcode.VMInstructions) {
	t.Helper()
	if len(exp) != len(ins.Disassemble()) {
		t.Fatalf("Wrong instructions.\nwant: %q\n\n got: %q", exp, ins.Disassemble())
	}

	for index, char := range ins.Disassemble() {
		expected := rune(exp[index])
		if char != expected {
			t.Fatalf("Wrong character at %d.\nwant: %q\n\n got: %q.", index, exp, ins.Disassemble())
		}
	}
}

func testConstants(t *testing.T, exp []string, cons []runtime.RuntimeObj) {
	t.Helper()
	if len(exp) != len(cons) {
		t.Errorf("Expected %d constants, got %d.", len(exp), len(cons))
	}

	for i, con := range exp {
		if con != cons[i].String() {
			t.Errorf("Expected %s at constant position %d, found %s", con, i, cons[i])
		}
	}
}
