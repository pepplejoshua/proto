package opcode

import "testing"

func TestMakingOpCode(t *testing.T) {
	tests := []struct {
		op                  OpCode
		operands            []int
		expectedInstruction []byte
	}{
		{LoadConstant, []int{65534}, []byte{byte(LoadConstant), 255, 254}},
		{PushBoolFalse, []int{}, []byte{byte(PushBoolFalse)}},
		{PushBoolTrue, []int{}, []byte{byte(PushBoolTrue)}},
		{AddI64, []int{}, []byte{byte(AddI64)}},
		{AddChar, []int{}, []byte{byte(AddChar)}},
		{AddStr, []int{}, []byte{byte(AddStr)}},
		{AddStrChar, []int{}, []byte{byte(AddStrChar)}},
		{Pop, []int{}, []byte{byte(Pop)}},
		{SubI64, []int{}, []byte{byte(SubI64)}},
		{MultI64, []int{}, []byte{byte(MultI64)}},
		{DivI64, []int{}, []byte{byte(DivI64)}},
		{ModuloI64, []int{}, []byte{byte(ModuloI64)}},
		{NegateI64, []int{}, []byte{byte(NegateI64)}},
		{NegateBool, []int{}, []byte{byte(NegateBool)}},
		{EqualsComp, []int{}, []byte{byte(EqualsComp)}},
		{NotEqualsComp, []int{}, []byte{byte(NotEqualsComp)}},
		{GreaterThanComp, []int{}, []byte{byte(GreaterThanComp)}},
		{GreaterEqualsComp, []int{}, []byte{byte(GreaterEqualsComp)}},
		{And, []int{}, []byte{byte(And)}},
		{Or, []int{}, []byte{byte(Or)}},
		{JumpOnNotTrueTo, []int{10}, []byte{byte(JumpOnNotTrueTo), 0, 10}},
		{JumpTo, []int{100}, []byte{byte(JumpTo), 0, 100}},
		{PushUnit, []int{}, []byte{byte(PushUnit)}},
		{SetGlobal, []int{5}, []byte{byte(SetGlobal), 0, 5}},
		{GetGlobal, []int{100}, []byte{byte(GetGlobal), 0, 100}},
		{MakeArray, []int{10}, []byte{byte(MakeArray), 0, 10}},
		{AccessIndex, []int{}, []byte{byte(AccessIndex)}},
		{GetLocal, []int{0}, []byte{byte(GetLocal), 0, 0}},
		{SetLocal, []int{0}, []byte{byte(SetLocal), 0, 0}},
		{PopN, []int{2}, []byte{byte(PopN), 0, 2}},
		{MakeFn, []int{0, 2, 3}, []byte{byte(MakeFn), 0, 0, 2, 0, 3}},
		{Return, []int{}, []byte{byte(Return)}},
		{Halt, []int{}, []byte{byte(Halt)}},
		{EnterScope, []int{}, []byte{byte(EnterScope)}},
		{ExitScope, []int{}, []byte{byte(ExitScope)}},
		{CallFn, []int{2}, []byte{byte(CallFn), 0, 2}},
		{MakeTuple, []int{4}, []byte{byte(MakeTuple), 0, 4}},
		{UpdateIndex, []int{}, []byte{byte(UpdateIndex)}},
		{AccessMember, []int{}, []byte{byte(AccessMember)}},
	}

	for _, test := range tests {
		ins := MakeInstruction(test.op, test.operands...)

		if len(ins) != len(test.expectedInstruction) {
			t.Errorf("Expected %d for instruction length but got %d",
				len(test.expectedInstruction), len(ins))
		}

		for i, b := range test.expectedInstruction {
			if ins[i] != test.expectedInstruction[i] {
				t.Errorf("Expected %d at instruction position %d but got %d",
					b, i, ins[i])
			}
		}
	}
}

func concatInstructions(ins []VMInstructions) VMInstructions {
	conc := VMInstructions{}
	for _, in := range ins {
		conc = append(conc, in...)
	}
	return conc
}

func TestInstructionsString(t *testing.T) {
	ins := concatInstructions([]VMInstructions{
		MakeInstruction(LoadConstant, 1),
		MakeInstruction(LoadConstant, 2),
		MakeInstruction(LoadConstant, 65535),
		MakeInstruction(PushBoolFalse),
		MakeInstruction(PushBoolTrue),
		MakeInstruction(AddI64),
		MakeInstruction(AddChar),
		MakeInstruction(AddStr),
		MakeInstruction(AddStrChar),
		MakeInstruction(Pop),
		MakeInstruction(SubI64),
		MakeInstruction(MultI64),
		MakeInstruction(DivI64),
		MakeInstruction(ModuloI64),
		MakeInstruction(NegateI64),
		MakeInstruction(NegateBool),
		MakeInstruction(EqualsComp),
		MakeInstruction(NotEqualsComp),
		MakeInstruction(GreaterThanComp),
		MakeInstruction(GreaterEqualsComp),
		MakeInstruction(And),
		MakeInstruction(Or),
		MakeInstruction(JumpOnNotTrueTo, 10),
		MakeInstruction(JumpTo, 100),
		MakeInstruction(PushUnit),
		MakeInstruction(SetGlobal, 0),
		MakeInstruction(GetGlobal, 10),
		MakeInstruction(MakeArray, 5),
		MakeInstruction(AccessIndex),
		MakeInstruction(GetLocal, 0),
		MakeInstruction(SetLocal, 2),
		MakeInstruction(PopN, 4),
		MakeInstruction(MakeFn, 0, 4, 0),
		MakeInstruction(Return),
		MakeInstruction(Halt),
		MakeInstruction(EnterScope),
		MakeInstruction(ExitScope),
		MakeInstruction(CallFn, 3),
		MakeInstruction(MakeTuple, 10),
		MakeInstruction(UpdateIndex),
		MakeInstruction(AccessMember),
		// MakeInstruction(),
	})

	exp := `0000 LoadConstant 1
0003 LoadConstant 2
0006 LoadConstant 65535
0009 PushBoolFalse
0010 PushBoolTrue
0011 AddI64
0012 AddChar
0013 AddStr
0014 AddStrChar
0015 Pop
0016 SubI64
0017 MultI64
0018 DivI64
0019 ModuloI64
0020 NegateI64
0021 NegateBool
0022 EqualsComp
0023 NotEqualsComp
0024 GreaterThanComp
0025 GreaterEqualsComp
0026 And
0027 Or
0028 JumpOnNotTrueTo 10
0031 JumpTo 100
0034 PushUnit
0035 SetGlobal 0
0038 GetGlobal 10
0041 MakeArray 5
0044 AccessIndex
0045 GetLocal 0
0048 SetLocal 2
0051 PopN 4
0054 MakeFn 0 4 0
0060 Return
0061 Halt
0062 EnterScope
0063 ExitScope
0064 CallFn 3
0067 MakeTuple 10
0070 UpdateIndex
0071 AccessMember
`

	if ins.Disassemble() != exp {
		t.Errorf("Incorrect instruction format.\nwant: %q\ngot: %q",
			exp, ins.Disassemble())
	}
}

func TestReadingOperandsOfInstruction(t *testing.T) {
	tests := []struct {
		op        OpCode
		operands  []int
		bytesRead int
	}{
		{LoadConstant, []int{65535}, 2},
		{PushBoolFalse, []int{}, 0},
		{PushBoolTrue, []int{}, 0},
		{AddI64, []int{}, 0},
		{AddChar, []int{}, 0},
		{AddStr, []int{}, 0},
		{AddStrChar, []int{}, 0},
		{Pop, []int{}, 0},
		{SubI64, []int{}, 0},
		{MultI64, []int{}, 0},
		{DivI64, []int{}, 0},
		{ModuloI64, []int{}, 0},
		{NegateI64, []int{}, 0},
		{NegateBool, []int{}, 0},
		{EqualsComp, []int{}, 0},
		{NotEqualsComp, []int{}, 0},
		{GreaterThanComp, []int{}, 0},
		{GreaterEqualsComp, []int{}, 0},
		{And, []int{}, 0},
		{Or, []int{}, 0},
		{JumpOnNotTrueTo, []int{10}, 2},
		{JumpTo, []int{100}, 2},
		{PushUnit, []int{}, 0},
		{SetGlobal, []int{10}, 2},
		{GetGlobal, []int{100}, 2},
		{MakeArray, []int{2}, 2},
		{AccessIndex, []int{}, 0},
		{GetLocal, []int{2}, 2},
		{SetLocal, []int{2}, 2},
		{PopN, []int{4}, 2},
		{MakeFn, []int{3, 2, 1}, 5},
		{Return, []int{}, 0},
		{Halt, []int{}, 0},
		{EnterScope, []int{}, 0},
		{ExitScope, []int{}, 0},
		{CallFn, []int{3}, 2},
		{MakeTuple, []int{10}, 2},
		{UpdateIndex, []int{}, 0},
		{AccessMember, []int{}, 0},
	}

	for _, tt := range tests {
		ins := MakeInstruction(tt.op, tt.operands...)
		if def, err := LookupInstructionDef(tt.op); err != nil {
			t.Fatal(err)
		} else {
			operandsRead, n := ReadOperands(def, ins[1:])
			if n != tt.bytesRead {
				t.Fatalf("expected %d read bytes but got %d", tt.bytesRead, n)
			}

			for i, operand := range tt.operands {
				if operandsRead[i] != operand {
					t.Errorf("Expected %d as operand but got %d", operand, operandsRead[i])
				}
			}
		}
	}
}
