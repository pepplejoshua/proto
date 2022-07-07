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
