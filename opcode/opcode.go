package opcode

import (
	"encoding/binary"
	"fmt"
	"strings"
)

type OpCode byte

type VMInstructions []byte

func (i VMInstructions) Disassemble() string {
	var out strings.Builder

	byteCount := 0

	for byteCount < len(i) {
		if def, err := LookupInstructionDef(OpCode(i[byteCount])); err != nil {
			out.WriteString(fmt.Sprintf("Error: %s\n", err))
			continue
		} else {
			operands, read := ReadOperands(def, i[byteCount+1:])
			out.WriteString(fmt.Sprintf("%04d %s\n",
				byteCount, i.formatInstruction(def, operands)))
			byteCount += 1 + read
		}
	}
	return out.String()
}

func (i *VMInstructions) formatInstruction(def *InstructionDef, ops []int) string {
	opcount := len(def.OperandWidths)

	if len(ops) != opcount {
		return fmt.Sprintf("Error: operand length %d does not match defined %d\n",
			len(ops), opcount)
	}

	switch opcount {
	case 0:
		return def.Name
	case 1:
		return fmt.Sprintf("%s %d", def.Name, ops[0])
	}

	return fmt.Sprintf("Error: Unhandled Operator Count %d for %s", opcount, def.Name)
}

type InstructionDef struct {
	Name string
	// how many bytes each operand will use, so [2, 3] means:
	// first operand takes 2 bytes and second operator takes 3 bytes
	OperandWidths []int
}

const (
	LoadConstant OpCode = iota
	PushBoolTrue
	PushBoolFalse
	AddI64
	AddChar
	AddStr
	AddStrChar
	Pop
	SubI64
	MultI64
	DivI64
	ModuloI64
	NegateI64
	NegateBool
	EqualsComp
	NotEqualsComp
	GreaterThanComp
	GreaterEqualsComp
	And
	Or
	JumpOnNotTrueTo
	JumpTo
	PushUnit
	SetGlobal
	GetGlobal
)

var Definitions = map[OpCode]*InstructionDef{
	LoadConstant:      {"LoadConstant", []int{2}},     // max size of constants pool is 65535 (starting at 0)
	PushBoolTrue:      {"PushBoolTrue", []int{}},      // an OpCode for pushing a true value onto stack
	PushBoolFalse:     {"PushBoolFalse", []int{}},     // an OpCode for pushing a false value onto stack
	AddI64:            {"AddI64", []int{}},            // add 2 i64 numbers
	AddChar:           {"AddChar", []int{}},           // add 2 characters
	AddStr:            {"AddStr", []int{}},            // add 2 strings
	AddStrChar:        {"AddStrChar", []int{}},        // add a character to a string
	Pop:               {"Pop", []int{}},               // pop a value off the stack
	SubI64:            {"SubI64", []int{}},            // subtract 2 i64 numbers
	MultI64:           {"MultI64", []int{}},           // multiply 2 i64 numbers
	DivI64:            {"DivI64", []int{}},            // divide 2 i64 numbers
	ModuloI64:         {"ModuloI64", []int{}},         // module between 2 i64 numbers
	NegateI64:         {"NegateI64", []int{}},         // negate an i64 number
	NegateBool:        {"NegateBool", []int{}},        // negate a boolean value
	EqualsComp:        {"EqualsComp", []int{}},        // compare 2 values for equality
	NotEqualsComp:     {"NotEqualsComp", []int{}},     // compare 2 values for inequality
	GreaterThanComp:   {"GreaterThanComp", []int{}},   // check if an i64 or char is greater than another i64 or char
	GreaterEqualsComp: {"GreaterEqualsComp", []int{}}, // check if an i64 or char is greater or equal to another i64 or char
	And:               {"And", []int{}},               // perform and on 2 boolean values
	Or:                {"Or", []int{}},                // perform or on 2 boolean values
	JumpOnNotTrueTo:   {"JumpOnNotTrueTo", []int{2}},  // jump if value at top of stack is not true to a provided location in instructions
	JumpTo:            {"JumpTo", []int{2}},           // jump to a provided location in instructions
	PushUnit:          {"PushUnit", []int{}},          // used in the case of an if expression with no else, or just pushing a unit onto stack
	SetGlobal:         {"SetGlobal", []int{2}},        // used to set the value at an index in global scope. It is used to bind global variables
	GetGlobal:         {"GetGlobal", []int{2}},        // used to get the value at the provided index in the global scope. It is used to bind global variables
}

func MakeInstruction(op OpCode, operands ...int) []byte {
	if def, ok := Definitions[op]; !ok {
		return []byte{}
	} else {
		insLen := 1
		for _, width := range def.OperandWidths {
			insLen += width
		}

		ins := make([]byte, insLen)
		ins[0] = byte(op)

		offset := 1 // where operand insertion will begin, right after the OpCode
		for i, operand := range operands {
			w := def.OperandWidths[i] // width to fit current operand in
			switch w {
			case 2:
				// turn operand into an 16 bits, and then fit it in BigEndian order
				// into the instruction array of bytes starting at offset
				binary.BigEndian.PutUint16(ins[offset:], uint16(operand))
			case 0:
			}
			offset += w // increase offset by the width of last operand
		}
		return ins
	}
}

func ReadUInt16(ins VMInstructions) uint16 {
	return binary.BigEndian.Uint16(ins)
}

func ReadOperands(def *InstructionDef, ins VMInstructions) ([]int, int) {
	// make an array to store the operands
	operands := make([]int, len(def.OperandWidths))
	offset := 0 // offset starts at 0, since we are reading past the OpCode

	// since it is the companion to MakeInstruction which encodes operands,
	// this decodes operands
	for i, width := range def.OperandWidths {
		switch width {
		case 0:
		case 2:
			val := int(ReadUInt16(ins[offset:]))
			operands[i] = val
		}
		offset += width
	}
	return operands, offset
}

func LookupInstructionDef(op OpCode) (*InstructionDef, error) {
	def, ok := Definitions[op]
	if !ok {
		return nil, fmt.Errorf("undefined OpCode: %d", op)
	}
	return def, nil
}
