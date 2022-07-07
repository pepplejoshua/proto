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
)

var Definitions = map[OpCode]*InstructionDef{
	LoadConstant:  {"LoadConstant", []int{2}}, // max size of constants pool is 65535 (starting at 0)
	PushBoolTrue:  {"PushBoolTrue", []int{}},  // an OpCode for pushing a true value onto stack
	PushBoolFalse: {"PushBoolFalse", []int{}}, // an OpCode for pushing a false value onto stack
	AddI64:        {"AddI64", []int{}},        // add 2 i64 numbers
	AddChar:       {"AddChar", []int{}},       // add 2 characters
	AddStr:        {"AddStr", []int{}},        // add 2 strings
	AddStrChar:    {"AddStrChar", []int{}},    // add a character to a string
	Pop:           {"Pop", []int{}},           // pop a value off the stack
	SubI64:        {"SubI64", []int{}},
	MultI64:       {"MultI64", []int{}},
	DivI64:        {"DivI64", []int{}},
	ModuloI64:     {"ModuloI64", []int{}},
	NegateI64:     {"NegateI64", []int{}},
	NegateBool:    {"NegateBool", []int{}},
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
