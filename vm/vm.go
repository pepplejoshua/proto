package vm

import (
	"fmt"
	"proto/ast"
	"proto/compiler"
	"proto/opcode"
	"proto/shared"
)

const STACK_SIZE = 2048

type VM struct {
	constants    []ast.ProtoNode
	instructions opcode.VMInstructions

	FoundError  bool
	stack       []ast.ProtoNode
	stack_index int
}

func NewVM(bc *compiler.ByteCode) *VM {
	return &VM{
		constants:    bc.Constants,
		instructions: bc.Instructions,
		FoundError:   false,
		stack:        make([]ast.ProtoNode, STACK_SIZE),
		stack_index:  0,
	}
}

func (vm *VM) StackTop() ast.ProtoNode {
	if vm.stack_index == 0 {
		return nil
	}
	return vm.stack[vm.stack_index-1]
}

func (vm *VM) PushOntoStack(item ast.ProtoNode) {
	if vm.stack_index >= STACK_SIZE {
		shared.ReportErrorAndExit("VM", "Stack Overflow")
	}
	vm.stack[vm.stack_index] = item
	vm.stack_index++
}

func (vm *VM) LoadConstant(ip int) int {
	cons_index := opcode.ReadUInt16(vm.instructions[ip+1:])
	vm.PushOntoStack(vm.constants[cons_index])
	return ip + 2
}

func (vm *VM) Run() {
	operations_dispatch := map[byte]func(int) int{
		byte(opcode.LoadConstant): vm.LoadConstant,
	}
	for ins_p := 0; ins_p < len(vm.instructions); ins_p++ {
		op := vm.instructions[ins_p]
		if dispatch, ok := operations_dispatch[op]; ok {
			ins_p = dispatch(ins_p)
		} else {
			if def, err := opcode.LookupInstructionDef(opcode.OpCode(op)); err != nil {
				shared.ReportErrorAndExit("VM", err.Error())
			} else {
				shared.ReportErrorAndExit("VM",
					fmt.Sprintf("OpCode %s found at Instruction index %d is not implemented in VM",
						def.Name, ins_p))
			}
		}
	}
}
