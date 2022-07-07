package vm

import (
	"fmt"
	"proto/ast"
	"proto/compiler"
	"proto/lexer"
	"proto/opcode"
	"proto/shared"
	"strconv"
)

const STACK_SIZE = 2048

var TRUE = &ast.Boolean{
	Value: true,
	Token: lexer.ProtoToken{
		Type:      lexer.TRUE,
		Literal:   "true",
		TokenSpan: lexer.Span{},
	},
}

var FALSE = &ast.Boolean{
	Value: false,
	Token: lexer.ProtoToken{
		Type:      lexer.FALSE,
		Literal:   "false",
		TokenSpan: lexer.Span{},
	},
}

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

func (vm *VM) PopOffStack() ast.ProtoNode {
	item := vm.stack[vm.stack_index-1]
	vm.stack_index--
	return item
}

func (vm *VM) LoadConstant(ip int) int {
	cons_index := opcode.ReadUInt16(vm.instructions[ip+1:])
	vm.PushOntoStack(vm.constants[cons_index])
	return ip + 3
}

func (vm *VM) PushBoolTrue(ip int) int {
	vm.PushOntoStack(TRUE)
	return ip + 1
}

func (vm *VM) PushBoolFalse(ip int) int {
	vm.PushOntoStack(FALSE)
	return ip + 1
}

func (vm *VM) AddI64(ip int) int {
	rhs := vm.PopOffStack().(*ast.I64)
	lhs := vm.PopOffStack().(*ast.I64)
	l_val, _ := strconv.ParseInt(lhs.LiteralRepr(), 10, 64)
	r_val, _ := strconv.ParseInt(rhs.LiteralRepr(), 10, 64)
	n_val := l_val + r_val
	val := &ast.I64{
		Token: lexer.ProtoToken{
			Type:      lexer.I64,
			Literal:   fmt.Sprint(n_val),
			TokenSpan: lexer.Span{},
		},
	}
	vm.PushOntoStack(val)
	return ip + 1
}

func (vm *VM) Pop(ip int) int {
	vm.PopOffStack()
	return ip + 1
}

func (vm *VM) Run() {
	operations_dispatch := map[byte]func(int) int{
		byte(opcode.LoadConstant):  vm.LoadConstant,
		byte(opcode.PushBoolTrue):  vm.PushBoolTrue,
		byte(opcode.PushBoolFalse): vm.PushBoolFalse,
		byte(opcode.AddI64):        vm.AddI64,
		byte(opcode.Pop):           vm.Pop,
	}
	for ins_p := 0; ins_p < len(vm.instructions); {
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

	for i := vm.stack_index - 1; i >= 0; i-- {
		println(vm.stack[i].LiteralRepr())
	}
}
