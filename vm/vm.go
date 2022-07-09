package vm

import (
	"fmt"
	"proto/ast"
	"proto/compiler"
	"proto/lexer"
	"proto/opcode"
	"proto/shared"
	"strconv"
	"strings"
)

const STACK_SIZE = 2048
const GLOBALS_SIZE = 65536

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

var UNIT = &ast.Unit{
	Token: lexer.ProtoToken{
		Type:      lexer.OPEN_PAREN,
		Literal:   "()",
		TokenSpan: lexer.Span{},
	},
}

type VM struct {
	constants    []ast.ProtoNode
	instructions opcode.VMInstructions

	FoundError bool

	stack       []ast.ProtoNode
	stack_index int

	globals []ast.ProtoNode
}

func NewVM(bc *compiler.ByteCode) *VM {
	return &VM{
		constants:    bc.Constants,
		instructions: bc.Instructions,
		FoundError:   false,
		stack:        make([]ast.ProtoNode, STACK_SIZE),
		stack_index:  0,
		globals:      make([]ast.ProtoNode, GLOBALS_SIZE),
	}
}

func (vm *VM) StackTop() ast.ProtoNode {
	return vm.stack[vm.stack_index-1]
}

func (vm *VM) LastPoppedElem() ast.ProtoNode {
	return vm.stack[vm.stack_index]
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

func MakeInt64(val string) int64 {
	num, _ := strconv.ParseInt(val, 10, 64)
	return num
}

func (vm *VM) AddI64(ip int) int {
	rhs := vm.PopOffStack().(*ast.I64)
	lhs := vm.PopOffStack().(*ast.I64)
	l_val := MakeInt64(lhs.LiteralRepr())
	r_val := MakeInt64(rhs.LiteralRepr())
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

func (vm *VM) AddChar(ip int) int {
	rhs := vm.PopOffStack().(*ast.Char)
	lhs := vm.PopOffStack().(*ast.Char)
	n_val := "\"" + lhs.Token.Literal + rhs.Token.Literal + "\""
	val := &ast.String{
		Token: lexer.ProtoToken{
			Type:      lexer.CHAR,
			Literal:   n_val,
			TokenSpan: lexer.Span{},
		},
	}
	vm.PushOntoStack(val)
	return ip + 1
}

func (vm *VM) AddStr(ip int) int {
	rhs := vm.PopOffStack().(*ast.String)
	lhs := vm.PopOffStack().(*ast.String)
	n_val := "\"" + lhs.Token.Literal[1:len(lhs.Token.Literal)-1] +
		rhs.Token.Literal[1:len(rhs.Token.Literal)-1] + "\""
	val := &ast.String{
		Token: lexer.ProtoToken{
			Type:      lexer.STRING,
			Literal:   n_val,
			TokenSpan: lexer.Span{},
		},
	}
	vm.PushOntoStack(val)
	return ip + 1
}

func (vm *VM) AddStrChar(ip int) int {
	rhs := vm.PopOffStack().(*ast.Char)
	lhs := vm.PopOffStack().(*ast.String)
	n_val := "\"" + lhs.Token.Literal[1:len(lhs.Token.Literal)-1] +
		rhs.Token.Literal + "\""
	val := &ast.String{
		Token: lexer.ProtoToken{
			Type:      lexer.STRING,
			Literal:   n_val,
			TokenSpan: lexer.Span{},
		},
	}
	vm.PushOntoStack(val)
	return ip + 1
}

func (vm *VM) OpCodePop(ip int) int {
	vm.stack_index--
	return ip + 1
}

func (vm *VM) SubI64(ip int) int {
	rhs := vm.PopOffStack().(*ast.I64)
	lhs := vm.PopOffStack().(*ast.I64)
	l_val := MakeInt64(lhs.LiteralRepr())
	r_val := MakeInt64(rhs.LiteralRepr())
	n_val := l_val - r_val
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

func (vm *VM) MultI64(ip int) int {
	rhs := vm.PopOffStack().(*ast.I64)
	lhs := vm.PopOffStack().(*ast.I64)
	l_val := MakeInt64(lhs.LiteralRepr())
	r_val := MakeInt64(rhs.LiteralRepr())
	n_val := l_val * r_val
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

func (vm *VM) DivI64(ip int) int {
	rhs := vm.PopOffStack().(*ast.I64)
	lhs := vm.PopOffStack().(*ast.I64)
	l_val := MakeInt64(lhs.LiteralRepr())
	r_val := MakeInt64(rhs.LiteralRepr())
	if r_val == 0 {
		var err strings.Builder
		err.WriteString("Division by 0 is not allowed.")
		shared.ReportErrorAndExit("VM", err.String())
	}
	n_val := l_val / r_val
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

func (vm *VM) ModuloI64(ip int) int {
	rhs := vm.PopOffStack().(*ast.I64)
	lhs := vm.PopOffStack().(*ast.I64)
	l_val := MakeInt64(lhs.LiteralRepr())
	r_val := MakeInt64(rhs.LiteralRepr())
	if r_val == 0 {
		var err strings.Builder
		err.WriteString("Modulo by 0 is not allowed.")
		shared.ReportErrorAndExit("VM", err.String())
	}
	n_val := l_val % r_val
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

func (vm *VM) NegateI64(ip int) int {
	op := vm.PopOffStack().(*ast.I64)
	op_val := MakeInt64(op.LiteralRepr())
	n_val := -op_val
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

func (vm *VM) NegateBool(ip int) int {
	op := vm.PopOffStack().(*ast.Boolean)
	n_val := !op.Value
	val := &ast.Boolean{
		Value: n_val,
		Token: lexer.ProtoToken{
			Type:      "",
			Literal:   "",
			TokenSpan: lexer.Span{},
		},
	}

	if n_val {
		val.Token.Literal = "true"
		val.Token.Type = lexer.TRUE
	} else {
		val.Token.Literal = "false"
		val.Token.Type = lexer.FALSE
	}
	vm.PushOntoStack(val)
	return ip + 1
}

func (vm *VM) EqualsComp(ip int) int {
	rhs := vm.PopOffStack()
	lhs := vm.PopOffStack()
	val := lhs.LiteralRepr() == rhs.LiteralRepr()
	if val {
		vm.PushOntoStack(TRUE)
	} else {
		vm.PushOntoStack(FALSE)
	}
	return ip + 1
}

func (vm *VM) NotEqualsComp(ip int) int {
	rhs := vm.PopOffStack()
	lhs := vm.PopOffStack()
	val := lhs.LiteralRepr() != rhs.LiteralRepr()
	if val {
		vm.PushOntoStack(TRUE)
	} else {
		vm.PushOntoStack(FALSE)
	}

	return ip + 1
}

func (vm *VM) GreaterThanComp(ip int) int {
	rhs := vm.PopOffStack()
	lhs := vm.PopOffStack()

	switch lhs.(type) {
	case *ast.Char:
		l_val := lhs.LiteralRepr()[1 : len(lhs.LiteralRepr())-1]
		r_val := rhs.LiteralRepr()[1 : len(rhs.LiteralRepr())-1]
		if l_val > r_val {
			vm.PushOntoStack(TRUE)
		} else {
			vm.PushOntoStack(FALSE)
		}
	case *ast.I64:
		l_val := MakeInt64(lhs.LiteralRepr())
		r_val := MakeInt64(rhs.LiteralRepr())
		if l_val > r_val {
			vm.PushOntoStack(TRUE)
		} else {
			vm.PushOntoStack(FALSE)
		}
	}

	return ip + 1
}

func (vm *VM) GreaterEqualsComp(ip int) int {
	rhs := vm.PopOffStack()
	lhs := vm.PopOffStack()

	switch lhs.(type) {
	case *ast.Char:
		l_val := lhs.LiteralRepr()[1 : len(lhs.LiteralRepr())-1]
		r_val := rhs.LiteralRepr()[1 : len(rhs.LiteralRepr())-1]
		if l_val >= r_val {
			vm.PushOntoStack(TRUE)
		} else {
			vm.PushOntoStack(FALSE)
		}
	case *ast.I64:
		l_val := MakeInt64(lhs.LiteralRepr())
		r_val := MakeInt64(rhs.LiteralRepr())
		if l_val >= r_val {
			vm.PushOntoStack(TRUE)
		} else {
			vm.PushOntoStack(FALSE)
		}
	}

	return ip + 1
}

func (vm *VM) And(ip int) int {
	rhs := vm.PopOffStack().(*ast.Boolean)
	lhs := vm.PopOffStack().(*ast.Boolean)

	if lhs.Value && rhs.Value {
		vm.PushOntoStack(TRUE)
	} else {
		vm.PushOntoStack(FALSE)
	}
	return ip + 1
}

func (vm *VM) Or(ip int) int {
	rhs := vm.PopOffStack().(*ast.Boolean)
	lhs := vm.PopOffStack().(*ast.Boolean)

	if lhs.Value || rhs.Value {
		vm.PushOntoStack(TRUE)
	} else {
		vm.PushOntoStack(FALSE)
	}
	return ip + 1
}

func (vm *VM) JumpOnNotTrueTo(ip int) int {
	val := vm.PopOffStack().(*ast.Boolean)

	if !val.Value {
		new_ip := opcode.ReadUInt16(vm.instructions[ip+1:])
		return int(new_ip)
	}
	return ip + 3
}

func (vm *VM) JumpTo(ip int) int {
	new_ip := opcode.ReadUInt16(vm.instructions[ip+1:])
	return int(new_ip)
}

func (vm *VM) PushUnit(ip int) int {
	vm.PushOntoStack(UNIT)
	return ip + 1
}

func (vm *VM) SetGlobal(ip int) int {
	globalIndex := opcode.ReadUInt16(vm.instructions[ip+1:])
	vm.globals[globalIndex] = vm.PopOffStack()
	return ip + 3
}

func (vm *VM) GetGlobal(ip int) int {
	globalIndex := opcode.ReadUInt16(vm.instructions[ip+1:])
	val := vm.globals[globalIndex]
	vm.PushOntoStack(val)
	return ip + 3
}

func (vm *VM) Run() {
	operations_dispatch := map[byte]func(int) int{
		byte(opcode.LoadConstant):      vm.LoadConstant,
		byte(opcode.PushBoolTrue):      vm.PushBoolTrue,
		byte(opcode.PushBoolFalse):     vm.PushBoolFalse,
		byte(opcode.AddI64):            vm.AddI64,
		byte(opcode.AddChar):           vm.AddChar,
		byte(opcode.AddStr):            vm.AddStr,
		byte(opcode.AddStrChar):        vm.AddStrChar,
		byte(opcode.Pop):               vm.OpCodePop,
		byte(opcode.SubI64):            vm.SubI64,
		byte(opcode.MultI64):           vm.MultI64,
		byte(opcode.DivI64):            vm.DivI64,
		byte(opcode.ModuloI64):         vm.ModuloI64,
		byte(opcode.NegateI64):         vm.NegateI64,
		byte(opcode.NegateBool):        vm.NegateBool,
		byte(opcode.EqualsComp):        vm.EqualsComp,
		byte(opcode.NotEqualsComp):     vm.NotEqualsComp,
		byte(opcode.GreaterThanComp):   vm.GreaterThanComp,
		byte(opcode.GreaterEqualsComp): vm.GreaterEqualsComp,
		byte(opcode.And):               vm.And,
		byte(opcode.Or):                vm.Or,
		byte(opcode.JumpOnNotTrueTo):   vm.JumpOnNotTrueTo,
		byte(opcode.JumpTo):            vm.JumpTo,
		byte(opcode.PushUnit):          vm.PushUnit,
		byte(opcode.SetGlobal):         vm.SetGlobal,
		byte(opcode.GetGlobal):         vm.GetGlobal,
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
}
