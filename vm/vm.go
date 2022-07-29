package vm

import (
	"fmt"
	"proto/builtins"
	"proto/compiler"
	"proto/opcode"
	"proto/runtime"
	"proto/shared"
	"strings"
)

const STACK_SIZE = 2048
const GLOBALS_SIZE = 65536
const FRAME_SIZE = 1024

var TRUE = &runtime.Bool{
	Value: true,
}

var FALSE = &runtime.Bool{
	Value: false,
}

var UNIT = &runtime.Unit{}

type CompiledFunction struct {
	LocIP int
	Arity int
}

func (cf *CompiledFunction) String() string {
	return fmt.Sprintf("fn at Instruction #%d", cf.LocIP)
}

type CallFrame struct {
	called_fn   *CompiledFunction
	stack_index int
	return_ip   int
}

type VM struct {
	constants    []runtime.RuntimeObj
	instructions opcode.VMInstructions

	FoundError bool

	stack       []runtime.RuntimeObj
	stack_index int

	globals     []runtime.RuntimeObj
	frames      []*CallFrame
	frame_index int
}

func NewVM(bc *compiler.ByteCode) *VM {
	return &VM{
		constants:    bc.Constants,
		instructions: bc.Instructions,
		FoundError:   false,
		stack:        make([]runtime.RuntimeObj, STACK_SIZE),
		stack_index:  0,
		globals:      make([]runtime.RuntimeObj, GLOBALS_SIZE),
		frames:       make([]*CallFrame, FRAME_SIZE),
		frame_index:  0,
	}
}

func (vm *VM) StackTop() runtime.RuntimeObj {
	return vm.stack[vm.stack_index-1]
}

func (vm *VM) LastPoppedElem() runtime.RuntimeObj {
	return vm.stack[vm.stack_index]
}

func (vm *VM) PushOntoStack(item runtime.RuntimeObj) {
	if vm.stack_index >= STACK_SIZE {
		shared.ReportErrorAndExit("VM", "Stack Overflow")
	}
	vm.stack[vm.stack_index] = item
	vm.stack_index++
}

func (vm *VM) PopOffStack() runtime.RuntimeObj {
	item := vm.stack[vm.stack_index-1]
	vm.stack_index--
	return item
}

func (vm *VM) Run() {
	// println(vm.instructions.Disassemble())
	for ip := 0; ip < len(vm.instructions); {
		op := opcode.OpCode(vm.instructions[ip])
		switch op {
		case opcode.LoadConstant:
			cons_index := opcode.ReadUInt16(vm.instructions[ip+1:])
			vm.PushOntoStack(vm.constants[cons_index])
			ip += 3
		case opcode.PushBoolTrue:
			vm.PushOntoStack(TRUE)
			ip += 1
		case opcode.PushBoolFalse:
			vm.PushOntoStack(FALSE)
			ip += 1
		case opcode.AddI64, opcode.SubI64, opcode.MultI64:
			rhs := vm.PopOffStack().(*runtime.I64)
			lhs := vm.PopOffStack().(*runtime.I64)
			var n_val int64
			if op == opcode.AddI64 {
				n_val = lhs.Value + rhs.Value
			} else if op == opcode.SubI64 {
				n_val = lhs.Value - rhs.Value
			} else {
				n_val = lhs.Value * rhs.Value
			}
			val := &runtime.I64{
				Value: n_val,
			}
			vm.PushOntoStack(val)
			ip += 1
		case opcode.AddChar:
			rhs := vm.PopOffStack().(*runtime.Char)
			lhs := vm.PopOffStack().(*runtime.Char)
			n_val := "\"" + lhs.Character() + rhs.Character() + "\""
			val := &runtime.String{
				Value: n_val,
			}
			vm.PushOntoStack(val)
			ip += 1
		case opcode.AddStr:
			rhs := vm.PopOffStack().(*runtime.String)
			lhs := vm.PopOffStack().(*runtime.String)
			n_val := "\"" + lhs.Content() +
				rhs.Content() + "\""
			val := &runtime.String{
				Value: n_val,
			}
			vm.PushOntoStack(val)
			ip += 1
		case opcode.AddStrChar:
			rhs := vm.PopOffStack().(*runtime.Char)
			lhs := vm.PopOffStack().(*runtime.String)
			n_val := "\"" + lhs.Content() +
				rhs.Character() + "\""
			val := &runtime.String{
				Value: n_val,
			}
			vm.PushOntoStack(val)
			ip += 1
		case opcode.Pop:
			vm.stack_index--
			ip += 1
		case opcode.DivI64, opcode.ModuloI64:
			rhs := vm.PopOffStack().(*runtime.I64)
			lhs := vm.PopOffStack().(*runtime.I64)
			if rhs.Value == 0 {
				var err strings.Builder
				err.WriteString("Division/Modulo by 0 is not allowed.")
				shared.ReportErrorAndExit("VM", err.String())
			}
			var n_val int64
			if op == opcode.DivI64 {
				n_val = lhs.Value / rhs.Value
			} else {
				n_val = lhs.Value % rhs.Value
			}
			val := &runtime.I64{
				Value: n_val,
			}
			vm.PushOntoStack(val)
			ip += 1
		case opcode.NegateI64:
			op := vm.PopOffStack().(*runtime.I64)
			n_val := -op.Value
			val := &runtime.I64{
				Value: n_val,
			}
			vm.PushOntoStack(val)
			ip += 1
		case opcode.NegateBool:
			op := vm.PopOffStack().(*runtime.Bool)
			if op.Value {
				vm.PushOntoStack(FALSE)
			} else {
				vm.PushOntoStack(TRUE)
			}
			ip += 1
		case opcode.EqualsComp:
			rhs := vm.PopOffStack()
			lhs := vm.PopOffStack()
			val := lhs.String() == rhs.String()
			if val {
				vm.PushOntoStack(TRUE)
			} else {
				vm.PushOntoStack(FALSE)
			}
			ip += 1
		case opcode.NotEqualsComp:
			rhs := vm.PopOffStack()
			lhs := vm.PopOffStack()
			val := lhs.String() != rhs.String()
			if val {
				vm.PushOntoStack(TRUE)
			} else {
				vm.PushOntoStack(FALSE)
			}
			ip += 1
		case opcode.GreaterThanComp:
			rhs := vm.PopOffStack()
			lhs := vm.PopOffStack()

			switch lhs.(type) {
			case *runtime.Char:
				l_val := lhs.String()
				r_val := rhs.String()
				if l_val > r_val {
					vm.PushOntoStack(TRUE)
				} else {
					vm.PushOntoStack(FALSE)
				}
			case *runtime.I64:
				lhs := lhs.(*runtime.I64)
				rhs := rhs.(*runtime.I64)
				if lhs.Value > rhs.Value {
					vm.PushOntoStack(TRUE)
				} else {
					vm.PushOntoStack(FALSE)
				}
			}
			ip += 1
		case opcode.GreaterEqualsComp:
			rhs := vm.PopOffStack()
			lhs := vm.PopOffStack()

			switch lhs.(type) {
			case *runtime.Char:
				l_val := lhs.String()
				r_val := rhs.String()
				if l_val >= r_val {
					vm.PushOntoStack(TRUE)
				} else {
					vm.PushOntoStack(FALSE)
				}
			case *runtime.I64:
				lhs := lhs.(*runtime.I64)
				rhs := rhs.(*runtime.I64)
				if lhs.Value >= rhs.Value {
					vm.PushOntoStack(TRUE)
				} else {
					vm.PushOntoStack(FALSE)
				}
			}
			ip += 1
		case opcode.And, opcode.Or:
			rhs := vm.PopOffStack().(*runtime.Bool)
			lhs := vm.PopOffStack().(*runtime.Bool)

			var boolean_val bool
			if op == opcode.And {
				boolean_val = lhs.Value && rhs.Value
			} else {
				boolean_val = lhs.Value || rhs.Value
			}
			if boolean_val {
				vm.PushOntoStack(TRUE)
			} else {
				vm.PushOntoStack(FALSE)
			}
			ip += 1
		case opcode.JumpOnNotTrueTo:
			val := vm.PopOffStack().(*runtime.Bool)
			if !val.Value {
				new_ip := opcode.ReadUInt16(vm.instructions[ip+1:])
				ip = int(new_ip)
				continue
			}
			ip += 3
		case opcode.JumpTo:
			new_ip := opcode.ReadUInt16(vm.instructions[ip+1:])
			ip = int(new_ip)
		case opcode.PushUnit:
			vm.PushOntoStack(UNIT)
			ip += 1
		case opcode.SetGlobal:
			globalIndex := int(opcode.ReadUInt16(vm.instructions[ip+1:]))
			vm.globals[globalIndex] = vm.PopOffStack()
			ip += 3
		case opcode.GetGlobal:
			globalIndex := int(opcode.ReadUInt16(vm.instructions[ip+1:]))
			val := vm.globals[globalIndex]
			vm.PushOntoStack(val)
			ip += 3
		case opcode.MakeArray:
			size := int(opcode.ReadUInt16(vm.instructions[ip+1:]))

			array := &runtime.Array{
				Items: make([]runtime.RuntimeObj, size),
			}

			start := vm.stack_index - size
			for i := start; i < vm.stack_index; i++ {
				val := vm.stack[i]
				array.Items[i-start] = val
			}

			vm.stack_index = start
			vm.PushOntoStack(array)
			ip += 3
		case opcode.MakeTuple:
			size := int(opcode.ReadUInt16(vm.instructions[ip+1:]))

			tuple := &runtime.Tuple{
				Items: make([]runtime.RuntimeObj, size),
			}

			start := vm.stack_index - size
			for i := start; i < vm.stack_index; i++ {
				val := vm.stack[i]
				tuple.Items[i-start] = val
			}

			vm.stack_index = start
			vm.PushOntoStack(tuple)
			ip += 3
		case opcode.AccessIndex:
			index_t := vm.PopOffStack().(*runtime.I64)
			indexable := vm.PopOffStack().(*runtime.Array)

			if index_t.Value < 0 || int(index_t.Value) >= len(indexable.Items) {
				if len(indexable.Items) == 0 {
					shared.ReportErrorAndExit("VM", fmt.Sprintf("Provided index %d is out of range, as Array has %d items (and is not indexable).",
						index_t.Value, len(indexable.Items)))
				} else if len(indexable.Items) == 1 {
					shared.ReportErrorAndExit("VM", fmt.Sprintf("Provided index %d is out of range, as Array has %d items (indexable only by 0).",
						index_t.Value, len(indexable.Items)))
				} else {
					shared.ReportErrorAndExit("VM", fmt.Sprintf("Provided index %d is out of range, as Array has %d items (indexable from 0 to %d)",
						index_t.Value, len(indexable.Items), len(indexable.Items)-1))
				}
			}
			vm.PushOntoStack(indexable.Items[index_t.Value])
			ip += 1
		case opcode.PopN:
			N := int(opcode.ReadUInt16(vm.instructions[ip+1:]))
			res := vm.PopOffStack()
			vm.stack_index -= N
			vm.PushOntoStack(res)
			ip += 3
		case opcode.GetLocal:
			offset := int(opcode.ReadUInt16(vm.instructions[ip+1:]))
			if vm.frame_index == 0 {
				vm.PushOntoStack(vm.stack[offset])
			} else {
				vm.PushOntoStack(vm.stack[vm.frames[vm.frame_index-1].stack_index+offset])
			}
			ip += 3
		case opcode.SetLocal:
			offset := int(opcode.ReadUInt16(vm.instructions[ip+1:]))
			if vm.frame_index == 0 {
				vm.stack[offset] = vm.PopOffStack()
			} else {
				vm.stack[vm.frames[vm.frame_index-1].stack_index+offset] = vm.PopOffStack()
			}
			ip += 3
		case opcode.MakeFn:
			arity := int(opcode.ReadUInt8(vm.instructions[ip+1:]))
			loc_ip := int(opcode.ReadUInt16(vm.instructions[ip+2:]))
			storage_loc := int(opcode.ReadUInt16(vm.instructions[ip+4:]))
			fn := &CompiledFunction{
				LocIP: loc_ip,
				Arity: arity,
			}
			if vm.frame_index == 0 { // in global scope
				vm.globals[storage_loc] = fn
			} else { // in local scope
				// why does this just work???
				vm.PushOntoStack(fn)
			}
			ip += 6
		case opcode.Return:
			ret_loc := vm.frames[vm.frame_index-1].return_ip
			vm.frame_index--
			if vm.frame_index == 0 {
				ip += 1
				continue
			}
			ip = ret_loc
		case opcode.CallFn:
			arg_count := opcode.ReadUInt16(vm.instructions[ip+1:])
			fn := vm.PopOffStack().(*CompiledFunction)
			new_frame := &CallFrame{
				called_fn:   fn,
				stack_index: vm.stack_index - int(arg_count),
				return_ip:   ip + 3,
			}
			vm.frames[vm.frame_index] = new_frame
			vm.frame_index++
			ip = new_frame.called_fn.LocIP
		case opcode.UpdateIndex:
			assigned := vm.PopOffStack()
			index := vm.PopOffStack().(*runtime.I64)
			array := vm.PopOffStack().(*runtime.Array)

			if int(index.Value) < 0 || int(index.Value) >= len(array.Items) {
				if len(array.Items) == 0 {
					shared.ReportErrorAndExit("VM", fmt.Sprintf("Provided index %d is out of range, as Array has %d items (and is not Indexable).",
						index.Value, len(array.Items)))
				} else if len(array.Items) == 1 {
					shared.ReportErrorAndExit("VM", fmt.Sprintf("Provided index %d is out of range, as Array has %d items (array only by 0).",
						index.Value, len(array.Items)))
				} else {
					shared.ReportErrorAndExit("VM", fmt.Sprintf("Provided index %d is out of range, as Array has %d items (array from 0 to %d)",
						index.Value, len(array.Items), len(array.Items)-1))
				}
			}

			array.Items[index.Value] = assigned
			ip += 1
		case opcode.UpdateStructMember:
			assigned := vm.PopOffStack()
			mem := vm.PopOffStack().(*runtime.String)
			strct := vm.PopOffStack().(*runtime.InitializedStruct)

			strct.Members[mem.Value] = assigned
			ip += 1
		case opcode.AccessTupleMember:
			index := vm.PopOffStack().(*runtime.I64)
			obj := vm.PopOffStack().(*runtime.Tuple)

			val := obj.Items[index.Value]
			vm.PushOntoStack(val)
			ip += 1
		case opcode.AccessStructMember:
			mem := vm.PopOffStack().(*runtime.String)
			obj := vm.PopOffStack().(*runtime.InitializedStruct)

			val := obj.Members[mem.Value]
			vm.PushOntoStack(val)
			ip += 1
		case opcode.MakeRange:
			past_end := vm.PopOffStack()
			start := vm.PopOffStack()

			if past_end.String() <= start.String() {
				var msg strings.Builder
				msg.WriteString(start.String() + " is greater than or equal to " + past_end.String() + ".")
				shared.ReportErrorAndExit("VM", msg.String())
			}

			rng := &runtime.Range{
				Start:   start,
				PastEnd: past_end,
			}
			vm.PushOntoStack(rng)
			ip += 1
		case opcode.MakeInclusiveRange:
			end := vm.PopOffStack()
			start := vm.PopOffStack()

			if end.String() <= start.String() {
				var msg strings.Builder
				msg.WriteString(start.String() + " is greater than or equal to " + end.String() + ".")
				shared.ReportErrorAndExit("VM", msg.String())
			}

			irng := &runtime.InclusiveRange{
				Start: start,
				End:   end,
			}
			vm.PushOntoStack(irng)
			ip += 1
		case opcode.InitStruct:
			init_count := int(opcode.ReadUInt16(vm.instructions[ip+1:]))
			strct := &runtime.InitializedStruct{
				StructName: "",
				Members:    map[string]runtime.RuntimeObj{},
			}

			name := vm.PopOffStack().(*runtime.String)
			strct.StructName = name.String() // set name

			for {
				if init_count == 0 {
					break
				}

				val := vm.PopOffStack()
				mem := vm.PopOffStack().String()

				strct.Members[mem] = val
				init_count--
			}

			vm.PushOntoStack(strct)
			ip += 3
		case opcode.CallBuiltinFn:
			builtin_index := int(opcode.ReadUInt16(vm.instructions[ip+1:]))
			arg_count := int(opcode.ReadUInt16(vm.instructions[ip+3:]))
			builtin := builtins.Builtins[builtin_index]

			args := []runtime.RuntimeObj{}

			for {
				if arg_count == 0 {
					break
				}
				arg := vm.PopOffStack()
				args = append(args, arg)
				arg_count--
			}

			i := 0
			j := len(args) - 1

			for i < j {
				args[i], args[j] = args[j], args[i]
				i++
				j--
			}

			// print(builtin.Name, "(")
			// for index, a := range args {
			// 	print(a.String())
			// 	if index+1 < len(args) {
			// 		print(", ")
			// 	}
			// }
			// println(")")

			if len(args) == 1 {
				vm.PushOntoStack(builtin.Fn(args[0]))
			} else if len(args) == 0 {
				vm.PushOntoStack(builtin.Fn(nil))
			} else {
				vm.PushOntoStack(builtin.Fn(args[0], args[1:]...))
			}
			ip += 5
		case opcode.Halt:
			vm.PopOffStack()
			return
		default:
			if def, err := opcode.LookupInstructionDef(opcode.OpCode(op)); err != nil {
				shared.ReportErrorAndExit("VM", err.Error())
			} else {
				shared.ReportErrorAndExit("VM",
					fmt.Sprintf("OpCode %s found at Instruction index %d is not implemented in VM",
						def.Name, ip))
			}
		}
	}
}
