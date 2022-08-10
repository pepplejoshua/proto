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

// let n = [1, 2, 3]; // n's original is itself
// let m = n; // m's original is now n, but it should be m
// GetOriginal implements runtime.RuntimeObj
func (cf *CompiledFunction) GetOriginal() runtime.RuntimeObj {
	return cf
}

func (cf *CompiledFunction) String() string {
	return fmt.Sprintf("fn at Instruction #%d", cf.LocIP)
}

func (cf *CompiledFunction) Copy() runtime.RuntimeObj {
	return cf
}

func (cf *CompiledFunction) Type() string {
	return cf.String()
}

type CallFrame struct {
	called_fn            *CompiledFunction
	original_stack_index int
	stack_index          int
	return_ip            int
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

func (vm *VM) ExtractValue(potential_ref runtime.RuntimeObj) runtime.RuntimeObj {
	var val runtime.RuntimeObj
	switch actual := potential_ref.(type) {
	case *runtime.Ref:
		val = actual.Value
	default:
		val = actual
	}
	return val
}

func (vm *VM) UpdateRefValue(ref *runtime.Ref, assigned runtime.RuntimeObj) {
	// println(ref.String())
	switch val := ref.Value.(type) {
	case *runtime.I64:
		val.Value = assigned.(*runtime.I64).Value
	case *runtime.Bool:
		val.Value = assigned.(*runtime.Bool).Value
	case *runtime.Char:
		val.Value = assigned.(*runtime.Char).Value
	case *runtime.String:
		val.Value = assigned.String()
	}
}

func (vm *VM) UpdateRegularValue(reg runtime.RuntimeObj, assigned runtime.RuntimeObj) {
	switch val := reg.(type) {
	case *runtime.I64:
		val.Value = assigned.(*runtime.I64).Value
	case *runtime.Bool:
		val.Value = assigned.(*runtime.Bool).Value
	case *runtime.Char:
		val.Value = assigned.(*runtime.Char).Value
	case *runtime.String:
		val.Value = assigned.String()
	case *runtime.InitializedStruct:
		val.Members = assigned.(*runtime.InitializedStruct).Members
		val.Original = assigned.(*runtime.InitializedStruct).Original
	}
}

func (vm *VM) Show_stack(start int) {
	println("at instruction:", start)
	for index, val := range vm.stack {
		if index >= vm.stack_index {
			break
		}
		println(index+1, val.String())
	}
	println()
}

func (vm *VM) Run() {
	// println(vm.instructions.Disassemble())
	for ip := 0; ip < len(vm.instructions); {
		op := opcode.OpCode(vm.instructions[ip])
		// start := ip

		switch op {
		case opcode.LoadConstant:
			cons_index := opcode.ReadUInt16(vm.instructions[ip+1:])
			// println("Loading", vm.constants[cons_index].String())
			vm.PushOntoStack(vm.constants[cons_index].Copy())
			ip += 3
		case opcode.PushBoolTrue:
			vm.PushOntoStack(TRUE.Copy())
			ip += 1
		case opcode.PushBoolFalse:
			vm.PushOntoStack(FALSE.Copy())
			ip += 1
		case opcode.AddI64, opcode.SubI64, opcode.MultI64:
			rhs := vm.ExtractValue(vm.PopOffStack()).(*runtime.I64)
			lhs := vm.ExtractValue(vm.PopOffStack()).(*runtime.I64)
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
			rhs := vm.ExtractValue(vm.PopOffStack()).(*runtime.Char)
			lhs := vm.ExtractValue(vm.PopOffStack()).(*runtime.Char)
			n_val := "\"" + lhs.Character() + rhs.Character() + "\""
			val := &runtime.String{
				Value: n_val,
			}
			vm.PushOntoStack(val)
			ip += 1
		case opcode.AddStr:
			rhs := vm.ExtractValue(vm.PopOffStack()).(*runtime.String)
			lhs := vm.ExtractValue(vm.PopOffStack()).(*runtime.String)
			n_val := "\"" + lhs.Content() +
				rhs.Content() + "\""
			val := &runtime.String{
				Value: n_val,
			}
			vm.PushOntoStack(val)
			ip += 1
		case opcode.AddStrChar:
			rhs := vm.ExtractValue(vm.PopOffStack()).(*runtime.Char)
			lhs := vm.ExtractValue(vm.PopOffStack()).(*runtime.String)
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
			rhs := vm.ExtractValue(vm.PopOffStack()).(*runtime.I64)
			lhs := vm.ExtractValue(vm.PopOffStack()).(*runtime.I64)
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
			op := vm.ExtractValue(vm.PopOffStack()).(*runtime.I64)
			n_val := -op.Value
			val := &runtime.I64{
				Value: n_val,
			}
			vm.PushOntoStack(val)
			ip += 1
		case opcode.NegateBool:
			op := vm.ExtractValue(vm.PopOffStack()).(*runtime.Bool)
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
			rhs := vm.ExtractValue(vm.PopOffStack())
			lhs := vm.ExtractValue(vm.PopOffStack())

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
			rhs := vm.ExtractValue(vm.PopOffStack()).(*runtime.Bool)
			lhs := vm.ExtractValue(vm.PopOffStack()).(*runtime.Bool)

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
			val := vm.ExtractValue(vm.PopOffStack()).(*runtime.Bool)
			if !val.Value {
				new_ip := opcode.ReadUInt16(vm.instructions[ip+1:])
				ip = int(new_ip)
			} else {
				ip += 3
			}
		case opcode.JumpTo:
			new_ip := opcode.ReadUInt16(vm.instructions[ip+1:])
			ip = int(new_ip)
		case opcode.PushUnit:
			vm.PushOntoStack(UNIT)
			ip += 1
		case opcode.SetGlobal:
			globalIndex := int(opcode.ReadUInt16(vm.instructions[ip+1:]))
			assigned := vm.PopOffStack()
			loc := vm.globals[globalIndex]

			if ref, ok := loc.(*runtime.Ref); ok {
				if assigned_ref, ok := assigned.(*runtime.Ref); ok {
					ref.Value = assigned_ref.Value
					// vm.globals[globalIndex] = assigned.(*runtime.Ref)
				} else {
					vm.UpdateRefValue(ref, assigned)
				}
			} else {
				// println("setting global", globalIndex, "to", assigned, assigned.String())
				if loc != nil {
					if loc.Type() == assigned.Type() {
						vm.UpdateRegularValue(loc, assigned)
					} else {
						vm.globals[globalIndex] = assigned
					}
				} else {
					vm.globals[globalIndex] = assigned
				}
				// ref.Value = assigned
			}
			ip += 3
		case opcode.GetGlobal:
			globalIndex := int(opcode.ReadUInt16(vm.instructions[ip+1:]))
			val := vm.globals[globalIndex].Copy()
			// println("Getting global", globalIndex, "which is", val, val.String())
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

			// println("making array with length", len(array.Items))
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
			index_t := vm.ExtractValue(vm.PopOffStack()).(*runtime.I64)
			indexable := vm.ExtractValue(vm.PopOffStack())
			switch indexable := indexable.(type) {
			case *runtime.Array:
				// println(len(indexable.Items), "with index", index_t.Value)
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
			case *runtime.String:
				if index_t.Value < 0 || int(index_t.Value) >= len(indexable.Content()) {
					if len(indexable.Content()) == 0 {
						shared.ReportErrorAndExit("VM", fmt.Sprintf("Provided index %d is out of range, as Array has %d items (and is not indexable).",
							index_t.Value, len(indexable.Content())))
					} else if len(indexable.Content()) == 1 {
						shared.ReportErrorAndExit("VM", fmt.Sprintf("Provided index %d is out of range, as Array has %d items (indexable only by 0).",
							index_t.Value, len(indexable.Content())))
					} else {
						shared.ReportErrorAndExit("VM", fmt.Sprintf("Provided index %d is out of range, as Array has %d items (indexable from 0 to %d)",
							index_t.Value, len(indexable.Content()), len(indexable.Content())-1))
					}
				}
				character := string(indexable.Content()[index_t.Value])
				vm.PushOntoStack(&runtime.Char{
					Value: character,
				})
			}

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
				local := vm.stack[offset].Copy()
				// println("getting local", local, local.String())
				vm.PushOntoStack(local)
			} else {
				local := vm.stack[vm.frames[vm.frame_index-1].stack_index+offset].Copy()
				// println("getting local", local, local.String())
				vm.PushOntoStack(local)
			}
			ip += 3
		case opcode.SetLocal:
			offset := int(opcode.ReadUInt16(vm.instructions[ip+1:]))
			assigned := vm.PopOffStack()
			if vm.frame_index == 0 {
				// fix this for { } scopes in global scope
				vm.stack[offset] = assigned
			} else {
				loc := vm.stack[vm.frames[vm.frame_index-1].stack_index+offset]

				// check if loc is a ref, if it is, then update the value it holds
				if ref, ok := loc.(*runtime.Ref); ok {
					if assigned_ref, ok := assigned.(*runtime.Ref); ok {
						ref.Value = assigned_ref.Value
						// vm.stack[vm.frames[vm.frame_index-1].stack_index+offset] = ref
					} else {
						vm.UpdateRefValue(ref, assigned)
					}
				} else {
					if loc.Type() == assigned.Type() {
						vm.UpdateRegularValue(loc, assigned)
					} else {
						vm.stack[vm.frames[vm.frame_index-1].stack_index+offset] = assigned
					}
				}
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
			res := vm.stack[vm.stack_index-1]
			if vm.frame_index-1 >= 0 && vm.frames[vm.frame_index-1] != nil {
				vm.stack_index = vm.frames[vm.frame_index].original_stack_index
				vm.PushOntoStack(res)
			}

			ip = ret_loc
		case opcode.CallFn:
			arg_count := opcode.ReadUInt16(vm.instructions[ip+1:])
			fn := vm.PopOffStack().(*CompiledFunction)

			// num := arg_count
			// args := []runtime.RuntimeObj{}
			// for {
			// 	if num == 0 {
			// 		break
			// 	}
			// 	args = append(args, vm.PopOffStack().Copy())
			// 	num--
			// }

			// for i := len(args) - 1; i >= 0; i-- {
			// 	vm.PushOntoStack(args[i].Copy())
			// }

			new_frame := &CallFrame{
				called_fn:            fn,
				original_stack_index: vm.stack_index - int(arg_count),
				stack_index:          vm.stack_index - int(arg_count),
				return_ip:            ip + 3,
			}
			vm.frames[vm.frame_index] = new_frame
			vm.frame_index++
			ip = new_frame.called_fn.LocIP
		case opcode.UpdateIndex:
			assigned := vm.PopOffStack()
			index := vm.ExtractValue(vm.PopOffStack()).(*runtime.I64)
			array := vm.ExtractValue(vm.PopOffStack()).(*runtime.Array)

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

			loc := array.Items[index.Value]
			if ref, ok := loc.(*runtime.Ref); ok {
				if a_ref, ok := assigned.(*runtime.Ref); ok {
					ref.Value = a_ref.Value
				} else {
					vm.UpdateRefValue(ref, assigned)
				}
			} else {
				vm.UpdateRegularValue(loc.GetOriginal(), assigned)
			}
			// array.Items[index.Value] = assigned
			ip += 1
		case opcode.UpdateStructMember:
			assigned := vm.PopOffStack()
			mem := vm.PopOffStack().(*runtime.String)
			strct := vm.ExtractValue(vm.PopOffStack()).(*runtime.InitializedStruct)

			mem_obj := strct.Members[mem.Value]
			if ref, ok := mem_obj.(*runtime.Ref); ok {
				if a_ref, ok := assigned.(*runtime.Ref); ok {
					ref.Value = a_ref.Value
				} else {
					vm.UpdateRefValue(ref, assigned)
				}
			} else {
				vm.UpdateRegularValue(strct.GetOriginal().(*runtime.InitializedStruct).Members[mem.Value], assigned)
			}
			ip += 1
		case opcode.AccessTupleMember:
			index := vm.PopOffStack().(*runtime.I64)
			obj := vm.ExtractValue(vm.PopOffStack()).(*runtime.Tuple)

			val := obj.Items[index.Value]
			vm.PushOntoStack(val)
			ip += 1
		case opcode.AccessStructMember:
			mem := vm.PopOffStack().(*runtime.String)
			obj := vm.ExtractValue(vm.PopOffStack()).(*runtime.InitializedStruct)

			val := obj.Members[mem.Value]
			vm.PushOntoStack(val)
			ip += 1
		case opcode.MakeRange:
			past_end := vm.ExtractValue(vm.PopOffStack())
			start := vm.ExtractValue(vm.PopOffStack())

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
			end := vm.ExtractValue(vm.PopOffStack())
			start := vm.ExtractValue(vm.PopOffStack())

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
				vm.PushOntoStack(builtin.Fn())
			} else {
				vm.PushOntoStack(builtin.Fn(args...))
			}
			ip += 5
		case opcode.MakeRef:
			val := vm.PopOffStack()
			// println("referencing", val, val.String())
			ref := &runtime.Ref{
				Value: val.GetOriginal(),
			}

			vm.PushOntoStack(ref)
			ip += 1
		case opcode.Deref:
			ref := vm.PopOffStack().(*runtime.Ref)
			// println("dereferencing to", ref.Value, ref.Value.String())
			vm.PushOntoStack(ref.Value.Copy())
			ip += 1
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
		// vm.Show_stack(start)
	}
}
