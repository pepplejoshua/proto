package compiler

import (
	"fmt"
	"proto/ast"
	"proto/opcode"
	"proto/runtime"
	"proto/shared"
	"strconv"
)

type ByteCode struct {
	Instructions opcode.VMInstructions
	Constants    []runtime.RuntimeObj
}

type Compiler struct {
	instructions opcode.VMInstructions
	constants    []runtime.RuntimeObj
	FoundError   bool
	symbolTable  *SymbolTable
}

func NewCompiler() *Compiler {
	return &Compiler{
		instructions: opcode.VMInstructions{},
		constants:    []runtime.RuntimeObj{},
		FoundError:   false,
		symbolTable:  NewSymbolTable(),
	}
}

func (c *Compiler) appendConstant(cons runtime.RuntimeObj) int {
	// first search for existing constant and return its index if it does
	for index, c := range c.constants {
		if c.String() == cons.String() {
			return index
		}
	}

	// else just add a new constant and return its index
	c.constants = append(c.constants, cons)
	return len(c.constants) - 1
}

func (c *Compiler) ByteCode() *ByteCode {
	return &ByteCode{
		Instructions: c.instructions,
		Constants:    c.constants,
	}
}

// generates bytecode with current opcode and operands and returns the starting
// position of the instruction so it can be used for later modification of the instruction
// if required
func (c *Compiler) generateBytecode(op opcode.OpCode, operands ...int) int {
	ins := opcode.MakeInstruction(op, operands...)
	ins_pos := c.addInstruction(ins)
	return ins_pos
}

// used to append an instruction to the array of compiler instructions and then returns
// the starting position of that instruction
func (c *Compiler) addInstruction(ins []byte) int {
	new_ins_pos := len(c.instructions)
	c.instructions = append(c.instructions, ins...)
	return new_ins_pos
}

func (c *Compiler) replaceInstruction(pos int, new_ins []byte) {
	// replace segment of instruction with new instruction
	for i := 0; i < len(new_ins); i++ {
		c.instructions[pos+i] = new_ins[i]
	}
}

func (c *Compiler) updateOperand(ins_pos int, operand int) {
	// get the original instruction
	op := opcode.OpCode(c.instructions[ins_pos])
	// make new instruction with new operand
	new_ins := opcode.MakeInstruction(op, operand)

	// replace instruction with update
	c.replaceInstruction(ins_pos, new_ins)
}

func (c *Compiler) enterScope() {
	if c.symbolTable.CurScopeDepth == 0 {
		c.symbolTable = NewSymbolTableFrom(c.symbolTable)
	} else {
		c.symbolTable.CurScopeDepth++
	}
}

func (c *Compiler) exitScope() {
	if c.symbolTable.CurScopeDepth > 1 {
		// removes all locals in scope
		num_of_locals := 0
		for _, sym := range c.symbolTable.store {
			if sym.ScopeDepth == c.symbolTable.CurScopeDepth {
				num_of_locals++
			}
		}

		if num_of_locals > 0 {
			c.generateBytecode(opcode.PopN, num_of_locals)
		}
		c.symbolTable.CurScopeDepth--
	} else if c.symbolTable.CurScopeDepth == 1 {
		c.symbolTable = c.symbolTable.EnclosingSymTable
	}
}

func (c *Compiler) CompileProgram(prog *ast.ProtoProgram) {
	for _, fn := range prog.FunctionDefs {
		c.symbolTable.Define(fn.Name.LiteralRepr())
	}

	for _, struct_ := range prog.Structs {
		c.symbolTable.Define(struct_.Name.LiteralRepr())
	}

	for _, node := range prog.Contents {
		c.Compile(node)
	}
	sym, _ := c.symbolTable.Resolve("main")
	c.generateBytecode(opcode.GetGlobal, sym.Index)
	c.generateBytecode(opcode.CallFn, 0)
}

func MakeInt64(val string) int64 {
	num, _ := strconv.ParseInt(val, 10, 64)
	return num
}

var BREAKS = []int{}
var CONTINUES = []int{}
var LOOP_START int = -1

func (c *Compiler) Compile(node ast.ProtoNode) {
	switch actual := node.(type) {
	case *ast.PromotedExpr:
		c.Compile(actual.Expr)
		c.generateBytecode(opcode.Pop)
	case *ast.I64:
		loc := c.appendConstant(&runtime.I64{Value: MakeInt64(actual.LiteralRepr())})
		c.generateBytecode(opcode.LoadConstant, loc)
	case *ast.Char:
		loc := c.appendConstant(&runtime.Char{
			Value: actual.Token.Literal,
		})
		c.generateBytecode(opcode.LoadConstant, loc)
	case *ast.String:
		loc := c.appendConstant(&runtime.String{
			Value: actual.LiteralRepr(),
		})
		c.generateBytecode(opcode.LoadConstant, loc)
	case *ast.Unit:
		c.generateBytecode(opcode.PushUnit)
	case *ast.Array:
		for _, item := range actual.Items {
			c.Compile(item)
		}
		c.generateBytecode(opcode.MakeArray, len(actual.Items))
	case *ast.Tuple:
		for _, item := range actual.Items {
			c.Compile(item)
		}
		c.generateBytecode(opcode.MakeTuple, len(actual.Items))
	case *ast.IndexExpression:
		c.Compile(actual.Indexable)
		c.Compile(actual.Index)
		c.generateBytecode(opcode.AccessIndex)
	case *ast.VariableDecl:
		c.CompileVariableDecl(actual)
	case *ast.CallExpression:
		c.CompileCallExprs(actual)
	case *ast.Identifier:
		c.CompileIdentifier(actual)
	case *ast.IfConditional:
		c.CompileIfConditional(actual)
	case *ast.Block:
		c.CompileBlock(actual, true)
	case *ast.Boolean:
		if actual.Value {
			c.generateBytecode(opcode.PushBoolTrue)
		} else {
			c.generateBytecode(opcode.PushBoolFalse)
		}
	case *ast.Break:
		BREAKS = append(BREAKS, c.generateBytecode(opcode.JumpTo, 9999))
	case *ast.Continue:
		CONTINUES = append(CONTINUES, c.generateBytecode(opcode.JumpTo, LOOP_START))
	case *ast.UnaryOp:
		c.CompileUnaryOp(actual)
	case *ast.BinaryOp:
		c.CompileBinaryOp(actual)
	case *ast.Assignment:
		c.CompileAssignment(actual)
	case *ast.FunctionDef:
		c.CompileFunctionDef(actual)
	case *ast.Return:
		c.CompileReturn(actual)
	case *ast.Membership:
		c.Compile(actual.Object)
		c.Compile(actual.Member)
		c.generateBytecode(opcode.AccessMember)
	case *ast.InfiniteLoop:
		c.CompileInfiniteLoop(actual)
	default:
	}
}

func (c *Compiler) CompileInfiniteLoop(loop *ast.InfiniteLoop) {
	prev_breaks := BREAKS
	prev_continues := CONTINUES
	prev_loop_start := LOOP_START
	LOOP_START = len(c.instructions)
	BREAKS = []int{}
	CONTINUES = []int{}
	c.enterScope()
	c.CompileBlock(loop.Body, false)
	c.exitScope()
	c.generateBytecode(opcode.JumpTo, LOOP_START)
	loop_end := len(c.instructions)
	for _, _break := range BREAKS {
		c.updateOperand(_break, loop_end)
	}
	BREAKS = prev_breaks
	CONTINUES = prev_continues
	LOOP_START = prev_loop_start
}

func (c *Compiler) CompileAssignment(assign *ast.Assignment) {
	target := assign.Target
	assigned := assign.Assigned
	switch assign.AssignmentToken.Literal {
	case "=":
		switch lhs := target.(type) {
		case *ast.Membership:
		case *ast.IndexExpression:
			c.Compile(lhs.Indexable)
			c.Compile(lhs.Index)
			c.Compile(assigned)
			c.generateBytecode(opcode.UpdateIndex)
		case *ast.Identifier:
			c.Compile(assigned)
			sym, ok := c.symbolTable.Resolve(target.LiteralRepr())
			if !ok {
				shared.ReportErrorAndExit("Compiler", fmt.Sprintf("Undefined name %s",
					target.LiteralRepr()))
			}
			if sym.ScopeDepth == 0 {
				c.generateBytecode(opcode.SetGlobal, sym.Index)
			} else {
				c.generateBytecode(opcode.SetLocal, sym.Index)
			}
		}
	case "+=":
		switch target.(type) {
		case *ast.Membership:
		case *ast.IndexExpression:
		case *ast.Identifier:
			switch target.Type().TypeSignature() {
			case "str":
				c.Compile(target)
				c.Compile(assigned)
				switch assigned.Type().TypeSignature() {
				case "char":
					c.generateBytecode(opcode.AddStrChar)
				case "str":
					c.generateBytecode(opcode.AddStr)
				}
				sym, ok := c.symbolTable.Resolve(target.LiteralRepr())
				if !ok {
					shared.ReportErrorAndExit("Compiler", fmt.Sprintf("Undefined name %s",
						target.LiteralRepr()))
				}
				if sym.ScopeDepth == 0 {
					c.generateBytecode(opcode.SetGlobal, sym.Index)
				} else {
					c.generateBytecode(opcode.SetLocal, sym.Index)
				}
			case "i64":
				c.Compile(target)
				c.Compile(assigned)
				c.generateBytecode(opcode.AddI64)
				sym, ok := c.symbolTable.Resolve(target.LiteralRepr())
				if !ok {
					shared.ReportErrorAndExit("Compiler", fmt.Sprintf("Undefined name %s",
						target.LiteralRepr()))
				}
				if sym.ScopeDepth == 0 {
					c.generateBytecode(opcode.SetGlobal, sym.Index)
				} else {
					c.generateBytecode(opcode.SetLocal, sym.Index)
				}
			}
		}
	case "-=", "*=", "%=", "/=":
		switch target.(type) {
		case *ast.Membership:
		case *ast.IndexExpression:
		case *ast.Identifier:
			c.Compile(target)
			c.Compile(assigned)
			ins := map[string]opcode.OpCode{
				"-=": opcode.SubI64,
				"*=": opcode.MultI64,
				"%=": opcode.ModuloI64,
				"/=": opcode.DivI64,
			}
			op := ins[assign.AssignmentToken.Literal]
			c.generateBytecode(op)
			sym, ok := c.symbolTable.Resolve(target.LiteralRepr())
			if !ok {
				shared.ReportErrorAndExit("Compiler", fmt.Sprintf("Undefined name %s",
					target.LiteralRepr()))
			}
			if sym.ScopeDepth == 0 {
				c.generateBytecode(opcode.SetGlobal, sym.Index)
			} else {
				c.generateBytecode(opcode.SetLocal, sym.Index)
			}
		}
	}
}

func (c *Compiler) CompileVariableDecl(actual *ast.VariableDecl) {
	c.Compile(actual.Assigned)
	sym, exists := c.symbolTable.Define(actual.Assignee.Token.Literal)
	if c.symbolTable.CurScopeDepth > 0 {
		if exists && sym.ScopeDepth == c.symbolTable.CurScopeDepth {
			c.generateBytecode(opcode.SetLocal, sym.Index)
		}
		return
	}
	c.generateBytecode(opcode.SetGlobal, sym.Index)
}

func (c *Compiler) CompileIdentifier(actual *ast.Identifier) {
	sym, ok := c.symbolTable.Resolve(actual.Token.Literal)
	if !ok {
		shared.ReportErrorAndExit("Compiler", fmt.Sprintf("Undefined name %s",
			actual.Token.Literal))
	}

	if sym.ScopeDepth == 0 {
		c.generateBytecode(opcode.GetGlobal, sym.Index)
	} else {
		c.generateBytecode(opcode.GetLocal, sym.Index)
	}
}

func (c *Compiler) CompileReturn(ret *ast.Return) {
	if ret.Value != nil {
		c.Compile(ret.Value)
	} else {
		c.generateBytecode(opcode.PushUnit)
	}
	c.generateBytecode(opcode.Return)
}

func (c *Compiler) CompileCallExprs(call *ast.CallExpression) {
	for _, arg := range call.Arguments {
		c.Compile(arg)
	}
	c.Compile(call.Callable)
	c.generateBytecode(opcode.CallFn, len(call.Arguments))
}

func (c *Compiler) CompileFunctionDef(fn *ast.FunctionDef) {
	start := c.generateBytecode(opcode.JumpTo, 9999)
	sym, _ := c.symbolTable.Define(fn.Name.LiteralRepr())
	fn_ip := len(c.instructions)
	if len(fn.Body.Contents) > 0 {
		new_comp := &Compiler{
			instructions: c.instructions,
			constants:    c.constants,
			FoundError:   false,
			symbolTable:  NewSymbolTableFrom(c.symbolTable),
		}
		new_comp.enterScope()
		for _, param := range fn.ParameterList {
			new_comp.symbolTable.Define(param.LiteralRepr())
		}
		new_comp.CompileBlock(fn.Body, false)
		new_comp.exitScope()
		if _, ok := fn.Body.Contents[len(fn.Body.Contents)-1].(*ast.Return); !ok {
			new_comp.generateBytecode(opcode.Return)
		}
		c.constants = new_comp.constants
		c.instructions = new_comp.instructions
	} else {
		c.generateBytecode(opcode.PushUnit)
		c.generateBytecode(opcode.Return)
	}

	if fn.Name.LiteralRepr() == "main" {
		c.generateBytecode(opcode.Halt)
	}

	fn_def := len(c.instructions)
	arity := len(fn.ParameterList)
	c.generateBytecode(opcode.MakeFn, arity, fn_ip, sym.Index)
	c.updateOperand(start, fn_def)
}

func (c *Compiler) CompileBlock(blk *ast.Block, makeScope bool) {
	if makeScope {
		c.enterScope()
	}

	for index, node := range blk.Contents {
		c.Compile(node)

		if index+1 == len(blk.Contents) {
			switch node.(type) {
			case ast.Expression:
			case *ast.Return:
			case *ast.Continue, *ast.Break:
			default:
				c.generateBytecode(opcode.PushUnit)
			}
		}
	}

	if makeScope {
		c.exitScope()
	}
}

func (c *Compiler) CompileIfConditional(actual *ast.IfConditional) {
	c.Compile(actual.Condition)
	// jump instruction with an operand to be updated later
	jump_not_true := c.generateBytecode(opcode.JumpOnNotTrueTo, 9999)

	// compile the then body of if conditional
	c.CompileBlock(actual.ThenBody, true)

	// store the jump instruction to be updated later
	// it allows the then body jump past the rest of the else statement
	jump_to := c.generateBytecode(opcode.JumpTo, 9999)
	// the jump to else target is the instruction after the then body of the if conditional
	// update that with the right location
	jump_not_true_target := len(c.instructions)

	c.updateOperand(jump_not_true, jump_not_true_target)

	// if there is no else body, then push a unit onto stack
	if actual.ElseBody == nil {
		c.enterScope()
		c.generateBytecode(opcode.PushUnit)
		c.exitScope()
	} else {
		c.Compile(actual.ElseBody)
	}

	// set jump to target for then body to be past the end of the if statement
	jump_to_target := len(c.instructions)
	c.updateOperand(jump_to, jump_to_target)
}

func (c *Compiler) CompileUnaryOp(actual *ast.UnaryOp) {
	c.Compile(actual.Operand)
	switch actual.Operator.Literal {
	case "-":
		c.generateBytecode(opcode.NegateI64)
	case "not":
		c.generateBytecode(opcode.NegateBool)
	}
}

func (c *Compiler) CompileBinaryOp(actual *ast.BinaryOp) {
	switch actual.Operator.Literal {
	case "+":
		c.Compile(actual.Left)
		c.Compile(actual.Right)
		switch actual.Op_Type.TypeSignature() {
		case "i64":
			c.generateBytecode(opcode.AddI64)
		case "str":
			if actual.Left.Type().TypeSignature() == "char" {
				c.generateBytecode(opcode.AddChar)
			} else if actual.Left.Type().TypeSignature() == "str" &&
				actual.Right.Type().TypeSignature() == "char" {
				c.generateBytecode(opcode.AddStrChar)
			} else {
				c.generateBytecode(opcode.AddStr)
			}
		}
	case "-":
		c.Compile(actual.Left)
		c.Compile(actual.Right)
		c.generateBytecode(opcode.SubI64)
	case "*":
		c.Compile(actual.Left)
		c.Compile(actual.Right)
		c.generateBytecode(opcode.MultI64)
	case "/":
		c.Compile(actual.Left)
		c.Compile(actual.Right)
		c.generateBytecode(opcode.DivI64)
	case "%":
		c.Compile(actual.Left)
		c.Compile(actual.Right)
		c.generateBytecode(opcode.ModuloI64)
	case "==":
		c.Compile(actual.Left)
		c.Compile(actual.Right)
		c.generateBytecode(opcode.EqualsComp)
	case "!=":
		c.Compile(actual.Left)
		c.Compile(actual.Right)
		c.generateBytecode(opcode.NotEqualsComp)
	case ">":
		c.Compile(actual.Left)
		c.Compile(actual.Right)
		c.generateBytecode(opcode.GreaterThanComp)
	case "<":
		c.Compile(actual.Right) // by reversing the orders of operands, reusing GreaterThan is possible
		c.Compile(actual.Left)
		c.generateBytecode(opcode.GreaterThanComp)
	case ">=":
		c.Compile(actual.Left)
		c.Compile(actual.Right)
		c.generateBytecode(opcode.GreaterEqualsComp)
	case "<=":
		c.Compile(actual.Right) // by reversing the orders of operands, reusing GreaterThanEquals is possible
		c.Compile(actual.Left)
		c.generateBytecode(opcode.GreaterEqualsComp)
	case "&&":
		c.Compile(actual.Left)
		c.Compile(actual.Right)
		c.generateBytecode(opcode.And)
	case "||":
		c.Compile(actual.Left)
		c.Compile(actual.Right)
		c.generateBytecode(opcode.Or)
	}
}
