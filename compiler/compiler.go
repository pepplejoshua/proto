package compiler

import (
	"fmt"
	"proto/ast"
	"proto/opcode"
	"proto/shared"
	"strings"
)

type ByteCode struct {
	Instructions opcode.VMInstructions
	Constants    []ast.ProtoNode
}

type Compiler struct {
	instructions opcode.VMInstructions
	constants    []ast.ProtoNode
	FoundError   bool
	symbolTable  *SymbolTable
}

var MAIN_START = 0

func NewCompiler() *Compiler {
	return &Compiler{
		instructions: opcode.VMInstructions{},
		constants:    []ast.ProtoNode{},
		FoundError:   false,
		symbolTable:  NewSymbolTable(),
	}
}

func (c *Compiler) appendConstant(cons ast.ProtoNode) int {
	// first search for existing constant and return its index if it does
	for index, c := range c.constants {
		if c.LiteralRepr() == cons.LiteralRepr() {
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
	// c.symbolTable.CurScopeDepth++
	if c.symbolTable.CurScopeDepth == 0 {
		c.symbolTable = NewSymbolTableFrom(c.symbolTable)
	} else {
		c.symbolTable.CurScopeDepth++
	}
	// c.generateBytecode(opcode.EnterScope)
}

func (c *Compiler) exitScope() {
	if c.symbolTable.CurScopeDepth > 0 {
		// removes all locals in scope
		num_of_locals := 0
		for _, sym := range c.symbolTable.store {
			if sym.ScopeDepth == c.symbolTable.CurScopeDepth {
				num_of_locals++
			}
		}

		if num_of_locals == 1 {
			c.generateBytecode(opcode.Pop)
		}
		if num_of_locals > 1 {
			c.generateBytecode(opcode.PopN, num_of_locals)
		}
		c.symbolTable.CurScopeDepth--
	}
}

func (c *Compiler) CompileProgram(prog *ast.ProtoProgram) {
	for _, node := range prog.Contents {
		c.Compile(node)
	}
	c.generateBytecode(opcode.JumpTo, MAIN_START)
}

func (c *Compiler) Compile(node ast.ProtoNode) {
	switch actual := node.(type) {
	case *ast.PromotedExpr:
		c.Compile(actual.Expr)
		c.generateBytecode(opcode.Pop)
	case *ast.I64, *ast.Char, *ast.String:
		loc := c.appendConstant(actual)
		c.generateBytecode(opcode.LoadConstant, loc)
	case *ast.Array:
		for _, item := range actual.Items {
			c.Compile(item)
		}
		c.generateBytecode(opcode.MakeArray, len(actual.Items))
	case *ast.IndexExpression:
		c.Compile(actual.Indexable)
		c.Compile(actual.Index)
		c.generateBytecode(opcode.AccessIndex)
	case *ast.VariableDecl:
		c.CompileVariableDecl(actual)
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
	default:
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

func (c *Compiler) CompileFunctionDef(fn *ast.FunctionDef) {
	start := c.generateBytecode(opcode.JumpTo, 9999)
	sym, exists := c.symbolTable.Define(fn.Name.LiteralRepr())
	if exists {
		line := fn.Name.Token.TokenSpan.Line
		col := fn.Name.Token.TokenSpan.Col
		var msg strings.Builder
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("The name %s already exists.", sym.Name))
		shared.ReportErrorAndExit("Compiler", msg.String())
	}

	fn_comp := &Compiler{
		instructions: []byte{},
		constants:    c.constants,
		FoundError:   false,
		symbolTable: &SymbolTable{
			store:              []*Symbol{},
			numOfStoredSymbols: 0,
			CurScopeDepth:      0,
			EnclosingSymTable:  c.symbolTable,
		},
	}

	fn_ip := len(c.instructions)
	if len(fn.Body.Contents) > 0 {
		fn_comp.enterScope()
		fn_comp.CompileBlock(fn.Body, false)
		c.instructions = append(c.instructions, fn_comp.instructions...)
		if _, ok := fn.Body.Contents[len(fn.Body.Contents)-1].(ast.Expression); ok {
			c.generateBytecode(opcode.Return)
		}
		c.constants = fn_comp.constants
		fn_comp.exitScope()
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
	if fn.Name.LiteralRepr() == "main" {
		MAIN_START = fn_ip
	}
}

func (c *Compiler) CompileAssignment(assign *ast.Assignment) {
	target := assign.Target
	assigned := assign.Assigned
	switch assign.AssignmentToken.Literal {
	case "=":
		c.Compile(assigned)
		switch target.(type) {
		case *ast.Membership:
		case *ast.IndexExpression:
		case *ast.Identifier:
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
		// since local variables use stack pointer offsets, we don't generate
		// any special code for them
		if exists {
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
	// } else {
	// 	c.generateBytecode(opcode.GetOuterLocal,
	// 		c.symbolTable.CurScopeDepth-sym.ScopeDepth, sym.Index)
	// }
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
	c.Compile(actual.ThenBody)

	// store the jump instruction to be updated later
	// it allows the then body jump past the rest of the else statement
	jump_to := c.generateBytecode(opcode.JumpTo, 9999)

	// the jump to else target is the instruction after the then body of the if conditional
	// update that with the right location
	jump_not_true_target := len(c.instructions)
	c.updateOperand(jump_not_true, jump_not_true_target)

	// if there is no else body, then push a unit onto stack
	if actual.ElseBody == nil {
		c.generateBytecode(opcode.PushUnit)
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
