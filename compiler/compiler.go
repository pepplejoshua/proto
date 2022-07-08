package compiler

import (
	"proto/ast"
	"proto/opcode"
)

type ByteCode struct {
	Instructions opcode.VMInstructions
	Constants    []ast.ProtoNode
}

type Compiler struct {
	instructions opcode.VMInstructions
	constants    []ast.ProtoNode
	FoundError   bool
}

func NewCompiler() *Compiler {
	return &Compiler{
		instructions: opcode.VMInstructions{},
		constants:    []ast.ProtoNode{},
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

func (c *Compiler) CompileProgram(prog *ast.ProtoProgram) {
	for _, node := range prog.Contents {
		c.Compile(node)
	}
}

func (c *Compiler) Compile(node ast.ProtoNode) {
	switch actual := node.(type) {
	case *ast.PromotedExpr:
		c.Compile(actual.Expr)
		c.generateBytecode(opcode.Pop)
	case *ast.I64, *ast.Char, *ast.String:
		loc := c.appendConstant(actual)
		c.generateBytecode(opcode.LoadConstant, loc)
	case *ast.Boolean:
		if actual.Value {
			c.generateBytecode(opcode.PushBoolTrue)
		} else {
			c.generateBytecode(opcode.PushBoolFalse)
		}
	case *ast.UnaryOp:
		c.Compile(actual.Operand)
		switch actual.Operator.Literal {
		case "-":
			c.generateBytecode(opcode.NegateI64)
		case "not":
			c.generateBytecode(opcode.NegateBool)
		}
	case *ast.BinaryOp:
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
		}
	default:
	}
}

func (c *Compiler) ByteCode() *ByteCode {
	return &ByteCode{
		Instructions: c.instructions,
		Constants:    c.constants,
	}
}
