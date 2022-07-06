package compiler

import (
	"proto/ast"
	"proto/opcode"
)

type ByteCode struct {
	Instructions opcode.VMInstructions
	Constants    []ast.Expression
}

type Compiler struct {
	instructions opcode.VMInstructions
	constants    []ast.Expression
	FoundError   bool
}

func NewCompiler() *Compiler {
	return &Compiler{
		instructions: opcode.VMInstructions{},
		constants:    []ast.Expression{},
	}
}

func (c *Compiler) CompileProgram(prog *ast.ProtoProgram) {
	for _, node := range prog.Contents {
		c.Compile(node)
	}
}

func (c *Compiler) Compile(node ast.ProtoNode) {
}

func (c *Compiler) ByteCode() *ByteCode {
	return &ByteCode{
		Instructions: c.instructions,
		Constants:    c.constants,
	}
}
