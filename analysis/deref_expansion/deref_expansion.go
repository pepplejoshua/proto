package derefexpansion

import "proto/ast"

type DefefExpander struct{}

func (de *DefefExpander) ExpandProgram(prog *ast.ProtoProgram) {
	for index, node := range prog.Contents {
		prog.Contents[index] = de.Expand(node)
	}
}

func (de *DefefExpander) Expand(node ast.ProtoNode) ast.ProtoNode {
	switch actual := node.(type) {
	case *ast.Identifier, *ast.I64, *ast.String, *ast.Char, *ast.Boolean, *ast.Unit,
		*ast.Continue, *ast.Break, *ast.Return:
	case *ast.Block:
		node = de.ExpandBlock(actual)
	case *ast.FunctionDef:
		node = de.ExpandFunctionDef(actual)
	case *ast.VariableDecl:
		node = de.Expand(actual.Assigned)
	case *ast.WhileLoop:
		node = de.ExpandWhileLoop(actual)
	case *ast.GenericForLoop:
		node = de.ExpandGenericFor(actual)
	case *ast.CollectionsForLoop:
		node = de.ExpandCollectionsFor(actual)
	case *ast.InfiniteLoop:
		node = de.ExpandInfiniteLoop(actual)
	case *ast.IfConditional:
		node = de.ExpandIfExpr(actual)
	}

	return node
}

func (de *DefefExpander) ExpandIfExpr(if_ *ast.IfConditional) *ast.IfConditional {
	for index, node := range if_.ThenBody.Contents {
		if_.ThenBody.Contents[index] = de.Expand(node)
	}

	if if_.ElseBody != nil {
		if else_if, ok := if_.ElseBody.(*ast.IfConditional); ok {
			if_.ElseBody = de.ExpandIfExpr(else_if)
		} else if blk, ok := if_.ElseBody.(*ast.Block); ok {
			if_.ElseBody = de.ExpandBlock(blk)
		}
	}
	return if_
}

func (de *DefefExpander) ExpandWhileLoop(while *ast.WhileLoop) *ast.WhileLoop {
	for index, node := range while.Body.Contents {
		while.Body.Contents[index] = de.Expand(node)
	}

	return while
}

func (de *DefefExpander) ExpandGenericFor(loop *ast.GenericForLoop) *ast.GenericForLoop {
	for index, node := range loop.Body.Contents {
		loop.Body.Contents[index] = de.Expand(node)
	}

	return loop
}

func (de *DefefExpander) ExpandCollectionsFor(loop *ast.CollectionsForLoop) *ast.CollectionsForLoop {
	for index, node := range loop.Body.Contents {
		loop.Body.Contents[index] = de.Expand(node)
	}

	return loop
}

func (de DefefExpander) ExpandInfiniteLoop(loop *ast.InfiniteLoop) *ast.InfiniteLoop {
	for index, node := range loop.Body.Contents {
		loop.Body.Contents[index] = de.Expand(node)
	}

	return loop
}

func (de *DefefExpander) ExpandFunctionDef(fn *ast.FunctionDef) *ast.FunctionDef {
	for index, node := range fn.Body.Contents {
		fn.Body.Contents[index] = de.Expand(node)
	}

	return fn
}

func (de *DefefExpander) ExpandBlock(blk *ast.Block) *ast.Block {
	for index, node := range blk.Contents {
		blk.Contents[index] = de.Expand(node)
	}
	return blk
}
