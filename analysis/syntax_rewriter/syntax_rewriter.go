package syntaxrewriter

import (
	"fmt"
	"proto/ast"
	"proto/lexer"
	"proto/parser"
	"proto/shared"
)

// if the provided collection is an identifier, then rewrite it
// to a generic for loop based on the type of the identifier
// if id is:
// string: start at 0 till len(string) - 1
// array: start at 0 till len(array) - 1
// range: start at start till end - 1
// irange: start at start till end
// the same ways will work on regular

type CollectionsForLoopRewriter struct{}

func (flr *CollectionsForLoopRewriter) RewriteProgram(prog *ast.ProtoProgram) {
	for index, node := range prog.Contents {
		prog.Contents[index] = flr.Rewrite(node)
	}
}

func (flr *CollectionsForLoopRewriter) Rewrite(node ast.ProtoNode) ast.ProtoNode {
	switch actual := node.(type) {
	case *ast.VariableDecl, *ast.Identifier, *ast.Assignment, *ast.PromotedExpr,
		*ast.Tuple, *ast.Array, *ast.CallExpression, *ast.BinaryOp, *ast.UnaryOp,
		*ast.Return, *ast.InclusiveRange, *ast.Range, *ast.IndexExpression, *ast.Struct,
		*ast.StructInitialization, *ast.Break, *ast.Continue, *ast.Membership, *ast.I64,
		*ast.String, *ast.Char, *ast.Boolean, *ast.Unit, *ast.Dereference, *ast.Reference:
	case *ast.BlockExpr:
		node = flr.RewriteBlockExpr(actual)
	case *ast.BlockStmt:
		node = flr.RewriteBlockStmt(actual)
	case *ast.FunctionDef:
		node = flr.RewriteFunctionDef(actual)
	case *ast.IfExpr:
		node = flr.RewriteIfExpr(actual)
	case *ast.IfStmt:
		node = flr.RewriteIfStmt(actual)
	case *ast.WhileLoop:
		node = flr.RewriteWhileLoop(actual)
	case *ast.GenericForLoop:
		node = flr.RewriteGenericForLoop(actual)
	case *ast.CollectionsForLoop:
		node = flr.RewriteCollectionsForLoop(actual)
	case *ast.InfiniteLoop:
		node = flr.RewriteInfiniteLoop(actual)
	default:
		shared.ReportErrorAndExit("CollectionsLoopRewriter", "Unexpected node "+node.LiteralRepr())
	}
	return node
}

func (flr *CollectionsForLoopRewriter) RewriteCollectionsForLoop(loop *ast.CollectionsForLoop) ast.ProtoNode {
	collection := loop.Collection
	var res ast.ProtoNode
	switch actual := collection.Type().(type) {
	case *ast.Proto_Array:
		if arr, ok := collection.(*ast.Array); ok {
			src := fmt.Sprintf(`
			{
				let _loop_collection_var_ = %s;

				for mut idx = 0; idx < len(_loop_collection_var_); idx += 1 {
					let %s = _loop_collection_var_[idx];
				}
			}
			`, arr.LiteralRepr(), loop.LoopVar.LiteralRepr())
			lex := lexer.NewFromLineCol(src, loop.Start.TokenSpan.Line, loop.Start.TokenSpan.Col)
			p := parser.NewWithLexer(lex)
			prog := parser.Top_Level(p, false)
			blk := prog.Contents[0].(*ast.BlockStmt)
			generic_loop := blk.Contents[1].(*ast.GenericForLoop)
			generic_loop.Body.Contents = append(generic_loop.Body.Contents, loop.Body.Contents...)
			res = blk
		} else {
			src := fmt.Sprintf(`
			for mut idx = 0; idx < len(%s); idx += 1 {
				let %s = %s[idx];
			}`, collection.LiteralRepr(), loop.LoopVar.LiteralRepr(), collection.LiteralRepr())
			lex := lexer.NewFromLineCol(src, loop.Start.TokenSpan.Line, loop.Start.TokenSpan.Col)
			p := parser.NewWithLexer(lex)
			prog := parser.Top_Level(p, false)
			generic_loop := prog.Contents[0].(*ast.GenericForLoop)
			generic_loop.Body.Contents = append(generic_loop.Body.Contents, loop.Body.Contents...)
			res = generic_loop
		}
	case *ast.Proto_Builtin:
		if str, ok := collection.(*ast.String); ok {
			src := fmt.Sprintf(`
			{
				let _loop_collection_var_ = %s;

				for mut idx = 0; idx < len(_loop_collection_var_); idx += 1 {
					let %s = _loop_collection_var_[idx];
				}
			}
			`, str.LiteralRepr(), loop.LoopVar.LiteralRepr())
			lex := lexer.NewFromLineCol(src, loop.Start.TokenSpan.Line, loop.Start.TokenSpan.Col)
			p := parser.NewWithLexer(lex)
			prog := parser.Top_Level(p, false)
			blk := prog.Contents[0].(*ast.BlockStmt)
			generic_loop := blk.Contents[1].(*ast.GenericForLoop)
			generic_loop.Body.Contents = append(generic_loop.Body.Contents, loop.Body.Contents...)
			res = blk
		} else {
			src := fmt.Sprintf(`
			for mut idx = 0; idx < len(%s); idx += 1 {
				let %s = %s[idx];
			}
			`, collection.LiteralRepr(), loop.LoopVar.LiteralRepr(), collection.LiteralRepr())
			lex := lexer.NewFromLineCol(src, loop.Start.TokenSpan.Line, loop.Start.TokenSpan.Col)
			p := parser.NewWithLexer(lex)
			prog := parser.Top_Level(p, false)
			generic_loop := prog.Contents[0].(*ast.GenericForLoop)
			generic_loop.Body.Contents = append(generic_loop.Body.Contents, loop.Body.Contents...)
			res = generic_loop
		}
	case *ast.Proto_Range:
		if rng, ok := collection.(*ast.Range); ok {
			switch actual.InternalType.TypeSignature() {
			case "i64":
				src := fmt.Sprintf(`
				for mut idx = %s; idx < %s; idx += 1 {
					let %s = idx;
				}
				`, rng.Start.LiteralRepr(), rng.PastEnd.LiteralRepr(), loop.LoopVar.LiteralRepr())
				lex := lexer.NewFromLineCol(src, loop.Start.TokenSpan.Line, loop.Start.TokenSpan.Col)
				p := parser.NewWithLexer(lex)
				prog := parser.Top_Level(p, false)
				generic_loop := prog.Contents[0].(*ast.GenericForLoop)
				generic_loop.Body.Contents = append(generic_loop.Body.Contents, loop.Body.Contents...)
				res = generic_loop
			case "char":
				src := fmt.Sprintf(`
				{
					let start = char_to_int(%s);
					let end = char_to_int(%s);
					for mut idx = start; idx < end; idx += 1 {
						let %s = int_to_char(idx);
					}
				}
				`, rng.Start.LiteralRepr(), rng.PastEnd.LiteralRepr(), loop.LoopVar.LiteralRepr())
				lex := lexer.NewFromLineCol(src, loop.Start.TokenSpan.Line, loop.Start.TokenSpan.Col)
				p := parser.NewWithLexer(lex)
				prog := parser.Top_Level(p, false)
				blk := prog.Contents[0].(*ast.BlockStmt)
				generic_loop := blk.Contents[2].(*ast.GenericForLoop)
				generic_loop.Body.Contents = append(generic_loop.Body.Contents, loop.Body.Contents...)
				res = blk
			}
		} else if irng, ok := collection.(*ast.InclusiveRange); ok {
			switch actual.InternalType.TypeSignature() {
			case "i64":
				src := fmt.Sprintf(`
				for mut idx = %s; idx <= %s; idx += 1 {
					let %s = idx;
				}
				`, irng.Start.LiteralRepr(), irng.End.LiteralRepr(), loop.LoopVar.LiteralRepr())
				lex := lexer.NewFromLineCol(src, loop.Start.TokenSpan.Line, loop.Start.TokenSpan.Col)
				p := parser.NewWithLexer(lex)
				prog := parser.Top_Level(p, false)
				generic_loop := prog.Contents[0].(*ast.GenericForLoop)
				generic_loop.Body.Contents = append(generic_loop.Body.Contents, loop.Body.Contents...)
				res = generic_loop
			case "char":
				src := fmt.Sprintf(`
				{
					let start = char_to_int(%s);
					let end = char_to_int(%s);
					for mut idx = start; idx <= end; idx += 1 {
						let %s = int_to_char(idx);
					}
				}
				`, irng.Start.LiteralRepr(), irng.End.LiteralRepr(), loop.LoopVar.LiteralRepr())
				lex := lexer.NewFromLineCol(src, loop.Start.TokenSpan.Line, loop.Start.TokenSpan.Col)
				p := parser.NewWithLexer(lex)
				prog := parser.Top_Level(p, false)
				blk := prog.Contents[0].(*ast.BlockStmt)
				generic_loop := blk.Contents[2].(*ast.GenericForLoop)
				generic_loop.Body.Contents = append(generic_loop.Body.Contents, loop.Body.Contents...)
				res = blk
			}
		} else {
			// a range stored in some sort of expression, so need to use built-ins to access start and end fields
			if !actual.IsInclusiveRange {
				switch actual.InternalType.TypeSignature() {
				case "i64":
					src := fmt.Sprintf(`
					{
						let r_start = range_start(%s);
						let r_end = range_end(%s);
						for mut idx = r_start; idx < r_end; idx += 1 {
							let %s = idx;
						}
					}
					`, collection.LiteralRepr(), collection.LiteralRepr(), loop.LoopVar.LiteralRepr())
					lex := lexer.NewFromLineCol(src, loop.Start.TokenSpan.Line, loop.Start.TokenSpan.Col)
					p := parser.NewWithLexer(lex)
					prog := parser.Top_Level(p, false)
					blk := prog.Contents[0].(*ast.BlockStmt)
					generic_loop := blk.Contents[2].(*ast.GenericForLoop)
					generic_loop.Body.Contents = append(generic_loop.Body.Contents, loop.Body.Contents...)
					res = blk
				case "char":
					src := fmt.Sprintf(`
					{
						let r_start = char_to_int(range_start(%s));
						let r_end = char_to_int(range_end(%s));
						for mut idx = r_start; idx < r_end; idx += 1 {
							let %s = int_to_char(idx);
						}
					}
					`, collection.LiteralRepr(), collection.LiteralRepr(), loop.LoopVar.LiteralRepr())
					lex := lexer.NewFromLineCol(src, loop.Start.TokenSpan.Line, loop.Start.TokenSpan.Col)
					p := parser.NewWithLexer(lex)
					prog := parser.Top_Level(p, false)
					blk := prog.Contents[0].(*ast.BlockStmt)
					generic_loop := blk.Contents[2].(*ast.GenericForLoop)
					generic_loop.Body.Contents = append(generic_loop.Body.Contents, loop.Body.Contents...)
					res = blk
				}
			} else {
				switch actual.InternalType.TypeSignature() {
				case "i64":
					src := fmt.Sprintf(`
					{
						let r_start = range_start(%s);
						let r_end = range_end(%s);
						for mut idx = r_start; idx <= r_end; idx += 1 {
							let %s = idx;
						}
					}
					`, collection.LiteralRepr(), collection.LiteralRepr(), loop.LoopVar.LiteralRepr())
					lex := lexer.NewFromLineCol(src, loop.Start.TokenSpan.Line, loop.Start.TokenSpan.Col)
					p := parser.NewWithLexer(lex)
					prog := parser.Top_Level(p, false)
					blk := prog.Contents[0].(*ast.BlockStmt)
					generic_loop := blk.Contents[2].(*ast.GenericForLoop)
					generic_loop.Body.Contents = append(generic_loop.Body.Contents, loop.Body.Contents...)
					res = blk
				case "char":
					src := fmt.Sprintf(`
					{
						let r_start = char_to_int(range_start(%s));
						let r_end = char_to_int(range_end(%s));
						for mut idx = r_start; idx <= r_end; idx += 1 {
							let %s = int_to_char(idx);
						}
					}
					`, collection.LiteralRepr(), collection.LiteralRepr(), loop.LoopVar.LiteralRepr())
					lex := lexer.NewFromLineCol(src, loop.Start.TokenSpan.Line, loop.Start.TokenSpan.Col)
					p := parser.NewWithLexer(lex)
					prog := parser.Top_Level(p, false)
					blk := prog.Contents[0].(*ast.BlockStmt)
					generic_loop := blk.Contents[2].(*ast.GenericForLoop)
					generic_loop.Body.Contents = append(generic_loop.Body.Contents, loop.Body.Contents...)
					res = blk
				}
			}

		}
	}
	return res
}
func (flr *CollectionsForLoopRewriter) RewriteInfiniteLoop(loop *ast.InfiniteLoop) *ast.InfiniteLoop {
	for index, node := range loop.Body.Contents {
		loop.Body.Contents[index] = flr.Rewrite(node)
	}
	return loop
}

func (flr *CollectionsForLoopRewriter) RewriteGenericForLoop(loop *ast.GenericForLoop) *ast.GenericForLoop {
	for index, node := range loop.Body.Contents {
		loop.Body.Contents[index] = flr.Rewrite(node)
	}
	return loop
}

func (flr *CollectionsForLoopRewriter) RewriteWhileLoop(while *ast.WhileLoop) *ast.WhileLoop {
	for index, node := range while.Body.Contents {
		while.Body.Contents[index] = flr.Rewrite(node)
	}

	return while
}

func (flr *CollectionsForLoopRewriter) RewriteBlockExpr(blk *ast.BlockExpr) *ast.BlockExpr {
	for index, node := range blk.Contents {
		blk.Contents[index] = flr.Rewrite(node)
	}
	return blk
}

func (flr *CollectionsForLoopRewriter) RewriteBlockStmt(blk *ast.BlockStmt) *ast.BlockStmt {
	for index, node := range blk.Contents {
		blk.Contents[index] = flr.Rewrite(node)
	}
	return blk
}

func (flr *CollectionsForLoopRewriter) RewriteFunctionDef(fn *ast.FunctionDef) *ast.FunctionDef {
	for index, node := range fn.Body.Contents {
		fn.Body.Contents[index] = flr.Rewrite(node)
	}
	return fn
}

func (flr *CollectionsForLoopRewriter) RewriteIfExpr(if_ *ast.IfExpr) *ast.IfExpr {
	for index, node := range if_.ThenBody.Contents {
		if_.ThenBody.Contents[index] = flr.Rewrite(node)
	}

	if if_.ElseBody != nil {
		switch actual := if_.ElseBody.(type) {
		case *ast.BlockExpr:
			if_.ElseBody = flr.RewriteBlockExpr(actual)
		case *ast.IfExpr:
			if_.ElseBody = flr.RewriteIfExpr(actual)
		}
	}

	return if_
}

func (flr *CollectionsForLoopRewriter) RewriteIfStmt(if_ *ast.IfStmt) *ast.IfStmt {
	for index, node := range if_.ThenBody.Contents {
		if_.ThenBody.Contents[index] = flr.Rewrite(node)
	}

	if if_.ElseBody != nil {
		switch actual := if_.ElseBody.(type) {
		case *ast.BlockStmt:
			if_.ElseBody = flr.RewriteBlockStmt(actual)
		case *ast.IfStmt:
			if_.ElseBody = flr.RewriteIfStmt(actual)
		}
	}

	return if_
}
