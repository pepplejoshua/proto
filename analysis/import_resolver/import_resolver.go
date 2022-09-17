package import_resolver

import (
	"fmt"
	"os"
	"path/filepath"
	"proto/ast"
	"proto/shared"
	"strings"
)

type ImportResolver struct {
	Dir   string
	Progs []*ast.ProtoProgram
}

func (ir *ImportResolver) ResolveProgram(prog *ast.ProtoProgram) []*ast.ProtoProgram {
	ir.Progs = append(ir.Progs, prog)
	for _, node := range prog.Contents {
		ir.Resolve(node)
	}
	return ir.Progs
}

func (ir *ImportResolver) Resolve(node ast.ProtoNode) {
	switch actual := node.(type) {
	// don't need checking
	case *ast.I64, *ast.String, *ast.Char, *ast.Boolean,
		*ast.Unit, *ast.Continue, *ast.Break, *ast.Membership,
		*ast.VariableDecl, *ast.Dereference, *ast.Identifier,
		*ast.Assignment, *ast.Struct, *ast.StructInitialization,
		*ast.PromotedExpr, *ast.Tuple, *ast.Array, *ast.CallExpression,
		*ast.BinaryOp, *ast.UnaryOp, *ast.Reference, *ast.Return,
		*ast.InclusiveRange, *ast.Range, *ast.IndexExpression:
	case *ast.WhileLoop:
		ir.ResolveBlockStmt(actual.Body)
	case *ast.InfiniteLoop:
		ir.ResolveBlockStmt(actual.Body)
	case *ast.GenericForLoop:
		ir.ResolveBlockStmt(actual.Body)
	case *ast.CollectionsForLoop:
		ir.ResolveBlockStmt(actual.Body)
	case *ast.IfExpr:
		ir.ResolveBlockExpr(actual.ThenBody)
		if actual.ElseBody != nil {
			ir.Resolve(actual.ElseBody)
		}
	case *ast.IfStmt:
		ir.ResolveBlockStmt(actual.ThenBody)
		if actual.ElseBody != nil {
			ir.Resolve(actual.ElseBody)
		}
	case *ast.BlockExpr:
		ir.ResolveBlockExpr(actual)
	case *ast.BlockStmt:
		ir.ResolveBlockStmt(actual)
	case *ast.FunctionDef:
		ir.ResolveBlockExpr(actual.Body)
	case *ast.Module:
		ir.ResolveBlockStmt(actual.Body)
	case *ast.UseStmt:
		ir.ResolveUseStmt(actual)
	default:
		shared.ReportErrorAndExit("ImportResolver", fmt.Sprintf("Unexpected node: %s", node.LiteralRepr()))
	}
}

func (ir *ImportResolver) ResolveUseStmt(use *ast.UseStmt) {
}

func find_file(path string, use_path string) (bool, string, []string) {
	main_path := path
	file_path := ""
	pieces := strings.Split(use_path, "::")

	found_file := false
	for index, p := range pieces {
		main_path = filepath.Join(main_path, p)

		// check for proto file
		if _, err := os.Stat(main_path + ".pr"); os.IsNotExist(err) {
			// since its not a file, check for a directory
			if _, err := os.Stat(main_path); os.IsNotExist(err) {
				return false, main_path, pieces
			} else {
				// it is directory, continue
				continue
			}
		} else {
			// we have seen a file
			found_file = true
			file_path = main_path + ".pr"
			pieces = pieces[index+1:]
			break
		}
	}
	return found_file, file_path, pieces
}

// func (ir *ImportResolver) process_tree(dir string, use_ast *ast.UseTree) {
// 	paths := use_ast.ShowAllPaths()

// 	for _, path := range paths {
// 		found_file, filepath, pieces := find_file(dir, path)

// 		if !found_file {
// 			var msg strings.Builder
// 			line := use_ast.StartToken.TokenSpan.Line
// 			col := use_ast.StartToken.TokenSpan.Col
// 			msg.WriteString(fmt.Sprintf("%d:%d: Unable to find any directory or proto file at %s", line,
// 				col, filepath))
// 			shared.ReportErrorAndExit("ImportResolver", msg.String())
// 		}
// 		use_ast.FileLoc = filepath
// 		use_ast.ItemsToCheck = pieces
// 		println(found_file, filepath, strings.Join(pieces, "::"))
// 	}
// }

func (ir *ImportResolver) ResolveBlockStmt(blk *ast.BlockStmt) {
	for _, node := range blk.Contents {
		ir.Resolve(node)
	}
}

func (ir *ImportResolver) ResolveBlockExpr(blk *ast.BlockExpr) {
	for _, node := range blk.Contents {
		ir.Resolve(node)
	}
}
