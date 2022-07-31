package type_checker

import (
	"log"
	"proto/parser"
	"proto/shared"
	"testing"
)

func TestTypeCheckingVariableDeclaration(t *testing.T) {
	path := "../../samples/test_sources/type_checker/variable_decl.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}

func TestTypeCheckingComplexTypes(t *testing.T) {
	path := "../../samples/test_sources/type_checker/complex_types.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}

func TestTypeCheckingBlocks(t *testing.T) {
	path := "../../samples/test_sources/type_checker/blocks.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}

func TestTypeCheckingBinaryOps(t *testing.T) {
	path := "../../samples/test_sources/type_checker/binary_ops.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}

func TestTypeCheckingUnaryOps(t *testing.T) {
	path := "../../samples/test_sources/type_checker/unary_ops.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}

func TestTypeCheckingIfExprs(t *testing.T) {
	path := "../../samples/test_sources/type_checker/if_exprs.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}

func TestTypeCheckingLoops(t *testing.T) {
	path := "../../samples/test_sources/type_checker/loops.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}

func TestTypeCheckingRanges(t *testing.T) {
	path := "../../samples/test_sources/type_checker/range_uses.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}

func TestTypeCheckingIndexExpressions(t *testing.T) {
	path := "../../samples/test_sources/type_checker/index_expr.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}

func TestTypeCheckingAssignments(t *testing.T) {
	path := "../../samples/test_sources/type_checker/assignment.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}

func TestTypeCheckingStructUses(t *testing.T) {
	path := "../../samples/test_sources/type_checker/struct_use.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}

func TestTypeCheckingMembership(t *testing.T) {
	path := "../../samples/test_sources/type_checker/membership.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}

func TestTypeCheckingFunctionUse(t *testing.T) {
	path := "../../samples/test_sources/type_checker/function_use.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}

func TestTypeCheckingBuiltins(t *testing.T) {
	path := "../../samples/test_sources/type_checker/builtins.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}

func TestTypeCheckingDiscardVariable(t *testing.T) {
	path := "../../samples/test_sources/type_checker/discard_var.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}
