package analysis

import (
	"log"
	"proto/parser"
	"proto/shared"
	"testing"
)

func TestTypeCheckingVariableDeclaration(t *testing.T) {
	path := "../../samples/test_sources/type_checker/variable_decl.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}

func TestTypeCheckingComplexTypes(t *testing.T) {
	path := "../../samples/test_sources/type_checker/complex_types.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}

func TestTypeCheckingBlocks(t *testing.T) {
	path := "../../samples/test_sources/type_checker/blocks.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}

func TestTypeCheckingBinaryOps(t *testing.T) {
	path := "../../samples/test_sources/type_checker/binary_ops.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}

func TestTypeCheckingUnaryOps(t *testing.T) {
	path := "../../samples/test_sources/type_checker/unary_ops.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}

func TestTypeCheckingIfExprs(t *testing.T) {
	path := "../../samples/test_sources/type_checker/if_exprs.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}

func TestTypeCheckingLoops(t *testing.T) {
	path := "../../samples/test_sources/type_checker/loops.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}
