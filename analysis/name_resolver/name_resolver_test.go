package name_resolver

import (
	"log"
	"proto/parser"
	"proto/shared"
	"testing"
)

func TestResolvingVariableDeclaration(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/variable_decl.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingBlocks(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/blocks.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingTuplesAndArrays(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/tuples_and_arrays.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingFunctionUse(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/function_use.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingIfConditional(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/if.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingBinaryAndUnaryOps(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/binary_and_unary_ops.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingLoops(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/loops.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingRanges(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/ranges.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingIndexExpr(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/index_expr.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingStructUse(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/struct_use.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingMembership(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/membership.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingLetSingleAssignment(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/let_single_assignment.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingAssignments(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/assignment.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}
