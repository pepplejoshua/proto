package name_resolver

import (
	"log"
	"proto/parser"
	"testing"
)

func TestResolvingVariableDeclaration(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/variable_decl.pr"
	program := parser.Parse(path, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingBlocks(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/blocks.pr"
	program := parser.Parse(path, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingTuplesAndArrays(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/tuples_and_arrays.pr"
	program := parser.Parse(path, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingFunctionUse(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/function_use.pr"
	program := parser.Parse(path, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingIfConditional(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/if.pr"
	program := parser.Parse(path, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingBinaryAndUnaryOps(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/binary_and_unary_ops.pr"
	program := parser.Parse(path, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingLoops(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/loops.pr"
	program := parser.Parse(path, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingRanges(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/ranges.pr"
	program := parser.Parse(path, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingIndexExpr(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/index_expr.pr"
	program := parser.Parse(path, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingStructUse(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/struct_use.pr"
	program := parser.Parse(path, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingMembership(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/membership.pr"
	program := parser.Parse(path, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingLetSingleAssignment(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/let_single_assignment.pr"
	program := parser.Parse(path, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingAssignments(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/assignment.pr"
	program := parser.Parse(path, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingBuiltinFunctions(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/builtins.pr"
	program := parser.Parse(path, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingDiscardVariable(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/discard_var.pr"
	program := parser.Parse(path, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}

func TestResolvingReferences(t *testing.T) {
	path := "../../samples/test_sources/name_resolver/references.pr"
	program := parser.Parse(path, false)

	nr := NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}
}
