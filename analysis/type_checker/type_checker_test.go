package type_checker

import (
	"log"
	"proto/analysis/name_resolver"
	"proto/parser"
	"proto/shared"
	"testing"
)

func TestTypeCheckingVariableDeclaration(t *testing.T) {
	path := "../../samples/test_sources/type_checker/variable_decl.pr"

	TypeCheckerTestRunner(t, path)
}

func TestTypeCheckingComplexTypes(t *testing.T) {
	path := "../../samples/test_sources/type_checker/complex_types.pr"

	TypeCheckerTestRunner(t, path)
}

func TestTypeCheckingBlocks(t *testing.T) {
	path := "../../samples/test_sources/type_checker/blocks.pr"

	TypeCheckerTestRunner(t, path)
}

func TestTypeCheckingBinaryOps(t *testing.T) {
	path := "../../samples/test_sources/type_checker/binary_ops.pr"

	TypeCheckerTestRunner(t, path)
}

func TestTypeCheckingUnaryOps(t *testing.T) {
	path := "../../samples/test_sources/type_checker/unary_ops.pr"

	TypeCheckerTestRunner(t, path)
}

func TestTypeCheckingIfConds(t *testing.T) {
	path := "../../samples/test_sources/type_checker/if_conds.pr"

	TypeCheckerTestRunner(t, path)
}

func TestTypeCheckingLoops(t *testing.T) {
	path := "../../samples/test_sources/type_checker/loops.pr"

	TypeCheckerTestRunner(t, path)
}

func TestTypeCheckingRanges(t *testing.T) {
	path := "../../samples/test_sources/type_checker/range_uses.pr"

	TypeCheckerTestRunner(t, path)
}

func TestTypeCheckingIndexExpressions(t *testing.T) {
	path := "../../samples/test_sources/type_checker/index_expr.pr"

	TypeCheckerTestRunner(t, path)
}

func TestTypeCheckingAssignments(t *testing.T) {
	path := "../../samples/test_sources/type_checker/assignment.pr"

	TypeCheckerTestRunner(t, path)
}

func TestTypeCheckingStructUses(t *testing.T) {
	path := "../../samples/test_sources/type_checker/struct_use.pr"

	TypeCheckerTestRunner(t, path)
}

func TestTypeCheckingMembership(t *testing.T) {
	path := "../../samples/test_sources/type_checker/membership.pr"

	TypeCheckerTestRunner(t, path)
}

func TestTypeCheckingFunctionUse(t *testing.T) {
	path := "../../samples/test_sources/type_checker/function_use.pr"

	TypeCheckerTestRunner(t, path)
}

func TestTypeCheckingBuiltins(t *testing.T) {
	path := "../../samples/test_sources/type_checker/builtins.pr"

	TypeCheckerTestRunner(t, path)
}

func TestTypeCheckingDiscardVariable(t *testing.T) {
	path := "../../samples/test_sources/type_checker/discard_var.pr"

	TypeCheckerTestRunner(t, path)
}

func TestTypeCheckingReferences(t *testing.T) {
	path := "../../samples/test_sources/type_checker/references.pr"

	TypeCheckerTestRunner(t, path)
}

func TestTypeCheckingDeferences(t *testing.T) {
	path := "../../samples/test_sources/type_checker/dereferences.pr"

	TypeCheckerTestRunner(t, path)
}

func TypeCheckerTestRunner(t *testing.T, path string) {
	t.Helper()

	src := shared.ReadFile(path)
	program := parser.Parse(src, false)

	nr := name_resolver.NewNameResolver()
	nr.ResolveProgram(program)

	tc := NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}
}
