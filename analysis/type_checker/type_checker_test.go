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
