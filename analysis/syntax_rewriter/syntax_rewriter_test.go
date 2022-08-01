package syntaxrewriter

import (
	"log"
	"proto/analysis/name_resolver"
	"proto/analysis/type_checker"
	"proto/parser"
	"proto/shared"
	"testing"
)

func TestSimpleRewrite(t *testing.T) {
	path := "../../samples/test_sources/syntax_rewriter/simple_case.pr"
	src := shared.ReadFile(path)

	program := parser.Parse(src, false)

	nr := name_resolver.NewNameResolver()
	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}

	tc := type_checker.NewTypeChecker()
	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}

	sr := &CollectionsForLoopRewriter{}
	sr.RewriteProgram(program)

	for index, node := range program.Contents {
		println(index+1, node.LiteralRepr())
	}

	nr.ResolveProgram(program)

	if nr.FoundError {
		log.Fatal("Found errors during name resolution")
	}

	for index, node := range program.Contents {
		println(index+1, node.LiteralRepr())
	}

	tc.TypeCheckProgram(program)

	if tc.FoundError {
		log.Fatal("Found errors during type checking")
	}

}
