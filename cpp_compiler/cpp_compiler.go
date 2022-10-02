package cpp_compiler

import (
	"fmt"
	"proto/ast"
	"proto/shared"
)

type Compiler struct{}

func (c *Compiler) CompileProgram(prog *ast.ProtoProgram, has_main bool) string {
	code_gen := ast.NewCodeGenerator()
	code_gen.Path = prog.Path

	code_gen.AddInclude("<iostream>")
	for _, node := range prog.Contents {
		node.AsCppCode(code_gen, true, true)
	}

	if has_main {
		var main *ast.FunctionDef
		for _, fn := range prog.FunctionDefs {
			if fn.Name.LiteralRepr() == "main" {
				main = fn
			}
		}

		// Test setup
		code_gen.WriteLine("#ifdef PROTO_TESTING", false)
		code_gen.WriteLine("TestFn tests[] = {", false)
		for _, test := range prog.Tests {
			name := shared.GetNameFromTest(test.Name.LiteralRepr())
			code_gen.IndentThenWriteline(fmt.Sprintf("{ .name = \"%s\", .fn = test_%s, },", name, name))
		}
		code_gen.WriteLine("};", false)
		code_gen.NewLine()

		// Test main
		code_gen.WriteLine("void test_main() {", false)
		code_gen.IndentThenWriteline(fmt.Sprintf("for (int i = 0; i < %d; i++) {", len(prog.Tests)))
		code_gen.IndentThenWriteline("    try {")
		code_gen.IndentThenWriteline("        TestFn* test = &tests[i];")
		code_gen.IndentThenWriteline("        cout << \"Test \" << i << \" '\" << test->name << \"'...\";")
		code_gen.IndentThenWriteline("        test->fn();")
		code_gen.IndentThenWriteline("        cout << \"PASSED\" << endl;")
		code_gen.IndentThenWriteline("    } catch(ProtoException ex) {")
		code_gen.IndentThenWriteline("        cout << \"FAILED\" << endl;")
		code_gen.IndentThenWriteline("        cout << \"Test error: \" << ex << endl;")
		code_gen.IndentThenWriteline("    }")
		code_gen.IndentThenWriteline("}")
		code_gen.WriteLine("}", false)
		code_gen.WriteLine("#endif", false)

		// Main
		code_gen.WriteLine("int main() {", false)
		code_gen.WriteLine("#ifdef PROTO_TESTING", false)
		code_gen.IndentThenWriteline("test_main();")
		code_gen.WriteLine("#else", false)
		if len(prog.Contents) > 0 {
			if main.ReturnType.TypeSignature() == "bool" {
				code_gen.IndentThenWriteline("cout << boolalpha << __main() << endl;")
			} else {
				code_gen.IndentThenWriteline("cout << __main() << endl;")
			}
		}
		code_gen.WriteLine("#endif", false)
		code_gen.IndentThenWriteline("return 0;")
		code_gen.WriteLine("}", false)
	}

	std_namespace := "using namespace std;\n"
	if has_main {
		return code_gen.GetIncludesAsString() + std_namespace + code_gen.CollectString()
	} else {
		return "#pragma once\n\n" + code_gen.GetIncludesAsString() + std_namespace + code_gen.CollectString()
	}
}
