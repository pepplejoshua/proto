package cpp_compiler

import (
	"proto/ast"
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

		code_gen.WriteLine("int main() {", false)
		if len(prog.Contents) > 0 {
			if main.ReturnType.TypeSignature() == "bool" {
				code_gen.IndentThenWriteline("cout << boolalpha << __main() << endl;")
			} else {
				code_gen.IndentThenWriteline("cout << __main() << endl;")
			}
		}
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
