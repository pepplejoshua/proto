package cpp_compiler

/*
* i64 -> int64_t
* bool -> bool
* char -> char
* string -> std::string
	- all operations defined in proto should map to c++ operations for the above
  	types
* struct -> regular c++ struct
* unit -> nil // i wonder if this is okay
	- is there a better way of doing this?
* tuple -> vector of multiple types...
	- use <tuple> built into C++
* ranges -> array of 2 items, start and end
	- easy enough to handle ranges this way
* array
	- use <array> or <vector>, or just a regular C++ static array
* blocks -> C++ blocks
* assignment
	- caveat for doing if expr or block expr as rhs of assignment. solvable
	- since auto cannot be used without init value, uninit'd var must have actual type, not auto
* if expr and block expr ^ refer to above for assignments
	- for use in implicit returns from blocks, need to figure out the proper way to do it
* if stmt and block stmt -> regular c++ stmts
* references and dereferences might be replaced. Or not. Decide later
*/

import (
	"proto/ast"
)

type Compiler struct{}

func (c *Compiler) CompileProgram(prog *ast.ProtoProgram, has_main bool) string {
	code_gen := ast.NewCodeGenerator()

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
			if main.ReturnType.CppTypeSignature() == "Proto_Unit" {
				code_gen.IndentThenWriteline("__main();")
			} else {
				code_gen.IndentThenWriteline("cout << __main() << endl;")
			}
		}
		code_gen.IndentThenWriteline("return 0;")
		code_gen.WriteLine("}", false)
	}

	std_namespace := "using namespace std;\n"

	return code_gen.GetIncludesAsString() + std_namespace + code_gen.CollectString()
}
