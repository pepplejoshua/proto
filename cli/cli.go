package cli

import (
	"flag"
	"fmt"
	"proto/analysis/name_resolver"
	"proto/analysis/type_checker"
	"proto/compiler"
	"proto/parser"
	"proto/shared"
	"proto/vm"
	"time"
)

var file = flag.String("file", "", "Path to Proto File to be compiled")

func Start() {
	flag.Parse()
	src := shared.ReadFile(*file)

	parsed_prog := parser.Parse(src, true)
	nr := name_resolver.NewNameResolver()
	nr.ResolveProgram(parsed_prog)
	if nr.FoundError {
		println("Found errors during Name Resolution.")
		return
	}

	tc := type_checker.NewTypeChecker()
	tc.TypeCheckProgram(parsed_prog)
	if nr.FoundError {
		println("Found errors during Type Checking.")
		return
	}

	compiler := compiler.NewCompiler()
	compiler.CompileProgram(parsed_prog)

	if compiler.FoundError {
		println("Found errors during Compilation.")
		return
	}

	bytecode := compiler.ByteCode()
	vm := vm.NewVM(bytecode)
	start := time.Now()
	vm.Run()
	duration := time.Since(start)

	fmt.Printf("[ran in %s]\n", duration)

	if vm.FoundError {
		println("Found errors during run of Virtual Machine.")
		return
	}

	res := vm.LastPoppedElem()

	if res.String() != "()" {
		println(res.String())
	}
}
