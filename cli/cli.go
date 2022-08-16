package cli

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"proto/analysis/name_resolver"
	syntaxrewriter "proto/analysis/syntax_rewriter"
	"proto/analysis/type_checker"
	"proto/compiler"
	"proto/parser"
	"proto/shared"
	"proto/vm"
	"time"
)

var file = flag.String("file", "", "Path to Proto File to be compiled")
var show_time = flag.Bool("time", false, "Show the runtime of the code found in file.")
var debug = flag.Bool("dbg", false, "Enter debug mode to step through program")

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
	if tc.FoundError {
		println("Found errors during Type Checking.")
		return
	}

	sr := &syntaxrewriter.CollectionsForLoopRewriter{}
	sr.RewriteProgram(parsed_prog)

	nr.ResolveProgram(parsed_prog)
	if nr.FoundError {
		println("Found errors during Name Resolution.")
		return
	}

	tc.TypeCheckProgram(parsed_prog)
	if tc.FoundError {
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

	if *debug {
		vm := vm.NewVM(bytecode)

		last_ip := 0

		// allow:
		// - step forward
		// - step back
		// - allow jump
		// - run till finish
		scanner := bufio.NewScanner(os.Stdin)

		for {
			fmt.Println("enter f to step forward.")
			fmt.Println("enter r to run till completion.")
			// think of the implications of jumps or step backs
			// fmt.Println("'j :n' to jump to instruction number.")
			fmt.Println("'q' to quit.")
			scanner.Scan()
			choice := scanner.Text()
			switch choice {
			case "q":
				return
			case "f":
				insp := last_ip
				if insp < len(bytecode.Instructions) {
					fmt.Println()
					fmt.Println()
					insp = vm.Debug(insp)
					fmt.Printf("ran instruction: %d\n", last_ip)
					println("next instruction:", insp)
					vm.Show_stack()
					vm.Show_globals()
					last_ip = insp
				} else {
					return
				}
			case "r":
				insp := last_ip
				if insp < len(bytecode.Instructions) {
					fmt.Println()
					fmt.Println()
					vm.Run(false, insp)
					// fmt.Printf("ran instruction: %d\n", last_ip)
					// println("next instruction:", insp)
					// vm.Show_stack()
					// vm.Show_globals()
					return
				} else {
					return
				}
			}
		}
	} else {
		vm := vm.NewVM(bytecode)
		start := time.Now()
		vm.Run(false, 0)

		var duration time.Duration
		if *show_time {
			duration = time.Since(start)
			fmt.Printf("[ran in %s]\n", duration)
		}

		if vm.FoundError {
			println("Found errors during run of Virtual Machine.")
			return
		}

		res := vm.StackTop()
		if res.String() != "()" {
			println(res.String())
		}
	}

}
