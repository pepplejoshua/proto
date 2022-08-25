package cli

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"proto/analysis/name_resolver"
	syntaxrewriter "proto/analysis/syntax_rewriter"
	"proto/analysis/type_checker"
	"proto/cpp_compiler"
	"proto/parser"
	"proto/shared"
	"strings"
)

var file = flag.String("file", "", "Path to Proto File to be compiled")
var clean = flag.Bool("c", false, "Clean generated C++ files")

// var show_time = flag.Bool("time", false, "Show the runtime of the code found in file.")
// var debug = flag.Bool("dbg", false, "Enter debug mode to step through program")

func Start() {
	flag.Parse()

	if *file == "" {
		shared.ReportErrorAndExit("FileReader", "No file provided")
	}

	src := shared.ReadFile(*file)

	parsed_prog := parser.Parse(src, false)
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

	cpp_comp := cpp_compiler.NewCppCompiler(*file, true)

	cpp_src := cpp_comp.CompileProgram(parsed_prog)
	// println(cpp_src)
	abs_path, _ := filepath.Abs(*file)
	base_name := filepath.Base(abs_path)
	file_name := strings.Split(base_name, ".")[0]
	fname := file_name + ".cpp"
	comp_loc := filepath.Dir(abs_path)
	exe_loc := filepath.Join(comp_loc, file_name)
	final_ := filepath.Join(comp_loc, fname)

	destination, _ := os.Create(final_)

	defer destination.Close()
	fmt.Fprintln(destination, cpp_src)
	println("written to", final_)

	if *clean {
		defer os.RemoveAll(final_)
	}

	clang_format := exec.Command("clang-format", "\""+final_+"\"", ">", final_)
	format_output, _ := clang_format.Output()
	clang_format.Run()
	println("formatted source at", final_)

	destination, _ = os.Create(final_)

	defer destination.Close()
	fmt.Fprintln(destination, string(format_output))
	println("updated", final_)

	clangpp_cmd := exec.Command("g++", "-o", exe_loc, final_, "-std=c++14")
	stderr, _ := clangpp_cmd.StderrPipe()
	if err := clangpp_cmd.Start(); err != nil {
		log.Fatal(err)
	}

	scanner := bufio.NewScanner(stderr)
	for scanner.Scan() {
		fmt.Println(scanner.Text())
	}
	println("compiled to", exe_loc)

	// compiler := compiler.NewCompiler()
	// compiler.CompileProgram(parsed_prog)

	// if compiler.FoundError {
	// 	println("Found errors during Compilation.")
	// 	return
	// }

	// bytecode := compiler.ByteCode()

	// if *debug {
	// 	vm := vm.NewVM(bytecode)

	// 	last_ip := 0

	// allow:
	// - step forward
	// - step back
	// - allow jump
	// - run till finish
	// scanner := bufio.NewScanner(os.Stdin)

	// for {
	// 	fmt.Println("enter f to step forward.")
	// 	fmt.Println("enter r to run till completion.")
	// think of the implications of jumps or step backs
	// fmt.Println("'j :n' to jump to instruction number.")
	// fmt.Println("'q' to quit.")
	// scanner.Scan()
	// choice := scanner.Text()
	// switch choice {
	// case "q":
	// 	return
	// case "f":
	// 	insp := last_ip
	// 	if insp < len(bytecode.Instructions) {
	// 		fmt.Println()
	// 		fmt.Println()
	// 		insp = vm.Debug(insp)
	// 		fmt.Printf("ran instruction: %d\n", last_ip)
	// 		println("next instruction:", insp)
	// 		vm.Show_stack()
	// 		vm.Show_globals()
	// 		last_ip = insp
	// 	} else {
	// 		return
	// 	}
	// case "r":
	// 	insp := last_ip
	// 	if insp < len(bytecode.Instructions) {
	// 		fmt.Println()
	// 		fmt.Println()
	// 		vm.Run(false, insp)
	// fmt.Printf("ran instruction: %d\n", last_ip)
	// println("next instruction:", insp)
	// vm.Show_stack()
	// vm.Show_globals()
	// 				return
	// 			} else {
	// 				return
	// 			}
	// 		}
	// 	}
	// } else {
	// 	vm := vm.NewVM(bytecode)
	// 	start := time.Now()
	// 	vm.Run(false, 0)

	// 	var duration time.Duration
	// 	if *show_time {
	// 		duration = time.Since(start)
	// 		fmt.Printf("[ran in %s]\n", duration)
	// 	}

	// 	if vm.FoundError {
	// 		println("Found errors during run of Virtual Machine.")
	// 		return
	// 	}

	// 	res := vm.StackTop()
	// 	if res.String() != "()" {
	// 		println(res.String())
	// 	}
	// }

}
