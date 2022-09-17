package compilemanager

import (
	"bufio"
	"fmt"
	"io"
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

type Manager struct {
	Path     string
	SrcPath  string
	CleanSrc bool
}

func make_src_path(path string) string {
	abs_path := shared.Get_abs_path("CompileManager", path)
	base := shared.Strip_base(abs_path)
	filename := strings.Split(base, ".")[0] + ".cpp"

	dir := filepath.Dir(abs_path)
	src_loc := filepath.Join(dir, filename)
	return src_loc
}

func make_exe_path(path string) string {
	abs_path := shared.Get_abs_path("CompileManager", path)
	base := shared.Strip_base(abs_path)
	filename := strings.Split(base, ".")[0]

	dir := filepath.Dir(abs_path)
	exe_loc := filepath.Join(dir, filename)
	return exe_loc
}

func NewManager(path string, clean bool) *Manager {
	man := &Manager{
		Path:     path,
		SrcPath:  make_src_path(path),
		CleanSrc: clean,
	}

	// println(path, man.SrcPath, make_exe_path(path))
	return man
}

func (m *Manager) Compile() {
	project_org := NewProjectManager(m.Path)
	project_org.BuildProject()
	prog := parser.Parse(m.Path, true)

	// the import resolver will give me a bunch of source files to be
	// name resolved and typechecked and compiled
	// they will be compiled in reverse order due to the order in which
	// they will refer to each other

	nr := name_resolver.NewNameResolver()
	nr.ResolveProgram(prog)

	tc := type_checker.NewTypeChecker()
	tc.TypeCheckProgram(prog)

	sr := &syntaxrewriter.CollectionsForLoopRewriter{}
	sr.RewriteProgram(prog)

	nr.ResolveProgram(prog)
	tc.TypeCheckProgram(prog)

	code := &cpp_compiler.Compiler{}
	gen_src := code.CompileProgram(prog, true)

	// println(gen_src)
	src_path := make_src_path(m.Path)
	destination, _ := os.Create(src_path)
	fmt.Fprintln(destination, gen_src)
	destination.Close()
	println("generated c++ written to", src_path)

	if m.CleanSrc {
		defer os.RemoveAll(src_path)
	} else {
		clang_format := exec.Command("clang-format", src_path)
		stdout, err := clang_format.StdoutPipe()
		if err != nil {
			// println("HERE1")
			shared.ReportErrorAndExit("CompileManager", err.Error())
		}
		stderr, err := clang_format.StderrPipe()
		if err != nil {
			// println("HERE2")
			shared.ReportErrorAndExit("CompileManager", err.Error())
		}

		if err = clang_format.Start(); err != nil {
			// println("HERE3")
			shared.ReportErrorAndExit("CompileManager", err.Error())
		}

		data, err := io.ReadAll(stderr)
		if err != nil {
			// println("HERE4")
			shared.ReportErrorAndExit("CompileManager", err.Error())
		}

		errout := string(data)
		if errout != "" {
			// println("HERE5")
			msg := fmt.Sprintf("Error '%s' while formatting %s.", strings.TrimSpace(errout), src_path)
			shared.ReportErrorAndExit("CompileManager", msg)
		}

		output, err := io.ReadAll(stdout)
		if err != nil {
			// println("HERE6")
			msg := fmt.Sprintf("Error while reading format output. Error: %s", output)
			shared.ReportErrorAndExit("CompileManager", msg)
		}

		if err := clang_format.Wait(); err != nil {
			// println("HERE7")
			shared.ReportErrorAndExit("CompileManager", err.Error())
		}

		format_output := output
		// println(string(format_output))
		println("formatted source at", src_path)

		destination, _ := os.Create(src_path)
		defer destination.Close()
		fmt.Fprintln(destination, string(format_output))
		println("updated with formatted output", src_path)
	}

	exe_loc := make_exe_path(m.Path)
	compile_cmd := exec.Command("clang++", "-o", exe_loc, src_path, "-std=c++14")
	stderr, err := compile_cmd.StderrPipe()
	if err != nil {
		// println("HERE8")
		shared.ReportErrorAndExit("CompileManager", err.Error())
	}
	if err := compile_cmd.Start(); err != nil {
		scanner := bufio.NewScanner(stderr)
		for scanner.Scan() {
			fmt.Println(scanner.Text())
		}
		msg := fmt.Sprintf("Error while compiling %s.\n\t%s", src_path, err.Error())
		shared.ReportErrorAndExit("CompileManager", msg)
	}
	if err := compile_cmd.Wait(); err != nil {
		// println("HERE9")
		shared.ReportErrorAndExit("CompileManager", err.Error())
	}

	println("compiled to", exe_loc)
}
