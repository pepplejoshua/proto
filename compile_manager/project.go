package compilemanager

import (
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"proto/analysis/name_resolver"
	syntaxrewriter "proto/analysis/syntax_rewriter"
	"proto/analysis/type_checker"
	"proto/ast"
	"proto/cpp_compiler"
	"proto/lexer"
	"proto/parser"
	"proto/shared"
	"strings"
)

type ProjectOrganizer struct {
	startfile              string
	dir                    string
	cache                  map[string]*ast.ProtoProgram
	files_and_dependencies map[string][]string
	files_and_resolvables  map[string][]*ast.Path
	compiled_files         []string
	CleanSrc               bool
}

func NewProjectManager(file string, clean_src bool) *ProjectOrganizer {
	abs, _ := filepath.Abs(file)
	return &ProjectOrganizer{
		startfile:              abs,
		dir:                    filepath.Dir(abs),
		cache:                  map[string]*ast.ProtoProgram{},
		files_and_dependencies: map[string][]string{},
		files_and_resolvables:  map[string][]*ast.Path{},
		compiled_files:         []string{},
		CleanSrc:               clean_src,
	}
}

func (po *ProjectOrganizer) ProcessSourceFile(file string, has_main bool) (map[string][]*ast.Path, *ast.ProtoProgram) {
	file_ast := parser.Parse(file, has_main)
	file_ast.Path = file
	if !has_main {
		filename := strings.Split(filepath.Base(file), ".pr")[0]
		module := &ast.Module{
			Start: file_ast.Start,
			Body: &ast.BlockStmt{
				Start:         file_ast.Start,
				End:           file_ast.End,
				Contents:      file_ast.Contents,
				Modules:       file_ast.Modules,
				Functions:     file_ast.FunctionDefs,
				VariableDecls: file_ast.VariableDecls,
			},
			Name: &ast.Identifier{
				Token: lexer.ProtoToken{
					Type:      "IDENT",
					Literal:   filename,
					TokenSpan: file_ast.Start.TokenSpan,
				},
				Id_Type: nil,
			},
		}
		file_ast = &ast.ProtoProgram{
			Start:         file_ast.Start,
			End:           file_ast.End,
			Path:          file_ast.Path,
			Main:          nil,
			Contents:      []ast.ProtoNode{module},
			FunctionDefs:  []*ast.FunctionDef{},
			Structs:       []*ast.Struct{},
			Imports:       file_ast.Imports,
			Modules:       []*ast.Module{module},
			VariableDecls: []*ast.VariableDecl{},
		}
	}
	imports := file_ast.Imports

	files_and_resolvables := map[string][]*ast.Path{}

	for _, i := range imports {
		files_and_remnants := po.process_paths(file, po.dir, i.Paths)

		for dep_file, res_paths := range files_and_remnants {
			// prevent duplication of resolvable paths
			if paths, ok := files_and_resolvables[dep_file]; ok {
				for _, r_p := range res_paths {
					// r_p.Start = &i.Start
					// duplicate := false
					// for _, p := range paths {
					// 	if r_p.String() == p.String() {
					// 		duplicate = true
					// 		break
					// 	}
					// }
					// if !duplicate {
					paths = append(paths, r_p)
					files_and_resolvables[dep_file] = paths
					// }
				}
			} else {
				files_and_resolvables[dep_file] = res_paths
			}

			// prevent duplication of dependencies
			if dependencies, ok := po.files_and_dependencies[file]; ok {
				// duplicate := false
				// for _, dep := range dependencies {
				// 	if dep == dep_file {
				// 		duplicate = true
				// 		break
				// 	}
				// }
				// if !duplicate {
				dependencies = append(dependencies, dep_file)
				po.files_and_dependencies[file] = dependencies
				// }
			} else {
				po.files_and_dependencies[file] = []string{dep_file}
			}
		}
	}

	return files_and_resolvables, file_ast
}

func (po *ProjectOrganizer) SearchASTFor(prog *ast.ProtoProgram, paths []*ast.Path) map[string]ast.ProtoNode {
	res := map[string]ast.ProtoNode{}
	for _, path := range paths {
		var cur_mod *ast.Module = nil
		// start from the first piece in the pieces
		mod_name := path.Pieces[0]
		for _, module := range prog.Modules {
			if module.Name.LiteralRepr() == mod_name.String() {
				piece := path.Pieces[0]
				id, _ := piece.(*ast.PathIDNode)
				id.Type = lexer.MOD
				cur_mod = module
				break
			}
		}

		if cur_mod == nil {
			// no matching module found
			var msg strings.Builder
			msg.WriteString(fmt.Sprintf("Unable to find identifier '%s' in '%s'.", mod_name, prog.Path))
			shared.ReportErrorAndExit("ProjectOrganizer", msg.String())
		}

		var item ast.ProtoNode = cur_mod
		index := 1
	loop:
		for ; index < len(path.Pieces); index++ {
			piece := path.Pieces[index]

			if piece.String() == "*" {
				id, _ := piece.(*ast.PathIDNode)
				id.Type = lexer.STAR
				break
			}

			if as, ok := piece.(*ast.UseAs); ok {
				as.PType = lexer.AS
				break
			}

			switch ac := item.(type) {
			case *ast.Module:
				for _, mod := range ac.Body.Modules {
					if mod.Name.LiteralRepr() == piece.String() {
						item = mod
						id, _ := piece.(*ast.PathIDNode)
						id.Type = lexer.MOD
						if index+1 < len(path.Pieces) {
							next := path.Pieces[index+1]
							if as, ok := next.(*ast.UseAs); ok {
								as.PType = lexer.AS
								index++
								break loop
							}
						}
						continue loop
					}
				}

				for _, fn := range ac.Body.Functions {
					if fn.Name.LiteralRepr() == piece.String() {
						item = fn
						id, _ := piece.(*ast.PathIDNode)
						id.Type = lexer.FN
						// there are still more pieces to process
						if index+1 < len(path.Pieces) {
							next := path.Pieces[index+1]
							if as, ok := next.(*ast.UseAs); ok {
								as.PType = lexer.AS
								index++
								break loop
							}
						}
						continue loop
					}
				}

				for _, decl := range ac.Body.VariableDecls {
					if decl.Assignee.LiteralRepr() == piece.String() {
						item = decl
						id, _ := piece.(*ast.PathIDNode)
						id.Type = lexer.LET
						if index+1 < len(path.Pieces) {
							next := path.Pieces[index+1]
							if as, ok := next.(*ast.UseAs); ok {
								as.PType = lexer.AS
								index++
								break loop
							}
						}
						continue loop
					}
				}

				// not found in module
				var msg strings.Builder
				user := path.UseSrcLoc
				line := path.Start.TokenSpan.Line
				col := path.Start.TokenSpan.Col
				msg.WriteString(fmt.Sprintf("%s %d:%d ", user, line, col))
				msg.WriteString(fmt.Sprintf("Unable to find identifier '%s' in module '%s'.", piece.String(), ac.Name.LiteralRepr()))
				shared.ReportErrorAndExit("ProjectOrganizer", msg.String())
			case *ast.FunctionDef:
				var msg strings.Builder
				user := path.UseSrcLoc
				line := path.Start.TokenSpan.Line
				col := path.Start.TokenSpan.Col
				msg.WriteString(fmt.Sprintf("%s %d:%d ", user, line, col))
				msg.WriteString(fmt.Sprintf("Accessing member '%s' on function '%s' is not valid.", piece.String(),
					ac.Name.LiteralRepr()))
				shared.ReportErrorAndExit("ProjectManager", msg.String())
			case *ast.VariableDecl:
				var msg strings.Builder
				user := path.UseSrcLoc
				line := path.Start.TokenSpan.Line
				col := path.Start.TokenSpan.Col
				msg.WriteString(fmt.Sprintf("%s %d:%d ", user, line, col))
				msg.WriteString(fmt.Sprintf("Accessing member '%s' on variable '%s' is not valid.", piece.String(),
					ac.Assignee.LiteralRepr()))
				shared.ReportErrorAndExit("ProjectManager", msg.String())
			}
		}

		tags := []string{}
		for i := 0; i < len(path.Pieces)-1; i++ {
			tags = append(tags, path.Pieces[i].String())
		}
		tag := strings.Join(tags, "::")
		tag += "::"

		// there are additional pieces to be processed for this path
		if index < len(path.Pieces) {
			piece := path.Pieces[index]
			if piece.String() == "*" {
				if mod, ok := item.(*ast.Module); ok {
					name := tag + mod.Name.LiteralRepr()
					res[name] = mod
				} else if decl, ok := item.(*ast.VariableDecl); ok {
					// cannot use * operator on non-module
					var msg strings.Builder
					loc := path.PathSrcLoc
					user := path.UseSrcLoc
					line := path.Start.TokenSpan.Line
					col := path.Start.TokenSpan.Col
					msg.WriteString(fmt.Sprintf("%s %d:%d ", user, line, col))
					msg.WriteString(fmt.Sprintf("Using '*' when importing variable '%s' from '%s' is not valid.", decl.Assignee.LiteralRepr(), loc))
					shared.ReportErrorAndExit("ProjectManager", msg.String())
				} else if fn, ok := item.(*ast.FunctionDef); ok {
					var msg strings.Builder
					loc := path.PathSrcLoc
					user := path.UseSrcLoc
					line := path.Start.TokenSpan.Line
					col := path.Start.TokenSpan.Col
					msg.WriteString(fmt.Sprintf("%s %d:%d ", user, line, col))
					msg.WriteString(fmt.Sprintf("Using '*' when importing '%s' function from '%s' is not valid.", fn.Name.LiteralRepr(), loc))
					shared.ReportErrorAndExit("ProjectManager", msg.String())
				}
			} else if as, ok := piece.(*ast.UseAs); ok {
				alias := as.As.LiteralRepr()
				res[tag+alias] = item
			}
		} else {
			// there are no pieces to process
			switch actual := item.(type) {
			case *ast.FunctionDef:
				res[tag+actual.Name.LiteralRepr()] = item
			case *ast.Module:
				res[tag+actual.Name.LiteralRepr()] = item
			case *ast.VariableDecl:
				res[tag+actual.Assignee.LiteralRepr()] = item
			}
		}
	}
	return res
}

func (po *ProjectOrganizer) ProcessImports(queue []string, file string, has_main bool) *ast.ProtoProgram {
	if file == po.startfile {
		parent := queue[len(queue)-1]
		// cannot depend on the start file as its the main file
		var msg strings.Builder
		msg.WriteString(fmt.Sprintf("'%s' depends on main file: '%s'. This is not allowed.", parent, file))
		shared.ReportErrorAndExit("ProjectOrganizer", msg.String())
	}

	imported, file_ast := po.ProcessSourceFile(file, has_main)
	po.cache[file] = file_ast
	info_loc := map[string]ast.ProtoNode{}
	for d_file, rest := range imported {
		// circular dependency
		for _, path := range queue {
			if d_file == path {
				var msg strings.Builder
				msg.WriteString("Circular dependency:\n")
				msg.WriteString("  Dependencies:\n")
				for index, path := range queue {
					if path == d_file {
						msg.WriteString(fmt.Sprintf("=>%d. %s (earlier dependency here).\n", index+1, path))
					} else {
						msg.WriteString(fmt.Sprintf("  %d. %s.\n", index+1, path))
					}
				}
				msg.WriteString(fmt.Sprintf("  %d. %s.\n", len(queue)+1, file))
				msg.WriteString(fmt.Sprintf("=>%d. %s.\n", len(queue)+2, d_file))
				// msg.WriteString(fmt.Sprintf("'%s' depends on '%s'.", d_file, file))
				shared.ReportErrorAndExit("ProjectOrganizer", msg.String())
			}
		}
		var d_ast *ast.ProtoProgram
		new_queue := []string{}
		new_queue = append(new_queue, queue...)
		new_queue = append(new_queue, file)
		if cached_ast, ok := po.cache[d_file]; ok {
			if d_file == po.startfile {
				parent := new_queue[len(new_queue)-1]
				// cannot depend on the start file as its the main file
				var msg strings.Builder
				msg.WriteString(fmt.Sprintf("'%s' depends on main file: '%s'. This is not allowed.", file, parent))
				shared.ReportErrorAndExit("ProjectOrganizer", msg.String())
			}
			d_ast = cached_ast
		} else {
			d_ast = po.ProcessImports(new_queue, d_file, has_main)
			po.cache[d_file] = d_ast
		}
		info := po.SearchASTFor(d_ast, rest)
		for name, item := range info {
			info_loc[name] = item
		}
	}

	// before we return ast, we have to make sure we name resolve and typecheck ast
	// so the caller can take the fully analysed tree and take what they need from it
	// using ast.SearchFor()
	po.AnalyseAST(file_ast, info_loc)
	po.GenerateCppFor(file, file_ast, false)
	return file_ast
}

func (po *ProjectOrganizer) AnalyseAST(prog *ast.ProtoProgram, import_info map[string]ast.ProtoNode) {
	nr := name_resolver.NewNameResolverWithImportInfo(import_info)
	nr.ResolveProgram(prog)

	tc := type_checker.NewTypeCheckerWithImportInfo(import_info)
	tc.TypeCheckProgram(prog)

	sr := syntaxrewriter.CollectionsForLoopRewriter{}
	sr.RewriteProgram(prog)

	nr.ResolveProgram(prog)
	tc.TypeCheckProgram(prog)
}

func (po *ProjectOrganizer) GenerateCppFor(file string, prog *ast.ProtoProgram, compile bool) {
	code := &cpp_compiler.Compiler{}
	gen_src := code.CompileProgram(prog, compile)

	// println(gen_src)
	src_path := shared.Make_src_path(file, compile)
	destination, _ := os.Create(src_path)
	fmt.Fprintln(destination, gen_src)
	destination.Close()
	println("generated c++ written to", src_path)

	if po.CleanSrc {
		defer os.RemoveAll(src_path)
	} else {
		clang_format := exec.Command("clang-format", src_path)
		stdout, err := clang_format.StdoutPipe()
		if err != nil {
			// println("HERE1")
			shared.ReportErrorAndExit("ProjectOrganizer", err.Error())
		}
		stderr, err := clang_format.StderrPipe()
		if err != nil {
			// println("HERE2")
			shared.ReportErrorAndExit("ProjectOrganizer", err.Error())
		}

		if err = clang_format.Start(); err != nil {
			// println("HERE3")
			shared.ReportErrorAndExit("ProjectOrganizer", err.Error())
		}

		data, err := io.ReadAll(stderr)
		if err != nil {
			// println("HERE4")
			shared.ReportErrorAndExit("ProjectOrganizer", err.Error())
		}

		errout := string(data)
		if errout != "" {
			// println("HERE5")
			msg := fmt.Sprintf("Error '%s' while formatting %s.", strings.TrimSpace(errout), src_path)
			shared.ReportErrorAndExit("ProjectOrganizer", msg)
		}

		output, err := io.ReadAll(stdout)
		if err != nil {
			// println("HERE6")
			msg := fmt.Sprintf("Error while reading format output. Error: %s", output)
			shared.ReportErrorAndExit("ProjectOrganizer", msg)
		}

		if err := clang_format.Wait(); err != nil {
			// println("HERE7")
			shared.ReportErrorAndExit("ProjectOrganizer", err.Error())
		}

		format_output := output
		// println(string(format_output))
		println("formatted source at", src_path)

		destination, _ := os.Create(src_path)
		defer destination.Close()
		fmt.Fprintln(destination, string(format_output))
		println("updated with formatted output", src_path)
	}

	if compile {
		po.CompileFile(src_path, shared.Make_exe_path(file))
	}
}

func (po *ProjectOrganizer) CompileFile(src_path, exe_loc string) {
	compile_cmd := exec.Command("clang++", "-o", exe_loc, src_path, "-std=c++14", "-Wno-unused-value")
	stderr, err := compile_cmd.StderrPipe()
	if err != nil {
		// println("HERE8")
		shared.ReportErrorAndExit("ProjectOrganizer", err.Error())
	}
	if err := compile_cmd.Start(); err != nil {
		shared.ReportErrorAndExit("ProjectOrganizer", err.Error())
	}

	data, err := io.ReadAll(stderr)
	if err != nil {
		// println("HERE9")
		shared.ReportErrorAndExit("ProjectOrganizer", err.Error())
	}

	errout := string(data)
	if errout != "" {
		// println("HERE10")
		msg := fmt.Sprintf("Error '%s' while compiling %s.", strings.TrimSpace(errout), src_path)
		shared.ReportErrorAndExit("ProjectOrganizer", msg)
	}

	if err := compile_cmd.Wait(); err != nil {
		// println("HERE11")
		shared.ReportErrorAndExit("ProjectOrganizer", err.Error())
	}
	println("compiled to", exe_loc)
}

func (po *ProjectOrganizer) BuildProject() {
	// add the source file that was used to start the compilation of the project
	file_imported_bundle, ast_prog := po.ProcessSourceFile(po.startfile, true)

	po.cache[po.startfile] = ast_prog

	info_loc := map[string]ast.ProtoNode{}
	// track each file_imported_bundle in a ProjectOrganizer member
	for d_file, rest := range file_imported_bundle {
		d_ast := po.ProcessImports([]string{po.startfile}, d_file, false)
		info := po.SearchASTFor(d_ast, rest)
		for name, item := range info {
			info_loc[name] = item
		}
	}

	// then do analysis for ast_prog using all the pieces taken from imported files
	po.AnalyseAST(ast_prog, info_loc)
	po.GenerateCppFor(po.startfile, ast_prog, true)
}

func (po *ProjectOrganizer) find_file(dir string, path *ast.Path) (bool, string, *ast.Path) {
	file_path := ""
	main_path := dir
	file_end := -1
	pieces := []ast.UsePath{}
loop:
	for index, n := range path.Pieces {
		switch actual := n.(type) {
		case *ast.PathIDNode:
			if actual.String() == "*" {
				return false, main_path, path
			}
			main_path = filepath.Join(main_path, actual.String())
			// check for proto file
			if _, err := os.Stat(main_path + ".pr"); os.IsNotExist(err) {
				// since its not a file, check for a directory
				if _, err := os.Stat(main_path); os.IsNotExist(err) {
					return false, main_path, path
				} else {
					// it is directory, continue
					continue
				}
			} else {
				// we have seen a file
				file_end = index + 1
				pieces = path.Pieces[file_end:]
				if len(pieces) == 0 {
					// means we matched the full path as a file
					pieces = append(pieces, actual)
				} else {
					temp := []ast.UsePath{actual}
					temp = append(temp, pieces...)
					pieces = temp
				}
				file_path = main_path + ".pr"
				break loop
			}
			// }
		case *ast.UseAs:
			return false, main_path, path
		}
	}
	return true, file_path, &ast.Path{
		Start:  path.Start,
		Pieces: pieces,
	}
}

func (po *ProjectOrganizer) process_paths(file, dir string, paths []*ast.Path) map[string][]*ast.Path {
	files_and_innerpaths := map[string][]*ast.Path{}
	for _, path := range paths {
		found_file, filepath, remnant_path := po.find_file(dir, path)
		if !found_file {
			var msg strings.Builder
			line := path.Start.TokenSpan.Line
			col := path.Start.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d: Unable to find any directory or proto file at %s", line,
				col, filepath))
			shared.ReportErrorAndExit("ProjectOrganizer", msg.String())
		}
		path.UseSrcLoc = file
		path.PathSrcLoc = filepath
		path.Pieces = remnant_path.Pieces
		remnant_path.Start = path.Start
		remnant_path.UseSrcLoc = file
		remnant_path.PathSrcLoc = filepath
		if existing, ok := files_and_innerpaths[filepath]; ok {
			if len(remnant_path.Pieces) > 0 {
				files_and_innerpaths[filepath] = append(existing, remnant_path)
			}
		} else {
			if len(remnant_path.Pieces) > 0 {
				files_and_innerpaths[filepath] = []*ast.Path{remnant_path}
			} else {
				files_and_innerpaths[filepath] = []*ast.Path{}
			}
		}
	}
	return files_and_innerpaths
}
