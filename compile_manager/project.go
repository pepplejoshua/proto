package compilemanager

import (
	"fmt"
	"os"
	"path/filepath"
	"proto/ast"
	"proto/parser"
	"proto/shared"
	"strings"
)

type Project struct {
	Files []string
}

func (p *Project) AddFile(file string) {
	p.Files = append(p.Files, file)
}

type ProjectOrganizer struct {
	startfile string
	dir       string
	project   *Project
}

func NewProjectManager(file string) *ProjectOrganizer {
	abs, _ := filepath.Abs(file)
	return &ProjectOrganizer{
		startfile: file,
		dir:       filepath.Dir(abs),
		project:   &Project{Files: []string{}},
	}
}

func (po *ProjectOrganizer) BuildProject() *Project {
	// add the source file that was used to start the compilation of the project
	po.project.AddFile(po.startfile)

	start_ast := parser.Parse(po.startfile, true)
	imports := start_ast.Imports

	collected_files_with_paths := map[string][]*ast.Path{}
	for _, i := range imports {
		files_and_remnants := po.process_paths(po.dir, i.Paths)

		for file, res_paths := range files_and_remnants {
			if paths, ok := collected_files_with_paths[file]; ok {
				// println(file)
				for _, r_p := range res_paths {
					// println("\t" + r_p.String())
					duplicate := false
					for _, p := range paths {
						// println("\t\t" + p.String())
						if r_p.String() == p.String() {
							duplicate = true
							break
						}
					}
					if !duplicate {
						paths = append(paths, r_p)
						collected_files_with_paths[file] = paths
					}
				}
			} else {
				collected_files_with_paths[file] = res_paths
			}
		}
	}

	for file, paths := range collected_files_with_paths {
		println(fmt.Sprintf("%s:", file))
		for _, p := range paths {
			println(fmt.Sprintf("\t%s", p.String()))
		}
		if len(paths) > 0 {
			println()

		}
	}

	return po.project
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
			} else {
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
			}
		case *ast.UseAs:
			return false, main_path, path
		}
	}
	return true, file_path, &ast.Path{
		Start:  path.Start,
		Pieces: pieces,
	}
}

func (po *ProjectOrganizer) process_paths(dir string, paths []*ast.Path) map[string][]*ast.Path {
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
