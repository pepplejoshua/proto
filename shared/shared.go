package shared

import (
	"log"
	"os"
	"path/filepath"
	"strings"
)

type ErrorHandler struct {
	File  string
	Lines []string
}

func NewErrorHandler(file string) *ErrorHandler {
	return &ErrorHandler{
		File:  file,
		Lines: []string{},
	}
}

func (h *ErrorHandler) ReportError(component string, line int, msg string) {
	// line := h.Lines[span.Line]
	// tag := fmt.Sprintf("\u001b[31;1m%s Error:\u001b[0m:", component)

	var output strings.Builder
	output.WriteString("")
}

func Make_src_path(path string, is_cpp bool) string {
	abs_path := Get_abs_path("CompileManager", path)
	base := Strip_base(abs_path)
	filename := ""
	if is_cpp {
		filename = strings.Split(base, ".")[0] + ".cpp"
	} else {
		filename = strings.Split(base, ".")[0] + ".hpp"
	}

	dir := filepath.Dir(abs_path)
	src_loc := filepath.Join(dir, filename)
	return src_loc
}

func Make_exe_path(path string) string {
	abs_path := Get_abs_path("CompileManager", path)
	base := Strip_base(abs_path)
	filename := strings.Split(base, ".")[0]

	dir := filepath.Dir(abs_path)
	exe_loc := filepath.Join(dir, filename)
	return exe_loc
}

func ReadFile(path string) string {
	src, err := os.ReadFile(path)
	if err != nil {
		log.Fatal(err)
	}
	return string(src)
}

func Get_abs_path(component, path string) string {
	abs_path, err := filepath.Abs(path)
	if err != nil {
		ReportErrorAndExit(component, err.Error())
	}
	return abs_path
}

func Strip_base(abs_path string) string {
	base := filepath.Base(abs_path)
	return base
}

func ReportError(component, msg string) {
	log.Printf("\u001b[31;1m%s Error:\u001b[0m %s", component, msg)
}

func ReportErrorAndExit(component, msg string) {
	log.Printf("\u001b[31;1m%s Error:\n\u001b[0m %s", component, msg)
	os.Exit(0)
}

func ReportErrorWithPathAndExit(component, file, msg string) {
	log.Printf("\u001b[31;1m%s Error:\n\u001b[0m %s %s", component, file, msg)
	os.Exit(0)
}
