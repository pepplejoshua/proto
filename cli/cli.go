package cli

import (
	"flag"
	"fmt"
	"path/filepath"
	"proto/project"
	"proto/shared"
	"time"
)

var file = flag.String("file", "", "Path to Proto File to be compiled")
var clean = flag.Bool("c", false, "Clean generated C++ files")
var generate_only = flag.Bool("gen", false, "Generate C++ files but don't compile.")
var cpp_flags = flag.String("cpp", "", "Flags that are passed directly to the C++ compiler.")
var show_time = flag.Bool("time", false, "Show the runtime of the code found in file.")
var show_compile_info = flag.Bool("v", false, "Show compile time information.")
var testing = flag.Bool("test", false, "Generate an executable to run tests.")

func Start() {
	flag.Parse()
	if *file == "" {
		shared.ReportErrorAndExit("FileReader", "No file provided")
	}

	if filepath.Ext(*file) != ".pr" {
		shared.ReportErrorAndExit("FileReader", "Provided file: '"+*file+"' is not a proto file. (missing .pr extension).")
	}
	start := time.Now()

	var duration time.Duration

	if *generate_only && *clean {
		shared.ReportWarning("CLI", "-gen and -c are both toggled. -c will clean up generated files.")
	}

	project_org := project.NewProjectManager(*file, *cpp_flags, *clean, *generate_only, *show_compile_info, *testing)
	project_org.BuildProject()
	if *show_time {
		duration = time.Since(start)
		fmt.Printf("[ran in %s]\n", duration)
	}
}
