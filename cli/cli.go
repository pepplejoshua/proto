package cli

import (
	"flag"
	"fmt"
	"path/filepath"
	compilemanager "proto/compile_manager"
	"proto/shared"
	"time"
)

var file = flag.String("file", "", "Path to Proto File to be compiled")
var clean = flag.Bool("c", false, "Clean generated C++ files")

var show_time = flag.Bool("time", false, "Show the runtime of the code found in file.")

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

	man := compilemanager.NewManager(*file, *clean)
	man.Compile()
	if *show_time {
		duration = time.Since(start)
		fmt.Printf("[ran in %s]\n", duration)
	}
}
