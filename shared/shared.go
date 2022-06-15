package shared

import (
	"io/ioutil"
	"log"
	"os"
)

func ReadFile(path string) string {
	src, err := ioutil.ReadFile(path)
	if err != nil {
		log.Fatal(err)
	}
	return string(src)
}

func ReportError(component, msg string) {
	log.Printf("\u001b[31;1m%s Error:\u001b[0m %s", component, msg)
}

func ReportErrorAndExit(component string, msg string) {
	log.Printf("\u001b[31;1m%s Error:\u001b[0m %s", component, msg)
	os.Exit(0)
}
