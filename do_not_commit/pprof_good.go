package main

import (
	"fmt"
	"log"
	"net/http"

	// ok
	"net/http/pprof"
)

func main() {
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Hello World!")
	})
	pprof.StartCPUProfile()
	log.Fatal(http.ListenAndServe(":8080", nil))
}
