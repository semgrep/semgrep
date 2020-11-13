package main

import (
	"fmt"
	"log"
	"net/http"

	// OK
	_ "net/http/pprof"
)

func main() {
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Hello World!")
	})
	log.Fatal(http.ListenAndServe("localhost:8080", nil))
}
