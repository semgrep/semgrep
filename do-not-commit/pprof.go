package main

import (
	"fmt"
	"log"
	"net/http"

	_ "net/http/pprof"
)

func ok() {
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Hello World!")
	})
    // ok
	log.Fatal(http.ListenAndServe("localhost:8080", nil))
}

func ok2() {
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Hello World!")
	})
    // ok
	log.Fatal(http.ListenAndServe("127.0.0.1:8080", nil))
}

func main() {
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Hello World!")
	})
    // ruleid: pprof-debug-exposure
	log.Fatal(http.ListenAndServe(":8080", nil))
}
