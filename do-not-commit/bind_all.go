package main

import (
	"log"
	"net"
)

func bind_all() {
	// ruleid: avoid-bind-to-all-interfaces
	l, err := net.Listen("tcp", "0.0.0.0:2000")
	if err != nil {
		log.Fatal(err)
	}
	defer l.Close()
}

func bind_default() {
	// ruleid: avoid-bind-to-all-interfaces
	l, err := net.Listen("tcp", ":2000")
	if err != nil {
		log.Fatal(err)
	}
	defer l.Close()
}

func main() {
	// ok
	l, err := net.Listen("tcp", "192.168.1.101:2000")
	if err != nil {
		log.Fatal(err)
	}
	defer l.Close()
}
