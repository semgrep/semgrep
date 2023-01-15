package main

import (
	"crypto/rand"
	"crypto/rsa"
	"fmt"
)

func main() {
	value := 2048 - 1
	value = value - 100
	// ruleid: use-of-weak-rsa-key
	pvk, err := rsa.GenerateKey(rand.Reader, value)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(pvk)
}

