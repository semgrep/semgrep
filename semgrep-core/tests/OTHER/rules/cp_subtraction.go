package main

import (
	"crypto/rand"
	"crypto/rsa"
	"fmt"
)

func main() {
	//Generate Private Key
	// ruleid: use-of-weak-rsa-key
	pvk, err := rsa.GenerateKey(rand.Reader, 1024)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(pvk)

	value := 2048 - 1
	value = value - 100
	// ruleid: use-of-weak-rsa-key
	pvk, err := rsa.GenerateKey(rand.Reader, value)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(pvk)

	// ok: use-of-weak-rsa-key
	pvk, err := rsa.GenerateKey(rand.Reader, 2048)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Println(pvk)
}

