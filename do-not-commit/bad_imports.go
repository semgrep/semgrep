package main

import (
	"crypto/cipher"
	"crypto/des"
	"crypto/md5"
	"crypto/rand"
	"crypto/rc4"
	"crypto/sha1"
	"encoding/hex"
	"fmt"
	"io"
	"net/http"
	"net/http/cgi"
	"os"
)

func main1() {
	// ruleid: insecure-module-used
	cgi.Serve(http.FileServer(http.Dir("/usr/share/doc")))
}

func main2() {
	// ruleid: insecure-module-used
	block, err := des.NewCipher([]byte("sekritz"))
	if err != nil {
		panic(err)
	}
	plaintext := []byte("I CAN HAZ SEKRIT MSG PLZ")
	ciphertext := make([]byte, des.BlockSize+len(plaintext))
	iv := ciphertext[:des.BlockSize]
	if _, err := io.ReadFull(rand.Reader, iv); err != nil {
		panic(err)
	}
	stream := cipher.NewCFBEncrypter(block, iv)
	stream.XORKeyStream(ciphertext[des.BlockSize:], plaintext)
	fmt.Println("Secret message is: %s", hex.EncodeToString(ciphertext))
}

func main3() {
	for _, arg := range os.Args {
		// ruleid: insecure-module-used
		fmt.Printf("%x - %s\n", md5.Sum([]byte(arg)), arg)
	}
}

func main4() {
	// ruleid: insecure-module-used
	cipher, err := rc4.NewCipher([]byte("sekritz"))
	if err != nil {
		panic(err)
	}
	plaintext := []byte("I CAN HAZ SEKRIT MSG PLZ")
	ciphertext := make([]byte, len(plaintext))
	cipher.XORKeyStream(ciphertext, plaintext)
	fmt.Println("Secret message is: %s", hex.EncodeToString(ciphertext))
}

func main5() {
	for _, arg := range os.Args {
		// ruleid: insecure-module-used
		fmt.Printf("%x - %s\n", sha1.Sum([]byte(arg)), arg)
	}
}
