package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

func main() {
}

func test_chmod() {
	// ruleid: incorrect-default-permission
	err := os.Chmod("/tmp/somefile", 0777)
	if err != nil {
		fmt.Println("Error when changing file permissions!")
		return
	}

	// ok: incorrect-default-permission
	err := os.Chmod("/tmp/somefile", 0400)
	if err != nil {
		fmt.Println("Error when changing file permissions!")
		return
	}
}

func test_mkdir() {
	// ruleid: incorrect-default-permission
	err := os.Mkdir("/tmp/mydir", 0777)
	if err != nil {
		fmt.Println("Error when creating a directory!")
		return
	}

	// ruleid: incorrect-default-permission
	err := os.MkdirAll("/tmp/mydir", 0777)
	if err != nil {
		fmt.Println("Error when creating a directory!")
		return
	}

	// ok: incorrect-default-permission
	err := os.MkdirAll("/tmp/mydir", 0600)
	if err != nil {
		fmt.Println("Error when creating a directory!")
		return
	}
}

func test_openfile() {
	// ruleid: incorrect-default-permission
	_, err := os.OpenFile("/tmp/thing", os.O_CREATE|os.O_WRONLY, 0666)
	if err != nil {
		fmt.Println("Error opening a file!")
		return
	}

	// ok: incorrect-default-permission
	_, err := os.OpenFile("/tmp/thing", os.O_CREATE|os.O_WRONLY, 0600)
	if err != nil {
		fmt.Println("Error opening a file!")
		return
	}
}

func test_writefile() {
	// ruleid: incorrect-default-permission
	err := ioutil.WriteFile("/tmp/demo2", []byte("This is some data"), 0644)
	if err != nil {
		fmt.Println("Error while writing!")
	}
}
