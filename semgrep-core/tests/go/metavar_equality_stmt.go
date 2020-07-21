package main

import "fmt"

func main() {
    //ERROR:
    if (x > 2) {
        fmt.Println("hello world")
        fmt.Println("goodbye world")
    } else {
        fmt.Println("hello world")
        fmt.Println("goodbye world")
    }

    if (x > 2) {
        fmt.Println("hello world")
        fmt.Println("goodbye world")
    } else {
        fmt.Println("hello world")
    }
}
