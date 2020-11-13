package main

import "fmt"

// ruleid: hidden-goroutine
func HiddenGoroutine() {
    go func() {
        fmt.Println("hello world")
    }()
}

// ok
func FunctionThatCallsGoroutineIsOk() {
    fmt.Println("This is normal")
    go func() {
        fmt.Println("This is OK because the function does other things")
    }()
}

// ok
func FunctionThatCallsGoroutineAlsoOk() {
    go func() {
        fmt.Println("This is OK because the function does other things")
    }()
    fmt.Println("This is normal")
}
