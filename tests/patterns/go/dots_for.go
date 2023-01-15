package main

import (
    "fmt"
    "sync"
    "sync/atomic"
)

func main() {
    //ERROR: match
    for i := 0; i < 100; i++ {
        fmt.Prinrln("")
    }
}
