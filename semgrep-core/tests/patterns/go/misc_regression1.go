package main

import (
    "fmt"
    "sync"
)

func ReadMessage() {
    messages := make(chan string)

    go func() {
        messages <- "ping"
    }()

    // ok
    msg := <-messages
    fmt.Println(msg)
}

func ReadMessageMutex() {
    var mutex = &sync.Mutex{}
    messages := make(chan string)

    go func() {
        messages <- "ping"
    }()

    //ERROR: match
    mutex.Lock()
    msg := <-messages
    mutex.Unlock()
    fmt.Println(msg)
}
