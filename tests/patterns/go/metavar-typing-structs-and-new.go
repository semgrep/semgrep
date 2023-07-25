package main

import "net/http"
import "fmt"

// from gh-6733 
func main() {
    client := &http.Client{}
    //ERROR:
    res := client.Get("http://example.com")
    fmt.Println(res)
}

func main2() {
    client := new(http.Client)
    //ERROR:
    res := client.Get("http://example.com")
    fmt.Println(res)
}
