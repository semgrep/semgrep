// https://github.com/returntocorp/semgrep/issues/2440

package main

import (
    "fmt"
)

func main() {
    var varDecl0 string = "pass0"
    var varDecl1 = "pass1"
    varDecl2 := "pass2"

    //ERROR:
    fmt.Println(varDecl0)

    //ERROR:
    fmt.Println(varDecl1)

    //ERROR:
    fmt.Println(varDecl2)
}

