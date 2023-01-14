package main

import "fmt"

func foo() (bool, error) { return true, nil}

func bar() (int, bool) { return 0, true}

func qux() {

    var d bool
    var err nil;
    
    //ERROR: match    
    if a, b := bar(); b {
        fmt.Println("hello")
    }

    // shouldn't match this    
    if d, err = foo(); d {
        fmt.Println("hello")
    }

    // neither this
    if b, a := bar(); b {
        fmt.Println("hello")
    }



}
