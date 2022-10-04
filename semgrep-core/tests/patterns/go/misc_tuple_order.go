package foo

func foo() {

    if a, b := bar(); b {
        fmt.Println("hello")
    }

    //ERROR: match
    if a, b := bar(); a {
        fmt.Println("hello")
    }

}
