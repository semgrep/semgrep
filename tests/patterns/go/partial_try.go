func foo() {
	// MATCH:
    defer func() {
        if r := recover(); r != nil {
            fmt.Println("handled") // code to handle the panic
        }
    }()

    var x int
    x = 10 / 0 // division by zero
}