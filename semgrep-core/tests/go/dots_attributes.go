package Foo

// ERROR: matching
func foo(int a, int b) {
    x = 42
    return 1
}

// ERROR: matching
func foo(int a, int b) {
    x = 42
    return 1
}