package Foo

// ERROR:
func foo() {
    foo (1,2)
}

// ERROR:
func bar(bar1, bar2, bar3) {
    return 1
}

// ERROR:
func foobar(bar1) int {
    bar(1, 2, 3)
    foo()
    return 2
}
