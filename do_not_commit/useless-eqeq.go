package main
import "fmt"

func main() {
    fmt.Println("hello world")
    var y = 1;
    // ruleid:eqeq-is-bad
    fmt.Println(y == y)
    // ok
    assert(y == y)

    // ruleid:hardcoded-eq-true-or-false
    if (false) {
        fmt.Println("never")
    }
}
