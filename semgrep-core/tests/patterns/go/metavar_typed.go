package Foo

func main() {
    var x string
    var y string
    var a int
    var b int
    //ERROR:
    if x == y {
       x = y
    }
    if a == b {
       a = b
    }
}
