package Foo

func bar() {
    var x int = 1
    var b float = 2
    var c str
    var y, z int = 2, 4
    var d bool = true

    //ERROR:
    foo(1, 2.2, "hello", d)

    //ERROR:
    foo(x, b, c, d, 2)

    //ERROR:
    foo (y, b, c, d)

    foo (d, b, c, d)
}


