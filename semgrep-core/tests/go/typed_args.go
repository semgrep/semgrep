package Foo

func bar() {
    var x int = 1
    var y, z int = 2, 4
    var d bool = true

    foo(1, d, 2)

    //ERROR:
    foo(x, d)

    //ERROR:
    foo (y, d)

    foo (d, d)
}


