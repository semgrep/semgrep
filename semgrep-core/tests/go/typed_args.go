package Foo

func bar() {
    var x int = 1
    var y, z int = 2, 4
    var d bool = true

    //TODO maybe we want this one to be caught
    foo(1, d)

    //ERROR:
    foo(x, d, 2)

    //ERROR:
    foo (y, d)

    foo (d, d)
}


