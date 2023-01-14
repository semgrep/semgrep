object Foo {
    //ERROR:
    val List(bad) = List(1)

    //ERROR:
    val List(x,bad) = List(1,2)

    //ERROR:
    val List(bad,x) = List(1,2)

    //ERROR:
    val List(x,bad,x) = List(1,2,3) 
}