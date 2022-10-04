object Foo {
    //ERROR:
    val x1 = 0 match { case x => 1 }
    //ERROR:
    val x2 = 0 match { case x => 1 case y => 2}
}
