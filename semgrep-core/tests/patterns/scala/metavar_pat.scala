object Foo {
    //ERROR:
    val x = 0 match { case x => 1 }
}