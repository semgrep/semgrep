object Foo {
    //ERROR:
    val x = (a + b - foo()) == (a + b - foo())
}