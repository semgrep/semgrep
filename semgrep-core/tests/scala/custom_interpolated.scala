object Foo {
    //ERROR: match
    val x = abcd"str"
    //OK:
    val y = s"str"
    //OK:
    val z = x"str"
}