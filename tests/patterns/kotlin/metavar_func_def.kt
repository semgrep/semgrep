class Foo {
    // ERROR:
    fun foo(): Void {
        foo(1,2)
    }

    // ERROR:
    public fun bar(bar1: Int, bar2: Int, bar3: Int): Void {
        foo(1,2,3)
        bar(1,2)
    }

    // ERROR:
    public fun foobar(bar1: Int): String {
        return "hello"
    }
}
