// ERROR:
public class Foo {
    val foo: Int = 5

    public fun foo(): Void {
        val foo1: Int = 4
    }
}

// ERROR:
private class Bar: Foo {
    val bar: Int = 3
}

// ERROR:
abstract class Foobar {
    public abstract fun foobar(): Void
}
