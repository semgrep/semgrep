//Test that we can parse a plain block of code not contained in an Object or Class
import stuff.{A,B,C}

def f (x : Int) : Int = x

val y : String = "abc"

object Foo {
    val z = 0
}
