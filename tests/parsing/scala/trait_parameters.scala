// Scala 3.4+ trait parameters

trait Foo(val x: Int)

trait Bar(val x: Int)(val y: String)

class Baz extends Foo(42)
