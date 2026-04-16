// Scala 3.4+ trait parameters

trait Foo(val x: Int)

trait Bar(val x: Int)(val y: String) {
  def sum: String = s"$x $y"
}

class Baz extends Foo(42)
