class ExampleClass(val x: TypeName) {
  // ERROR: match
  val res = x.foo

  def test(val y: TypeName) : Unit =
    // ERROR: match
    y.foo
}